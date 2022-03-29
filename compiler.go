package main

import (
	"errors"
	"fmt"
	"math"
	"os"
	"strconv"
)

type Precedence int
type ParseFn func(canAssign bool)
type ParseRule struct {
	Prefix     ParseFn
	Infix      ParseFn
	Precedence Precedence
}

const (
	PrecedenceNone       Precedence = iota
	PrecedenceAssigment             // =
	PrecedenceOr                    // or
	PrecedenceAnd                   // and
	PrecedenceEquality              // == !=
	PrecedenceComparison            // < > <= >=
	PrecedenceTerm                  // + -
	PrecedenceFactor                // * /
	PrecedenceUnary                 // ! -
	PrecedenceCall                  // . ()
	PrecedencePrimary
)

type FunctionType int

const (
	FunctionFunction = iota
	FunctionScript
)

type Parser struct {
	hadError    bool
	panicMode   bool
	disassemble bool

	scanner           *Scanner
	current, previous Token
	rules             map[TokenType]ParseRule

	chunk    *Chunk
	compiler *Compiler
}

func NewParser(source string, disassemble bool) *Parser {
	parser := &Parser{
		scanner:     NewScanner(source),
		chunk:       NewChunk(),
		compiler:    newCompiler("", FunctionScript),
		disassemble: disassemble,
	}
	parser.buildParseRuleTable()

	return parser
}

func (parser *Parser) compile() (*Function, bool) {
	parser.advance()
	for !parser.match(EOF) {
		parser.declaration()
	}
	parser.consume(EOF, "Expect end of expression.")

	fn := parser.endCompiler()
	return fn, !parser.hadError
}

func (parser *Parser) advance() {
	parser.previous = parser.current

	for {
		parser.current = parser.scanner.Scan()
		if parser.current.Type != Error {
			break
		}

		parser.errorAtCurrent(parser.current.Lexeme)
	}
}

func (parser *Parser) consume(expected TokenType, msg string) {
	if parser.current.Type == expected {
		parser.advance()
		return
	}

	parser.errorAtCurrent(msg)
}

func (parser *Parser) match(tt TokenType) bool {
	if !parser.check(tt) {
		return false
	}
	parser.advance()
	return true
}

func (parser *Parser) check(tt TokenType) bool {
	return parser.current.Type == tt
}

func (parser *Parser) declaration() {
	switch {
	case parser.match(Class):
		parser.classDeclaration()
	case parser.match(Fun):
		parser.funDeclaration()
	case parser.match(Var):
		parser.varDeclaration()
	default:
		parser.statement()
	}

	if parser.panicMode {
		parser.synchronize()
	}
}

func (parser *Parser) varDeclaration() {
	global := parser.parseVariable("Expect variable name.")

	if parser.match(Equal) {
		parser.expression()
	} else {
		parser.emitByte(OpNil)
	}
	parser.consume(Semicolon, "Expect ';' after variable declaration.")

	parser.defineVariable(global)
}

func (parser *Parser) funDeclaration() {
	global := parser.parseVariable("Expect function name.")
	parser.compiler.markInitialized()
	parser.function(FunctionFunction)
	parser.defineVariable(global)
}

func (parser *Parser) classDeclaration() {
	parser.consume(Identifier, "Expect class name.")
	nameConstant := parser.identifierConstant(parser.previous)
	parser.declareVariable()

	parser.emitBytes(OpClass, nameConstant)
	parser.defineVariable(nameConstant)

	parser.consume(LeftBrace, "Expect '{' before class body.")
	parser.consume(RightBrace, "Expect '}' after class body.")
}

func (parser *Parser) defineVariable(global OpCode) {
	if parser.compiler.ScopeDepth > 0 {
		parser.compiler.markInitialized()
		return
	}

	parser.emitBytes(OpDefineGlobal, global)
}

func (parser *Parser) parseVariable(errMsg string) OpCode {
	parser.consume(Identifier, errMsg)

	parser.declareVariable()

	if parser.compiler.ScopeDepth > 0 {
		return 0
	}

	return parser.identifierConstant(parser.previous)
}

func (parser *Parser) identifierConstant(name Token) OpCode {
	return parser.makeConstant(stringValue(name.Lexeme))
}

func (parser *Parser) declareVariable() {
	if parser.compiler.ScopeDepth == 0 {
		return
	}

	name := parser.previous
	if parser.compiler.isLocalDeclared(name) {
		parser.error("Already a variable with this name in this scope.")
	}

	err := parser.compiler.addLocal(name)
	if err != nil {
		parser.error("Too many local variables in function.")
	}
}

func (parser *Parser) function(ft FunctionType) {
	current := parser.pushCompiler(ft)
	// This beginScope() doesn’t have a corresponding endScope() call.
	// Because we end Compiler completely when we reach the end of
	// the function body, there’s no need to close the lingering outermost scope.
	parser.beginScope()

	parser.consume(LeftParen, "Expect '(' after function name.")
	if !parser.check(RightParen) {
		for {
			current.Function.Arity++
			if current.Function.Arity > 255 {
				parser.errorAtCurrent("Can't have more than 255 parameters.")
			}

			ci := parser.parseVariable("Expect parameter name.")
			parser.defineVariable(ci)

			if !parser.match(Comma) {
				break
			}
		}
	}
	parser.consume(RightParen, "Expect ')' after parameters.")
	parser.consume(LeftBrace, "Expect '{' before function body.")
	parser.block()

	function := parser.endCompiler()
	parser.emitBytes(OpClosure, parser.makeConstant(functionValue(function)))
	for _, upValue := range function.Upvalues {
		if upValue.IsLocal {
			parser.emitByte(1)
		} else {
			parser.emitByte(0)
		}
		parser.emitByte(upValue.Index)
	}
}

func (parser *Parser) pushCompiler(ft FunctionType) *Compiler {
	compiler := newCompiler(parser.previous.Lexeme, ft)
	compiler.Enclosing = parser.compiler
	parser.compiler = compiler

	return compiler
}

func (parser *Parser) statement() {
	switch {
	case parser.match(Print):
		parser.printStatement()
	case parser.match(If):
		parser.ifStatement()
	case parser.match(Return):
		parser.returnStatement()
	case parser.match(LeftBrace):
		parser.beginScope()
		parser.block()
		parser.endScope()
	case parser.match(While):
		parser.whileStatement()
	case parser.match(For):
		parser.forStatement()
	default:
		parser.expressionStatement()
	}
}

func (parser *Parser) beginScope() {
	parser.compiler.ScopeDepth += 1
}

func (parser *Parser) endScope() {
	parser.compiler.ScopeDepth -= 1

	locals := parser.compiler.Locals
	localCount := len(locals)

	for localCount > 0 && locals[localCount-1].Depth > parser.compiler.ScopeDepth {
		if locals[localCount-1].IsCaptured {
			parser.emitByte(OpCloseUpvalue)
		} else {
			parser.emitByte(OpPop)
		}
		localCount -= 1
	}
	parser.compiler.Locals = locals[:localCount]
}

func (parser *Parser) ifStatement() {
	parser.consume(LeftParen, "Expect '(' after 'if'.")
	parser.expression()
	parser.consume(RightParen, "Expect ')' after condition.")

	thenJump := parser.emitJump(OpJumpIfFalse)
	parser.emitByte(OpPop)
	parser.statement()

	elseJump := parser.emitJump(OpJump)
	parser.patchJump(thenJump)

	parser.emitByte(OpPop)
	if parser.match(Else) {
		parser.statement()
	}
	parser.patchJump(elseJump)
}

func (parser *Parser) emitJump(op OpCode) int {
	parser.emitByte(op)
	parser.emitByte(0xff)
	parser.emitByte(0xff)
	return len(parser.currentChunk().codes) - 2
}

func (parser *Parser) patchJump(offset int) {
	// -2 to adjust for the bytecode for the jump offset itself.
	chunk := parser.currentChunk()
	jump := len(chunk.codes) - offset - 2

	if jump > math.MaxUint16 {
		parser.error("Too much code to jump over.")
	}

	chunk.codes[offset] = OpCode((jump >> 8) & 0xff)
	chunk.codes[offset+1] = OpCode(jump & 0xff)
}

func (parser *Parser) whileStatement() {
	loopStart := len(parser.currentChunk().codes)

	parser.consume(LeftParen, "Expect '(' after 'while'.")
	parser.expression()
	parser.consume(RightParen, "Expect ')' after condition.")

	exitJump := parser.emitJump(OpJumpIfFalse)
	parser.emitByte(OpPop)
	parser.statement()

	parser.emitLoop(loopStart)

	parser.patchJump(exitJump)
	parser.emitByte(OpPop)
}

func (parser *Parser) forStatement() {
	parser.beginScope()
	parser.consume(LeftParen, "Expect '(' after 'for'.")

	switch {
	case parser.match(Semicolon):
	case parser.match(Var):
		parser.varDeclaration()
	default:
		parser.expressionStatement()
	}

	loopStart := parser.currentChunk().count()
	exitJump := -1

	if !parser.match(Semicolon) {
		parser.expression()
		parser.consume(Semicolon, "Expect ';' after loop condition.")

		// Jump out of the loop if the condition is false.
		exitJump = parser.emitJump(OpJumpIfFalse)
		parser.emitByte(OpPop) // Condition.
	}

	if !parser.match(RightParen) {
		bodyJump := parser.emitJump(OpJump)
		incrementStart := parser.currentChunk().count()
		parser.expression()
		parser.emitByte(OpPop)
		parser.consume(RightParen, "Expect ')' after for clauses.")

		parser.emitLoop(loopStart)
		loopStart = incrementStart
		parser.patchJump(bodyJump)
	}

	parser.statement()
	parser.emitLoop(loopStart)

	if exitJump != -1 {
		parser.patchJump(exitJump)
		parser.emitByte(OpPop)
	}

	parser.endScope()
}

func (parser *Parser) emitLoop(loopStart int) {
	parser.emitByte(OpLoop)

	offset := parser.currentChunk().count() - loopStart + 2
	if offset > math.MaxUint16 {
		parser.error("Loop body too large.")
	}

	parser.emitByte(OpCode((offset >> 8) & 0xff))
	parser.emitByte(OpCode(offset & 0xff))
}

func (parser *Parser) printStatement() {
	parser.expression()
	parser.consume(Semicolon, "Expect ';' after value.")
	parser.emitByte(OpPrint)
}

func (parser *Parser) returnStatement() {
	if parser.compiler.Type == FunctionScript {
		parser.error("Can't return from top-level code.")
	}

	if parser.match(Semicolon) {
		parser.emitReturn()
	} else {
		parser.expression()
		parser.consume(Semicolon, "Expect ';' after return value.")
		parser.emitByte(OpReturn)
	}
}

func (parser *Parser) expressionStatement() {
	parser.expression()
	parser.consume(Semicolon, "Expect ';' after expression.")
	parser.emitByte(OpPop)
}

func (parser *Parser) expression() {
	parser.parsePrecedence(PrecedenceAssigment)
}

// starts at the current token and parses any expression at the given Precedence level or higher.
func (parser *Parser) parsePrecedence(precedence Precedence) {
	parser.advance()

	parsePrefix := parser.getRule(parser.previous.Type).Prefix
	if parsePrefix == nil {
		parser.error("Expect expression.")
		return
	}
	canAssign := precedence <= PrecedenceAssigment
	parsePrefix(canAssign)

	for precedence <= parser.getRule(parser.current.Type).Precedence {
		parser.advance()
		parseInfix := parser.getRule(parser.previous.Type).Infix
		parseInfix(canAssign)
	}

	if canAssign && parser.match(Equal) {
		parser.error("Invalid assignment target.")
	}
}

func (parser *Parser) block() {
	for !parser.check(RightBrace) && !parser.check(EOF) {
		parser.declaration()
	}

	parser.consume(RightBrace, "Expect '}' after block.")
}

func (parser *Parser) string(canAssign bool) {
	length := len(parser.previous.Lexeme)
	parser.emitConstant(stringValue(parser.previous.Lexeme[1 : length-1]))
}

func (parser *Parser) number(canAssign bool) {
	value, _ := strconv.ParseFloat(parser.previous.Lexeme, 64)
	parser.emitConstant(numberValue(value))
}

func (parser *Parser) group(canAssign bool) {
	parser.expression()
	parser.consume(RightParen, "Expect ')' after expression.")
}

func (parser *Parser) unary(canAssign bool) {
	operatorType := parser.previous.Type

	parser.parsePrecedence(PrecedenceUnary)

	// Emit the operator instruction.
	switch operatorType {
	case Bang:
		parser.emitByte(OpNot)
	case Minus:
		parser.emitByte(OpNegate)
	default:
		return // Unreachable.
	}
}

func (parser *Parser) binary(canAssign bool) {
	operatorType := parser.previous.Type
	rule := parser.getRule(operatorType)
	parser.parsePrecedence(rule.Precedence + 1)

	switch operatorType {
	case BangEqual:
		parser.emitBytes(OpEqual, OpNot)
	case EqualEqual:
		parser.emitByte(OpEqual)
	case Greater:
		parser.emitByte(OpGreater)
	case GreaterEqual:
		parser.emitBytes(OpLess, OpNot)
	case Less:
		parser.emitByte(OpLess)
	case LessEqual:
		parser.emitBytes(OpGreater, OpNot)
	case Plus:
		parser.emitByte(OpAdd)
	case Minus:
		parser.emitByte(OpSubtract)
	case Star:
		parser.emitByte(OpMultiply)
	case Slash:
		parser.emitByte(OpDivide)
	default:
	}
}

func (parser *Parser) literal(canAssign bool) {
	switch parser.previous.Type {
	case False:
		parser.emitByte(OpFalse)
	case Nil:
		parser.emitByte(OpNil)
	case True:
		parser.emitByte(OpTrue)
	default:
	}
}

func (parser *Parser) variable(canAssign bool) {
	parser.namedVariable(parser.previous, canAssign)
}

func (parser *Parser) namedVariable(name Token, canAssign bool) {
	var getOp, setOp OpCode

	arg, found, err := parser.compiler.resolveLocal(name)
	if err != nil {
		parser.error(err.Error())
		return
	}

	if found {
		getOp = OpGetLocal
		setOp = OpSetLocal
	} else if arg, found, err = parser.compiler.resolveUpValue(name); err != nil {
		parser.error(err.Error())
		return
	} else if found {
		getOp = OpGetUpValue
		setOp = OpSetUpValue
	} else {
		arg = parser.identifierConstant(name)
		getOp = OpGetGlobal
		setOp = OpSetGlobal
	}

	if canAssign && parser.match(Equal) {
		parser.expression()
		parser.emitBytes(setOp, arg)
	} else {
		parser.emitBytes(getOp, arg)
	}
}

func (parser *Parser) and(canAssign bool) {
	endJump := parser.emitJump(OpJumpIfFalse)

	parser.emitByte(OpPop)
	parser.parsePrecedence(PrecedenceAnd)

	parser.patchJump(endJump)
}

func (parser *Parser) or(canAssign bool) {
	elseJump := parser.emitJump(OpJumpIfFalse)
	endJump := parser.emitJump(OpJump)

	parser.patchJump(elseJump)

	parser.emitByte(OpPop)
	parser.parsePrecedence(PrecedenceOr)

	parser.patchJump(endJump)
}

func (parser *Parser) call(canAssign bool) {
	argCount := parser.argumentList()
	parser.emitBytes(OpCall, OpCode(argCount))
}

func (parser *Parser) dot(canAssign bool) {
	parser.consume(Identifier, "Expect property name after '.'.")
	name := parser.identifierConstant(parser.previous)

	if canAssign && parser.match(Equal) {
		parser.expression()
		parser.emitBytes(OpSetProperty, name)
	} else {
		parser.emitBytes(OpGetProperty, name)
	}
}

func (parser *Parser) argumentList() uint {
	var argCount uint = 0
	if !parser.check(RightParen) {
		for {
			parser.expression()
			argCount += 1

			if argCount == 255 {
				parser.error("Can't have more than 255 arguments.")
			}
			if !parser.match(Comma) {
				break
			}
		}
	}
	parser.consume(RightParen, "Expect ')' after arguments.")
	return argCount
}

func (parser *Parser) synchronize() {
	parser.panicMode = false

	for parser.current.Type != EOF {
		if parser.previous.Type == Semicolon {
			return
		}

		switch parser.current.Type {
		case Class, Fun, Var, For, If, While, Print, Return:
			return
		default:
		}

		parser.advance()
	}
}

func (parser *Parser) endCompiler() *Function {
	parser.emitByte(OpNil)
	parser.emitReturn()

	function := parser.compiler.Function
	parser.compiler = parser.compiler.Enclosing

	if parser.disassemble {
		name := "<script>"
		if function.Name != "" {
			name = function.Name
		}
		function.Chunk.DisassembleChunk(name)
	}

	return function
}

func (parser *Parser) errorAtCurrent(msg string) {
	parser.errorAt(parser.current, msg)
}

func (parser *Parser) error(msg string) {
	parser.errorAt(parser.previous, msg)
}

func (parser *Parser) errorAt(token Token, msg string) {
	if parser.panicMode {
		return
	}
	parser.panicMode = true

	fmt.Fprintf(os.Stderr, "[line %d] Error", token.Line)

	if token.Type == EOF {
		fmt.Fprintf(os.Stderr, " at end")
	} else if token.Type == Error {
		// Nothing.
	} else {
		fmt.Fprintf(os.Stderr, " at '%s'", token.Lexeme)
	}

	fmt.Fprintf(os.Stderr, ": %s\n", msg)
	parser.hadError = true
}

func (parser *Parser) currentChunk() *Chunk {
	return parser.compiler.Function.Chunk
}

func (parser *Parser) emitReturn() {
	parser.emitByte(OpReturn)
}

func (parser *Parser) emitByte(op OpCode) {
	parser.currentChunk().Write(op, parser.previous.Line)
}

func (parser *Parser) emitBytes(op1, op2 OpCode) {
	parser.emitByte(op1)
	parser.emitByte(op2)
}

func (parser *Parser) emitConstant(value Value) {
	parser.emitBytes(OpConstant, parser.makeConstant(value))
}

func (parser *Parser) makeConstant(value Value) OpCode {
	ci := parser.currentChunk().AddConstant(value)
	if ci > math.MaxUint8 {
		parser.error("Too many constants in one chunk.")
		return 0
	}

	return OpCode(ci)
}

func (parser *Parser) getRule(tt TokenType) ParseRule {
	return parser.rules[tt]
}

func (parser *Parser) buildParseRuleTable() {
	parser.rules = map[TokenType]ParseRule{
		LeftParen:    {Prefix: parser.group, Infix: parser.call, Precedence: PrecedenceCall},
		RightParen:   {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		LeftBrace:    {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		RightBrace:   {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Comma:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Dot:          {Prefix: nil, Infix: parser.dot, Precedence: PrecedenceCall},
		Minus:        {Prefix: parser.unary, Infix: parser.binary, Precedence: PrecedenceTerm},
		Plus:         {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceTerm},
		Semicolon:    {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Slash:        {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceFactor},
		Star:         {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceFactor},
		Bang:         {Prefix: parser.unary, Infix: nil, Precedence: PrecedenceNone},
		BangEqual:    {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceNone},
		Equal:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		EqualEqual:   {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceEquality},
		Greater:      {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceComparison},
		GreaterEqual: {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceComparison},
		Less:         {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceComparison},
		LessEqual:    {Prefix: nil, Infix: parser.binary, Precedence: PrecedenceComparison},
		Identifier:   {Prefix: parser.variable, Infix: nil, Precedence: PrecedenceNone},
		String:       {Prefix: parser.string, Infix: nil, Precedence: PrecedenceNone},
		Number:       {Prefix: parser.number, Infix: nil, Precedence: PrecedenceNone},
		And:          {Prefix: nil, Infix: parser.and, Precedence: PrecedenceAnd},
		Class:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Else:         {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		False:        {Prefix: parser.literal, Infix: nil, Precedence: PrecedenceNone},
		For:          {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Fun:          {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		If:           {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Nil:          {Prefix: parser.literal, Infix: nil, Precedence: PrecedenceNone},
		Or:           {Prefix: nil, Infix: parser.or, Precedence: PrecedenceOr},
		Print:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Return:       {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Super:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		This:         {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		True:         {Prefix: parser.literal, Infix: nil, Precedence: PrecedenceNone},
		Var:          {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		While:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Error:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		EOF:          {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
	}
}

type Local struct {
	Name       Token
	Depth      int
	IsCaptured bool
}

type Compiler struct {
	Locals     []Local
	Function   *Function
	Type       FunctionType
	ScopeDepth int
	Enclosing  *Compiler
}

func newCompiler(functionName string, ft FunctionType) *Compiler {
	compiler := &Compiler{
		Locals:     make([]Local, 0, math.MaxUint8),
		ScopeDepth: 0,
		Type:       ft,
		Function:   newFunction(functionName),
	}

	compiler.Locals = append(compiler.Locals, Local{
		Name:  Token{Type: Error, Lexeme: "", Line: 0},
		Depth: 0,
	})

	return compiler
}

func (compiler *Compiler) markInitialized() {
	if compiler.ScopeDepth == 0 {
		return
	}

	compiler.Locals[len(compiler.Locals)-1].Depth = compiler.ScopeDepth
}

func (compiler *Compiler) isLocalDeclared(name Token) bool {
	for i := len(compiler.Locals) - 1; i >= 0; i-- {
		local := compiler.Locals[i]
		if local.Depth != -1 && local.Depth < compiler.ScopeDepth {
			break
		}

		if name.Lexeme == local.Name.Lexeme {
			return true
		}
	}

	return false
}

func (compiler *Compiler) addLocal(name Token) error {
	if len(compiler.Locals) > math.MaxUint8 {
		return errors.New("Too many local variables in function.")
	}

	compiler.Locals = append(compiler.Locals, Local{
		Name:       name,
		Depth:      -1,
		IsCaptured: false,
	})

	return nil
}

func (compiler *Compiler) resolveLocal(name Token) (uint, bool, error) {
	for i := len(compiler.Locals) - 1; i >= 0; i-- {
		local := compiler.Locals[i]
		if name.Lexeme == local.Name.Lexeme {
			if local.Depth == -1 {
				return 0, false, errors.New("can't read local variable in its own initializer")
			}
			return uint(i), true, nil
		}
	}

	return 0, false, nil
}

func (compiler *Compiler) resolveUpValue(name Token) (uint, bool, error) {
	if compiler.Enclosing == nil {
		return 0, false, nil
	}

	index, found, err := compiler.Enclosing.resolveLocal(name)
	if found {
		compiler.Enclosing.Locals[index].IsCaptured = true

		index, err = compiler.addUpValue(index, true)
		return index, true, err
	}

	index, found, err = compiler.Enclosing.resolveUpValue(name)
	if found {
		index, err = compiler.addUpValue(index, false)
		return index, true, err
	}

	return 0, false, nil
}

func (compiler *Compiler) addUpValue(index uint, isLocal bool) (uint, error) {
	count := len(compiler.Function.Upvalues)
	if count >= math.MaxUint8 {
		return 0, errors.New("too many closure variables in function")
	}

	for i, upValue := range compiler.Function.Upvalues {
		if upValue.Index == index && upValue.IsLocal == isLocal {
			return uint(i), nil
		}
	}

	compiler.Function.Upvalues = append(compiler.Function.Upvalues, Upvalue{
		Index:   index,
		IsLocal: isLocal,
	})

	return uint(count), nil
}

package main

import (
	"fmt"
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

const MaxUint8 = 256

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

type Parser struct {
	disassemble       bool
	scanner           *Scanner
	current, previous Token
	hadError          bool
	panicMode         bool
	chunk             *Chunk
	rules             map[TokenType]ParseRule
}

func NewParser(source string, disassemble bool) *Parser {
	parser := &Parser{
		scanner:     NewScanner(source),
		chunk:       NewChunk(),
		disassemble: disassemble,
	}
	parser.buildParseRuleTable()

	return parser
}

func (parser *Parser) compile() (*Chunk, bool) {
	parser.advance()
	for !parser.match(EOF) {
		parser.declaration()
	}
	parser.consume(EOF, "Expect end of expression.")
	parser.endCompiler()
	return parser.chunk, !parser.hadError
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
	if parser.match(Var) {
		parser.varDeclaration()
	} else {
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
		parser.emitOp(OpNil)
	}
	parser.consume(Semicolon, "Expect ';' after variable declaration.")

	parser.defineVariable(global)
}

func (parser *Parser) parseVariable(errMsg string) OpCode {
	parser.consume(Identifier, errMsg)
	return parser.identifierConstant(parser.previous)
}

func (parser *Parser) identifierConstant(name Token) OpCode {
	return parser.makeConstant(stringValue(name.Lexeme))
}

func (parser *Parser) statement() {
	if parser.match(Print) {
		parser.printStatement()
	} else {
		parser.expressionStatement()
	}
}

func (parser *Parser) printStatement() {
	parser.expression()
	parser.consume(Semicolon, "Expect ';' after value.")
	parser.emitOp(OpPrint)
}

func (parser *Parser) expressionStatement() {
	parser.expression()
	parser.consume(Semicolon, "Expect ';' after expression.")
	parser.emitOp(OpPop)
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

func (parser *Parser) string(canAssign bool) {
	length := len(parser.previous.Lexeme)
	parser.emitConstant(stringValue(parser.previous.Lexeme[1 : length-1]))
}

func (parser *Parser) number(canAssign bool) {
	value, _ := strconv.ParseFloat(parser.previous.Lexeme, 64)
	parser.emitConstant(numberValue(value))
}

func (parser *Parser) defineVariable(global OpCode) {
	parser.emitOps(OpDefineGlobal, global)
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
		parser.emitOp(OpNot)
	case Minus:
		parser.emitOp(OpNegate)
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
		parser.emitOps(OpEqual, OpNot)
	case EqualEqual:
		parser.emitOp(OpEqual)
	case Greater:
		parser.emitOp(OpGreater)
	case GreaterEqual:
		parser.emitOps(OpLess, OpNot)
	case Less:
		parser.emitOp(OpLess)
	case LessEqual:
		parser.emitOps(OpGreater, OpNot)
	case Plus:
		parser.emitOp(OpAdd)
	case Minus:
		parser.emitOp(OpSubtract)
	case Star:
		parser.emitOp(OpMultiply)
	case Slash:
		parser.emitOp(OpDivide)
	default:
	}
}

func (parser *Parser) literal(canAssign bool) {
	switch parser.previous.Type {
	case False:
		parser.emitOp(OpFalse)
	case Nil:
		parser.emitOp(OpNil)
	case True:
		parser.emitOp(OpTrue)
	default:
	}
}

func (parser *Parser) variable(canAssign bool) {
	parser.namedVariable(parser.previous, canAssign)
}

func (parser *Parser) namedVariable(name Token, canAssign bool) {
	if canAssign && parser.match(Equal) {
		parser.expression()
		parser.emitOps(OpSetGlobal, parser.identifierConstant(name))
	} else {
		parser.emitOps(OpGetGlobal, parser.identifierConstant(name))
	}
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

func (parser *Parser) endCompiler() {
	parser.emitReturn()
	if parser.disassemble {
		parser.currentChunk().DisassembleChunk("code")
	}
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
	return parser.chunk
}

func (parser *Parser) emitReturn() {
	parser.emitOp(OpReturn)
}

func (parser *Parser) emitOp(op OpCode) {
	parser.currentChunk().Write(op, parser.previous.Line)
}

func (parser *Parser) emitOps(op1, op2 OpCode) {
	parser.emitOp(op1)
	parser.emitOp(op2)
}

func (parser *Parser) emitConstant(value Value) {
	parser.emitOps(OpConstant, parser.makeConstant(value))
}

func (parser *Parser) makeConstant(value Value) OpCode {
	ci := parser.currentChunk().AddConstant(value)
	if ci > MaxUint8 {
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
		LeftParen:    {Prefix: parser.group, Infix: nil, Precedence: PrecedenceNone},
		RightParen:   {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		LeftBrace:    {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		RightBrace:   {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Comma:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
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
		And:          {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Class:        {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Else:         {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		False:        {Prefix: parser.literal, Infix: nil, Precedence: PrecedenceNone},
		For:          {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Fun:          {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		If:           {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
		Nil:          {Prefix: parser.literal, Infix: nil, Precedence: PrecedenceNone},
		Or:           {Prefix: nil, Infix: nil, Precedence: PrecedenceNone},
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

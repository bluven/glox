package main

type TokenType int

const (
	// Single-character tokens.
	LeftParen TokenType = iota
	RightParen
	LeftBrace
	RightBrace
	Comma
	Dot
	Minus
	Plus
	Semicolon
	Slash
	Star

	// One or two character tokens.
	Bang
	BangEqual
	Equal
	EqualEqual
	Greater
	GreaterEqual
	Less
	LessEqual

	// Literals.
	Identifier
	String
	Number

	// Keywords.
	And
	Class
	Else
	False
	For
	Fun
	If
	Nil
	Or
	Print
	Return
	Super
	This
	True
	Var
	While

	Error
	EOF
)

type Token struct {
	Type   TokenType
	Lexeme string
	Line   int
}

func (t Token) String() string {
	return t.Lexeme
}

type Scanner struct {
	code                 string
	start, current, line int
}

func NewScanner(code string) *Scanner {
	return &Scanner{code: code, line: 1}
}

func (scanner *Scanner) Scan() Token {
	scanner.skipWhitespace()
	scanner.start = scanner.current

	if scanner.isAtEnd() {
		return scanner.makeToken(EOF)
	}

	c := scanner.advance()

	if scanner.isDigit(c) {
		return scanner.number()
	}

	if scanner.isAlpha(c) {
		return scanner.identifier()
	}

	switch c {
	case '(':
		return scanner.makeToken(LeftParen)
	case ')':
		return scanner.makeToken(RightParen)
	case '{':
		return scanner.makeToken(LeftBrace)
	case '}':
		return scanner.makeToken(RightBrace)
	case ';':
		return scanner.makeToken(Semicolon)
	case ',':
		return scanner.makeToken(Comma)
	case '.':
		return scanner.makeToken(Dot)
	case '-':
		return scanner.makeToken(Minus)
	case '+':
		return scanner.makeToken(Plus)
	case '/':
		return scanner.makeToken(Slash)
	case '*':
		return scanner.makeToken(Star)
	case '!':
		return scanner.matchAndMakeToken('=', BangEqual, Bang)
	case '=':
		return scanner.matchAndMakeToken('=', EqualEqual, Equal)
	case '<':
		return scanner.matchAndMakeToken('=', LessEqual, Less)
	case '>':
		return scanner.matchAndMakeToken('=', GreaterEqual, Greater)
	case '"':
		return scanner.string()
	default:
		return scanner.errorToken("Unexpected character.")
	}
}

func (scanner *Scanner) isAtEnd() bool {
	return scanner.current == len(scanner.code)
}

func (scanner *Scanner) makeToken(tt TokenType) Token {
	return Token{
		Type:   tt,
		Lexeme: scanner.code[scanner.start:scanner.current],
		Line:   scanner.line,
	}
}

func (scanner *Scanner) matchAndMakeToken(expected byte, t1, t2 TokenType) Token {
	tt := t1
	if scanner.match(expected) {
		tt = t2
	}

	return Token{
		Type:   tt,
		Lexeme: scanner.code[scanner.start:scanner.current],
		Line:   scanner.line,
	}
}

func (scanner *Scanner) errorToken(msg string) Token {
	return Token{
		Type:   Error,
		Lexeme: msg,
		Line:   scanner.line,
	}
}

func (scanner *Scanner) match(expected byte) bool {
	if scanner.isAtEnd() {
		return false
	}

	if scanner.code[scanner.current] != expected {
		return false
	}

	scanner.current++
	return true
}

func (scanner *Scanner) advance() byte {
	scanner.current++
	return scanner.code[scanner.current-1]
}

func (scanner *Scanner) skipWhitespace() {
	for !scanner.isAtEnd() {
		switch scanner.peek() {
		case ' ', '\r', '\t':
			scanner.advance()
		case '\n':
			scanner.line += 1
			scanner.advance()
		case '/':
			if scanner.peekNext() == '/' {
				for !scanner.isAtEnd() && scanner.peek() != '\n' {
					scanner.advance()
				}
			}
		default:
			return
		}
	}
}

func (scanner *Scanner) peek() byte {
	return scanner.code[scanner.current]
}

func (scanner *Scanner) peekNext() byte {
	if scanner.current+1 < len(scanner.code) {
		return scanner.code[scanner.current+1]
	} else {
		return 0
	}
}

func (scanner *Scanner) string() Token {
	for scanner.peek() != '"' && !scanner.isAtEnd() {
		if scanner.peek() == '\n' {
			scanner.line++
		}
		scanner.advance()
	}

	if scanner.isAtEnd() {
		return scanner.errorToken("Unterminated string.")
	}

	scanner.advance()
	return scanner.makeToken(String)
}

func (scanner *Scanner) isDigit(b byte) bool {
	return b >= '0' && b <= '9'
}

func (scanner *Scanner) number() Token {
	for scanner.isDigit(scanner.peek()) {
		scanner.advance()
	}

	// Look for a fractional part.
	if scanner.peek() == '.' && scanner.isDigit(scanner.peekNext()) {
		scanner.advance()

		for scanner.isDigit(scanner.peek()) {
			scanner.advance()
		}
	}

	return scanner.makeToken(Number)
}

func (scanner *Scanner) isAlpha(b byte) bool {
	return (b >= 'a' && b <= 'z') ||
		(b >= 'A' && b <= 'Z') ||
		b == '_'
}

func (scanner *Scanner) identifier() Token {
	for scanner.isAlpha(scanner.peek()) || scanner.isDigit(scanner.peek()) {
		scanner.advance()
	}

	return scanner.makeToken(scanner.identifierType())
}

func (scanner *Scanner) identifierType() TokenType {
	switch scanner.code[scanner.start] {
	case 'a':
		return scanner.checkKeyword(1, "nd", And)
	case 'c':
		return scanner.checkKeyword(1, "lass", Class)
	case 'e':
		return scanner.checkKeyword(1, "lse", Else)
	case 'f':
		if scanner.current-scanner.start > 1 {
			switch scanner.code[scanner.start+1] {
			case 'a':
				return scanner.checkKeyword(2, "lse", False)
			case 'o':
				return scanner.checkKeyword(2, "r", For)
			case 'u':
				return scanner.checkKeyword(2, "n", Fun)
			}
		}
	case 'i':
		return scanner.checkKeyword(1, "f", If)
	case 'n':
		return scanner.checkKeyword(1, "il", Nil)
	case 'o':
		return scanner.checkKeyword(1, "r", Or)
	case 'p':
		return scanner.checkKeyword(1, "rint", Print)
	case 'r':
		return scanner.checkKeyword(1, "eturn", Return)
	case 's':
		return scanner.checkKeyword(1, "uper", Super)
	case 'v':
		return scanner.checkKeyword(1, "ar", Var)
	case 'w':
		return scanner.checkKeyword(1, "hile", While)
	case 't':
		if scanner.current-scanner.start > 1 {
			switch scanner.code[scanner.start+1] {
			case 'h':
				return scanner.checkKeyword(2, "is", This)
			case 'r':
				return scanner.checkKeyword(2, "ue", True)
			}
		}
	}
	return Identifier
}

func (scanner *Scanner) checkKeyword(start int, rest string, tt TokenType) TokenType {
	if scanner.code[scanner.start+start:scanner.current] == rest {
		return tt
	}

	return Identifier
}

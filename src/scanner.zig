const std = @import("std");

const Keyword = struct {
    token_type: TokenType,
    name: []const u8,
};

fn keyword(token_type: TokenType, name: []const u8) Keyword {
    return Keyword{
        .token_type = token_type,
        .name = name,
    };
}

pub const keywords = [_]Keyword{
    keyword(.And, "and"),
    keyword(.Break, "break"),
    keyword(.Class, "class"),
    keyword(.Continue, "continue"),
    keyword(.Const, "const"),
    keyword(.Else, "else"),
    keyword(.False, "false"),
    keyword(.Fn, "fn"),
    keyword(.For, "for"),
    keyword(.If, "if"),
    keyword(.Nil, "nil"),
    keyword(.Or, "or"),
    keyword(.Print, "print"),
    keyword(.Return, "return"),
    keyword(.Static, "static"),
    keyword(.Super, "super"),
    keyword(.This, "this"),
    keyword(.True, "true"),
    keyword(.Var, "var"),
    keyword(.With, "with"),
    keyword(.While, "while"),
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    line: u32,

    pub fn create(token_type: TokenType, lexeme: []const u8, line: u32) Token {
        return Token{
            .token_type = token_type,
            .lexeme = lexeme,
            .line = line,
        };
    }

    pub fn symbol(name: []const u8) Token {
        return Token{
            .token_type = .Identifier,
            .lexeme = name,
            .line = 0,
        };
    }
};

pub const TokenType = enum {
    // Punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Colon,
    Semicolon,
    // Operators
    Slash,
    Star,
    Minus,
    Plus,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Keywords
    And,
    Break,
    Class,
    Continue,
    Const,
    Else,
    False,
    Fn,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Static,
    Super,
    This,
    True,
    Var,
    With,
    While,
    // Literals
    Identifier,
    String,
    Number,
    // Control
    Error,
    EOF,
};

pub const Scanner = struct {
    start: []const u8,
    current: u32,
    line: u32,

    pub fn create(source: []const u8) Scanner {
        return Scanner {
            .start = source,
            .current = 0,
            .line = 1,
        };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();
        self.start = self.start[self.current..];
        self.current = 0;

        if (self.isAtEnd()) return self.makeToken(.EOF);

        const c = self.advance();
        switch (c) {
            '(' => return self.makeToken(.LeftParen),
            ')' => return self.makeToken(.RightParen),
            '{' => return self.makeToken(.LeftBrace),
            '}' => return self.makeToken(.RightBrace),
            ',' => return self.makeToken(.Comma),
            '.' => return self.makeToken(.Dot),
            '-' => return self.makeToken(.Minus),
            '+' => return self.makeToken(.Plus),
            ':' => return self.makeToken(.Colon),
            ';' => return self.makeToken(.Semicolon),
            '*' => return self.makeToken(.Star),
            '!' => return self.makeToken(if (self.match('=')) .BangEqual else .Bang),
            '=' => return self.makeToken(if (self.match('=')) .EqualEqual else .Equal),
            '<' => return self.makeToken(if (self.match('=')) .LessEqual else .Less),
            '>' => return self.makeToken(if (self.match('=')) .GreaterEqual else .Greater),
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                }
                return self.makeToken(.Slash);
            },
            '"' => return self.readString(),
            else => {
                if (isDigit(c)) {
                    return self.readNumber();
                } else if (isAlpha(c)) {
                    return self.readIdentifier();
                } else if (c == 0) {
                    return self.makeToken(.EOF);
                }
                return self.makeError("Unexpected character");
            },
        }
    }

    pub fn makeError(self: Scanner, message: []const u8) Token {
        return Token.create(.Error, message, self.line);
    }

    pub fn makeToken(self: Scanner, token_type: TokenType) Token {
        return Token.create(token_type, self.start[0..self.current], self.line);
    }

    pub fn makeLiteral(self: Scanner, token_type: TokenType, literal: []const u8) Token {
        return Token.create(token_type, literal, self.line);
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.start.len;
    }

    fn advance(self: *Scanner) u8 {
        const current = self.start[self.current];
        self.current += 1;
        return current;
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.start[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.start.len) return 0;
        return self.start[self.current + 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.start[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isAlphaNumeric(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                else => {
                    return;
                },
            }
        }
    }

    fn readIdentifier(self: *Scanner) Token {
        while (isAlphaNumeric(self.peek())) _ = self.advance();
        const id_type = self.identifierType();
        return self.makeToken(id_type);
    }

    fn identifierType(self: *Scanner) TokenType {
        const text = self.start[0..self.current];
        for (keywords) |kw, i| {
            if (std.mem.eql(u8, kw.name, text)) return kw.token_type;
        }
        return .Identifier;
    }

    fn readString(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.makeError("Unterminated string.");
        }

        // The closing ".
        _ = self.advance();

        // Trim the surrounding quotes.
        return self.makeLiteral(.String, self.start[1 .. self.current - 1]);
    }

    fn readNumber(self: *Scanner) Token {
        while (isDigit(self.peek())) _ = self.advance();
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the "." and the fractional part
            _ = self.advance();
            while (isDigit(self.peek())) _ = self.advance();
        }
        return self.makeLiteral(.Number, self.start[0..self.current]);
    }
};

const std = @import("std");
const debug = @import("./debug.zig");

fn isWhitespace(c: u8) bool {
    return switch (c) {
        ' ', '\r', '\t', '\n' => true,
        else => false,
    };
}

fn isGrouping(c: u8) bool {
    return switch (c) {
        '(', ')', '[', ']', '{', '}' => true,
        else => false,
    };
}

fn isSign(c: u8) bool {
    return c == '-' or c == '+';
}

fn isDigit(c: u8) bool {
    return '0' <= c and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isAlphaNumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

fn isIdentifier(c: u8) bool {
    return !isWhitespace(c) and !isGrouping(c);
}

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
    keyword(.False, "false"),
    keyword(.Nil, "nil"),
    keyword(.True, "true"),
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    index: usize,

    pub fn create(token_type: TokenType, lexeme: []const u8, index: usize) Token {
        return Token{ .token_type = token_type, .lexeme = lexeme, .index = index };
    }

    pub fn symbol(name: []const u8) Token {
        return Token{ .token_type = .Identifier, .lexeme = name, .index = 0 };
    }
};

pub const TokenType = enum(u8) {
    // Punctuation
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Quote,
    Quasiquote,
    Unquote,
    Splice,
    // Literals
    Identifier,
    String,
    Number,
    True,
    False,
    Nil,
    // Control
    Error,
    EOF,
};

pub const Scanner = struct {
    buffer: []const u8,
    current: usize = 0,

    pub fn create(source: []const u8) Scanner {
        return Scanner{ .buffer = source };
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();

        self.buffer = self.buffer[self.current..];
        self.current = 0;

        if (self.isAtEnd()) return self.makeToken(.EOF);

        const c = self.buffer[self.current];
        self.advance();

        switch (c) {
            '(' => return self.makeToken(.LeftParen),
            ')' => return self.makeToken(.RightParen),
            '[' => return self.makeToken(.LeftBracket),
            ']' => return self.makeToken(.RightBracket),
            '{' => return self.makeToken(.LeftBrace),
            '}' => return self.makeToken(.RightBrace),
            '\'' => return self.makeToken(.Quote),
            '~' => return self.makeToken(.Quasiquote),
            ',' => return self.makeToken(.Unquote),
            ';' => return self.makeToken(.Splice),
            '"' => return self.readString(),
            else => {
                if (isDigit(c)) {
                    return self.readNumber();
                } else if (isSign(c) and isDigit(self.peek())) {
                    return self.readNumber();
                } else {
                    return self.readIdentifier();
                }
                return self.makeError("Unexpected character");
            },
        }
    }

    pub fn makeError(self: Scanner, message: []const u8) Token {
        const token = Token.create(.Error, message, self.current);
        if (debug.trace_scanner) std.debug.print("{}\n", .{TokenType.Error});
        return token;
    }

    pub fn makeToken(self: Scanner, token_type: TokenType) Token {
        const token = Token.create(token_type, self.buffer[0..self.current], self.current);
        if (debug.trace_scanner) std.debug.print("{} - {s}\n", .{ token_type, self.buffer[0..self.current] });
        return token;
    }

    pub fn makeLiteral(self: Scanner, token_type: TokenType, literal: []const u8) Token {
        const token = Token.create(token_type, literal, self.current);
        if (debug.trace_scanner) std.debug.print("{} - {s}\n", .{ token_type, literal });
        return token;
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.buffer.len;
    }

    fn advance(self: *Scanner) void {
        self.current += 1;
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.buffer[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.buffer.len) return 0;
        return self.buffer[self.current + 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.buffer[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t', '\n' => self.advance(),
                else => return,
            }
        }
    }

    fn readIdentifier(self: *Scanner) Token {
        while (isIdentifier(self.peek())) self.advance();
        const id_type = self.identifierType();
        return self.makeToken(id_type);
    }

    fn identifierType(self: *Scanner) TokenType {
        const text = self.buffer[0..self.current];
        for (keywords) |kw| {
            if (std.mem.eql(u8, kw.name, text)) return kw.token_type;
        }
        return .Identifier;
    }

    fn readString(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) self.advance();
        if (self.isAtEnd()) return self.makeError("Unterminated string.");
        // The closing ".
        self.advance();
        // Trim the surrounding quotes.
        return self.makeLiteral(.String, self.buffer[1 .. self.current - 1]);
    }

    fn readNumber(self: *Scanner) Token {
        while (isDigit(self.peek())) self.advance();
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the "." and the fractional part
            self.advance();
            while (isDigit(self.peek())) self.advance();
        }
        return self.makeLiteral(.Number, self.buffer[0..self.current]);
    }
};

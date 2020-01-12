const std = @import("std");
const allocator = @import("root").allocator;

const Chunk = @import("./chunk.zig").Chunk;
const VM = @import("./vm.zig").VM;
const OpCode = @import("./vm.zig").OpCode;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Scanner = @import("./scanner.zig").Scanner;
const Value = @import("./value.zig").Value;
const Obj = @import("./object.zig").Obj;
const ObjString = @import("./object.zig").ObjString;
const ObjFunction = @import("./object.zig").ObjFunction;

const verbose = false;
const verbose_parse = false;

pub const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    had_panic: bool,

    pub fn create() Parser {
        return Parser{
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .had_panic = false,
        };
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) void {
        if (self.had_panic) return;
        self.had_panic = true;

        std.debug.warn("[line {}] Error", .{token.line});

        if (token.token_type == TokenType.EOF) {
            std.debug.warn(" at end", .{});
        } else if (token.token_type == TokenType.Error) {
            // Nothing.
        } else {
            std.debug.warn(" at '{}'", .{token.lexeme});
        }

        std.debug.warn(": {}\n", .{message});
        self.had_error = true;
    }
};

pub const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

pub const ParseFn = fn (self: *Instance, canAssign: bool) void;

pub const Precedence = packed enum(u8) {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! - +
    Call, // . () []
    Primary,

    fn next(current: Precedence) Precedence {
        return @intToEnum(Precedence, @enumToInt(current) + 1);
    }

    fn isLowerThan(current: Precedence, other: Precedence) bool {
        return @enumToInt(current) <= @enumToInt(other);
    }
};

fn makeRule(_: TokenType, prefix: ?ParseFn, infix: ?ParseFn, precedence: Precedence) ParseRule {
    return ParseRule{
        .prefix = prefix,
        .infix = infix,
        .precedence = precedence,
    };
}

const num_rules = @memberCount(TokenType);
const rules: [num_rules]ParseRule = blk: {
    var list: [num_rules]ParseRule = undefined;
    list[@enumToInt(TokenType.LeftParen)] = makeRule(.LeftParen, Instance.grouping, Instance.call, Precedence.Call);
    list[@enumToInt(TokenType.RightParen)] = makeRule(.RightParen, null, null, Precedence.None);
    list[@enumToInt(TokenType.LeftBrace)] = makeRule(.LeftBrace, null, null, Precedence.None);
    list[@enumToInt(TokenType.RightBrace)] = makeRule(.RightBrace, null, null, Precedence.None);
    list[@enumToInt(TokenType.Comma)] = makeRule(.Comma, null, null, Precedence.None);
    list[@enumToInt(TokenType.Dot)] = makeRule(.Dot, null, Instance.dot, Precedence.Call);
    list[@enumToInt(TokenType.Minus)] = makeRule(.Minus, Instance.unary, Instance.binary, Precedence.Term);
    list[@enumToInt(TokenType.Plus)] = makeRule(.Plus, null, Instance.binary, Precedence.Term);
    list[@enumToInt(TokenType.Colon)] = makeRule(.Colon, null, null, Precedence.None);
    list[@enumToInt(TokenType.Semicolon)] = makeRule(.Semicolon, null, null, Precedence.None);
    list[@enumToInt(TokenType.Slash)] = makeRule(.Slash, null, Instance.binary, Precedence.Factor);
    list[@enumToInt(TokenType.Star)] = makeRule(.Star, null, Instance.binary, Precedence.Factor);
    list[@enumToInt(TokenType.Bang)] = makeRule(.Bang, Instance.unary, null, Precedence.None);
    list[@enumToInt(TokenType.BangEqual)] = makeRule(.BangEqual, null, Instance.binary, Precedence.Equality);
    list[@enumToInt(TokenType.Equal)] = makeRule(.Equal, null, null, Precedence.None);
    list[@enumToInt(TokenType.EqualEqual)] = makeRule(.EqualEqual, null, Instance.binary, Precedence.Equality);
    list[@enumToInt(TokenType.Greater)] = makeRule(.Greater, null, Instance.binary, Precedence.Comparison);
    list[@enumToInt(TokenType.GreaterEqual)] = makeRule(.GreaterEqual, null, Instance.binary, Precedence.Comparison);
    list[@enumToInt(TokenType.Less)] = makeRule(.Less, null, Instance.binary, Precedence.Comparison);
    list[@enumToInt(TokenType.LessEqual)] = makeRule(.LessEqual, null, Instance.binary, Precedence.Comparison);
    list[@enumToInt(TokenType.Identifier)] = makeRule(.Identifier, Instance.variable, null, Precedence.None);
    list[@enumToInt(TokenType.String)] = makeRule(.String, Instance.string, null, Precedence.None);
    list[@enumToInt(TokenType.Number)] = makeRule(.Number, Instance.number, null, Precedence.None);
    list[@enumToInt(TokenType.And)] = makeRule(.And, null, Instance.andFn, Precedence.And);
    list[@enumToInt(TokenType.Class)] = makeRule(.Class, null, null, Precedence.None);
    list[@enumToInt(TokenType.Const)] = makeRule(.Const, null, null, Precedence.None);
    list[@enumToInt(TokenType.Else)] = makeRule(.Else, null, null, Precedence.None);
    list[@enumToInt(TokenType.False)] = makeRule(.False, Instance.literal, null, Precedence.None);
    list[@enumToInt(TokenType.Fn)] = makeRule(.Fn, null, null, Precedence.None);
    list[@enumToInt(TokenType.For)] = makeRule(.For, null, null, Precedence.None);
    list[@enumToInt(TokenType.If)] = makeRule(.If, null, null, Precedence.None);
    list[@enumToInt(TokenType.Nil)] = makeRule(.Nil, Instance.literal, null, Precedence.None);
    list[@enumToInt(TokenType.Or)] = makeRule(.Or, null, Instance.orFn, Precedence.Or);
    list[@enumToInt(TokenType.Print)] = makeRule(.Print, null, null, Precedence.None);
    list[@enumToInt(TokenType.Return)] = makeRule(.Return, null, null, Precedence.None);
    list[@enumToInt(TokenType.Super)] = makeRule(.Super, null, null, Precedence.None);
    list[@enumToInt(TokenType.This)] = makeRule(.This, null, null, Precedence.None);
    list[@enumToInt(TokenType.True)] = makeRule(.True, Instance.literal, null, Precedence.None);
    list[@enumToInt(TokenType.Var)] = makeRule(.Var, null, null, Precedence.None);
    list[@enumToInt(TokenType.While)] = makeRule(.While, null, null, Precedence.None);
    list[@enumToInt(TokenType.Error)] = makeRule(.Error, null, null, Precedence.None);
    list[@enumToInt(TokenType.EOF)] = makeRule(.EOF, null, null, Precedence.None);

    break :blk list;
};

const FunctionType = enum {
    Function,
    Initializer,
    Method,
    Static,
    Script,
};

pub const Compiler = struct {
    enclosing: ?*Compiler,
    function: *Obj,
    T: FunctionType,

    upvalues: [256]Upvalue,
    locals: [256]Local,
    local_count: u32,
    scope_depth: u32,
    loop_depth: u32,

    pub fn create(current: ?*Compiler, scope_depth: u32, T: FunctionType) Compiler {
        return Compiler{
            .enclosing = current,
            .function = ObjFunction.allocate(),
            .T = T,
            .upvalues = undefined,
            .locals = undefined,
            .local_count = 0,
            .scope_depth = scope_depth,
            .loop_depth = 0,
        };
    }

    pub fn init(self: *Compiler, instance: *Instance) void {
        instance.current = self;

        switch (self.T) {
            .Initializer, .Method, .Static, .Function => {
                self.function.data.Function.name = ObjString.copy(instance.parser.previous.lexeme);
            },
            .Script => self.function.data.Function.name = null,
        }

        const hasReceiver = self.T != .Function and self.T != .Static;
        
        var local = &self.locals[@intCast(u32, self.local_count)];
        local.depth = @intCast(i32, self.scope_depth);
        local.isCaptured = false;
        local.name.lexeme = if (hasReceiver) "this" else "";
        self.local_count += 1;
    }
};

const ClassCompiler = struct {
    enclosing: *ClassCompiler,
    name: Token,
    hasSuperclass: bool,

    pub fn create() ClassCompiler {
        return ClassCompiler{
            .enclosing = undefined,
            .name = undefined,
            .hasSuperclass = undefined,
        };
    }
};

pub const Local = struct {
    name: Token,
    depth: i32,
    isCaptured: bool,
};

pub const Upvalue = struct {
    index: u8,
    isLocal: bool,
};

pub const Instance = struct {
    compiler: Compiler,
    current: *Compiler,
    currentClass: *ClassCompiler,
    parser: Parser,
    scanner: Scanner,
    innermostLoopStart: u32,
    innermostLoopScopeDepth: u32,

    pub fn create() Instance {
        return Instance{
            .compiler = Compiler.create(null, 0, .Script),
            .current = undefined,
            .currentClass = undefined,
            .parser = Parser.create(),
            .scanner = Scanner.create(),
            .innermostLoopStart = 0,
            .innermostLoopScopeDepth = 0,
        };
    }

    pub fn init(self: *Instance) void {
        self.current = &self.compiler;
    }

    pub fn compile(self: *Instance, source: []const u8) !*Obj {
        self.scanner.init(source);

        self.compiler.init(self);

        self.parser.had_error = false;
        self.parser.had_panic = false;
        self.advance();

        while (!self.match(.EOF))
            try self.declaration();

        return self.end();
    }

    fn advance(self: *Instance) void {
        self.parser.previous = self.parser.current;

        while (true) {
            self.parser.current = self.scanner.scanToken();
            if (self.parser.current.token_type != TokenType.Error) break;
            self.parser.errorAtCurrent(self.parser.current.lexeme);
        }
    }

    fn match(self: *Instance, token_type: TokenType) bool {
        if (!self.check(token_type)) return false;
        self.advance();
        return true;
    }

    fn check(self: *Instance, token_type: TokenType) bool {
        return self.parser.current.token_type == token_type;
    }

    fn end(self: *Instance) !*Obj {
        self.emitReturn();

        if (self.parser.had_error)
            return error.ParseError;

        if (verbose)
            self.currentChunk().disassemble("Chunk");

        const func = self.current.function;
        self.current = if (self.current.enclosing) |e| e else &self.compiler;

        return func;
    }

    fn consume(self: *Instance, token_type: TokenType, message: []const u8) void {
        if (self.parser.current.token_type == token_type) {
            _ = self.advance();
            return;
        }

        self.parser.errorAtCurrent(message);
    }

    fn currentChunk(self: Instance) *Chunk {
        return &self.current.function.data.Function.chunk;
    }

    fn statement(self: *Instance) !void {
        if (self.match(.Print)) {
            self.printStatement();
        } else if (self.match(.If)) {
            try self.ifStatement();
        } else if (self.match(.Return)) {
            self.returnStatement();
        } else if (self.match(.While)) {
            try self.whileStatement();
        } else if (self.match(.With)) {
            self.withStatement();
        } else if (self.match(.Break)) {
            self.breakStatement();
        } else if (self.match(.Continue)) {
            self.continueStatement();
        } else if (self.match(.For)) {
            self.forStatement();
        } else if (self.match(.LeftBrace)) {
            self.beginScope();
            try self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn withStatement(self: *Instance) void {}
    fn breakStatement(self: *Instance) void {}
    fn continueStatement(self: *Instance) void {}
    fn forStatement(self: *Instance) void {}

    fn printStatement(self: *Instance) void {
        if (verbose_parse) std.debug.warn("S | Print\n", .{});
        self.expression();
        self.consume(TokenType.Semicolon, "Expect ';' after value.");
        self.emitOpCode(.Print);
    }

    fn ifStatement(self: *Instance) anyerror!void {
        if (verbose_parse) std.debug.warn("S | If\n", .{});
        self.consume(.LeftParen, "Expect '(' after if.");
        self.expression();
        self.consume(.RightParen, "Expect ')' after condition");

        const thenJump = self.emitJump(OpCode.JumpIfFalse);
        self.emitOpCode(.Pop);
        try self.statement();

        const elseJump = self.emitJump(.Jump);

        self.patchJump(thenJump);
        self.emitOpCode(.Pop);

        if (self.match(.Else)) try self.statement();
        self.patchJump(elseJump);
    }

    fn returnStatement(self: *Instance) void {
        if (verbose_parse) std.debug.warn("S | Return\n", .{});

        if (self.current.T == .Script) {
            self.parser.errorAtPrevious("Cannot return from top-level code.");
        }

        if (self.match(.Semicolon)) {
            self.emitReturn();
        } else {
            if (self.current.T == .Initializer) {
                self.parser.errorAtPrevious("Cannot return from an initializer");
            }
            self.expression();
            self.consume(.Semicolon, "Expect ';' after return value.");
            self.emitOpCode(.Return);
        }
    }

    fn patchJump(self: *Instance, offset: usize) void {
        const jump = self.currentChunk().code.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            self.parser.errorAtPrevious("Too much code to jump over");
        }
        self.currentChunk().code.items[offset + 0] = @truncate(u8, (jump >> 8) & 0xff);
        self.currentChunk().code.items[offset + 1] = @truncate(u8, jump & 0xff);
    }

    fn whileStatement(self: *Instance) anyerror!void {
        if (verbose_parse)
            std.debug.warn("S | While\n", .{});

        self.current.loop_depth += 1;

        const surroundingLoopStart = self.innermostLoopStart;
        const surroundingLoopScopeDepth = self.innermostLoopScopeDepth;
        self.innermostLoopStart = @intCast(u32, self.currentChunk().code.len);
        self.innermostLoopScopeDepth = self.current.scope_depth;

        self.consume(.LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(.RightParen, "Expect ')' after condition.");

        const exitJump = self.emitJump(.JumpIfFalse);

        self.emitOpCode(.Pop);
        try self.statement();

        self.emitLoop(@intCast(i32, self.innermostLoopStart));

        self.patchJump(exitJump);
        self.emitOpCode(.Pop);

        self.innermostLoopStart = surroundingLoopStart;
        self.innermostLoopScopeDepth = surroundingLoopScopeDepth;

        self.current.loop_depth -= 1;
    }

    fn expression(self: *Instance) void {
        self.parsePrecedence(Precedence.Assignment);
    }

    fn beginScope(self: *Instance) void {
        self.current.scope_depth += 1;
    }

    fn endScope(self: *Instance) void {
        self.current.scope_depth -= 1;
        var next_index = @intCast(u8, self.current.local_count - 1);
        while (self.current.local_count > 0 and self.current.locals[next_index].depth > self.current.scope_depth) {
            if (self.current.locals[self.current.local_count - 1].isCaptured) {
                self.emitOpCode(.CloseUpvalue);
            } else {
                self.emitOpCode(.Pop);
            }
            self.current.local_count -= 1;
            next_index = @intCast(u8, self.current.local_count);
        }
    }

    fn block(self: *Instance) anyerror!void {
        while (!(self.check(.RightBrace) or self.check(.EOF))) {
            try self.declaration();
        }

        self.consume(.RightBrace, "Expect '}' after block.");
    }

    fn expressionStatement(self: *Instance) void {
        if (verbose_parse) std.debug.warn("S | Expression\n", .{});
        self.expression();
        self.emitOpCode(.Pop);
        self.consume(TokenType.Semicolon, "Expect ';' after expression.");
    }

    fn declaration(self: *Instance) !void {
        if (self.match(TokenType.Class)) {
            try self.classDeclaration();
        } else if (self.match(TokenType.Fn)) {
            try self.fnDeclaration();
        } else if (self.match(TokenType.Var)) {
            self.varDeclaration();
        } else if (self.match(TokenType.Const)) {
            self.constDeclaration();
        } else {
            try self.statement();
        }

        if (self.parser.had_panic) {
            self.synchronize();
        }
    }

    fn synchronize(self: *Instance) void {
        self.parser.had_panic = false;

        while (self.parser.current.token_type != TokenType.EOF) {
            if (self.parser.previous.token_type == TokenType.Semicolon) return;

            switch (self.parser.current.token_type) {
                .Class, .Fn, .Var, .Const, .For, .If, .While, .Print, .Return => return,
                else => {
                    self.advance();
                },
            }
        }
    }

    fn classDeclaration(self: *Instance) !void {
        if (verbose_parse) std.debug.warn("S | Class\n", .{});
        self.consume(.Identifier, "Expect class name.");
        const nameConstant = self.identifierConstant(self.parser.previous);
        self.declareVariable();

        var classCompiler = ClassCompiler.create();
        classCompiler.name = self.parser.previous;
        classCompiler.hasSuperclass = false;
        classCompiler.enclosing = self.currentClass;
        self.currentClass = &classCompiler;

        // TODO: use colon?
        if (self.match(.Less)) {
            if (verbose_parse) std.debug.warn("S | Superclass\n", .{});
            self.consume(.Identifier, "Expect superclass name");
            classCompiler.hasSuperclass = true;

            self.beginScope();
            self.variable(false);
            self.addLocal(Token.symbol("super"));

            self.emitOpCode(.Subclass);
            self.emitByte(nameConstant);
        } else {
            self.emitOpCode(.Class);
            self.emitByte(nameConstant);
        }

        self.consume(.LeftBrace, "Expect '{' before class body.");
        while (!self.check(.RightBrace) and !self.check(.EOF)) try self.method();
        self.consume(.RightBrace, "Expect '}' after class body.");

        if (classCompiler.hasSuperclass) self.endScope();

        self.defineVariable(nameConstant);

        self.currentClass = self.currentClass.enclosing;
    }

    fn method(self: *Instance) !void {
        if (verbose_parse) std.debug.warn("S | Method\n", .{});

        var T: FunctionType = undefined;
        if (self.check(.Static)) {
            T = .Static;
            self.consume(.Static, "Expect static.");
        } else {
            T = .Method;
        }

        self.consume(.Fn, "Expect method declaration.");
        self.consume(.Identifier, "Expect method name.");
        const constant = self.identifierConstant(self.parser.previous);

        if (std.mem.eql(u8, self.parser.previous.lexeme, "init")) {
            T = .Initializer;
        }

        try self.function(T);

        self.emitOpCode(.Method);
        self.emitByte(constant);
    }

    fn fnDeclaration(self: *Instance) !void {
        if (verbose_parse) std.debug.warn("S | Function\n", .{});
        const global = self.parseVariable("Expect function name.");
        self.markInitialized(); // should this be here?
        try self.function(.Function);
        self.defineVariable(global);
    }

    fn function(self: *Instance, fn_type: FunctionType) !void {
        var compiler = Compiler.create(self.current, self.current.scope_depth, fn_type);
        compiler.init(self);

        self.beginScope();
        self.consume(.LeftParen, "Expect '(' after function name.");

        // Parameters
        if (!self.check(.RightParen)) {
            while (true) {
                self.current.function.data.Function.arity += 1;
                if (self.current.function.data.Function.arity > 32)
                    self.parser.errorAtCurrent("Cannot have more than 32 parameters.");
                const paramConstant = self.parseVariable("Expect parameter name.");
                if (verbose_parse) std.debug.warn("S | Parameter\n", .{});
                self.defineVariable(paramConstant);
                if (!self.match(.Comma)) break;
            }
        }

        self.consume(.RightParen, "Expect ')' after parameters.");

        // Body
        self.consume(.LeftBrace, "Expect '{' before function body.");
        try self.block();

        // Function Object
        self.endScope();
        var func = try self.end(); // TODO: Handle static

        self.emitOpCode(.Closure);
        self.emitByte(self.makeConstant(func.value()));

        var i: usize = 0;
        while (i < func.data.Function.upvalueCount) : (i += 1) {
            self.emitByte(if (compiler.upvalues[i].isLocal) 1 else 0);
            self.emitByte(compiler.upvalues[i].index);
        }
    }

    fn constDeclaration(self: *Instance) void {
        const global = self.parseVariable("Expect variable name.");
        if (verbose_parse) std.debug.warn("K | Const\n", .{});
        if (self.match(TokenType.Colon)) self.varType("Expect type name  after ':'.");
        self.consume(TokenType.Equal, "Constants must be initialized.");
        self.expression();
        self.consume(TokenType.Semicolon, "Expect ';' after variable declaration.");
        self.defineVariable(global);
    }

    fn varDeclaration(self: *Instance) void {
        const global = self.parseVariable("Expect variable name.");
        if (verbose_parse) std.debug.warn("K | Var\n", .{});
        if (self.match(TokenType.Colon)) self.varType("Expect type name after ':'.");
        if (self.match(TokenType.Equal)) {
            self.expression();
        } else {
            self.emitOpCode(.Nil);
        }
        self.consume(TokenType.Semicolon, "Expect ';' after variable declaration.");
        self.defineVariable(global);
    }

    fn parseVariable(self: *Instance, errorMessage: []const u8) u8 {
        self.consume(TokenType.Identifier, errorMessage);
        self.declareVariable();
        if (self.current.scope_depth > 0) return 0;
        return self.identifierConstant(self.parser.previous);
    }

    fn varType(self: *Instance, errorMessage: []const u8) void {
        self.consume(TokenType.Identifier, errorMessage);
    }

    fn declareVariable(self: *Instance) void {
        // Global variables are implicitly declared.
        if (self.current.scope_depth == 0) return;

        const name = self.parser.previous;
        var i = self.current.local_count - 1;
        while (i >= 0) : (i -= 1) {
            const local_index = @intCast(u8, i);
            const local = &self.current.locals[local_index];
            if (local.depth != -1 and local.depth < self.current.scope_depth) break;
            if (identifiersEqual(name, local.name)) {
                self.parser.errorAtPrevious("Cannot redeclare variable in this scope.");
            }
        }

        self.addLocal(name);
    }

    fn identifiersEqual(a: Token, b: Token) bool {
        return std.mem.eql(u8, a.lexeme, b.lexeme);
    }

    fn identifierConstant(self: *Instance, name: Token) u8 {
        const obj_string = ObjString.copy(name.lexeme);
        return self.makeConstant(obj_string.value());
    }

    fn markInitialized(self: *Instance) void {
        if (self.current.scope_depth == 0) return;
        const local_index = @intCast(u8, self.current.local_count - 1);
        self.current.locals[local_index].depth = @intCast(i32, self.current.scope_depth);
    }

    fn defineVariable(self: *Instance, global: u8) void {
        if (self.current.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        self.emitBytes(@enumToInt(OpCode.DefineGlobal), global);
    }

    fn addLocal(self: *Instance, name: Token) void {
        if (self.current.local_count == 255) {
            self.parser.errorAtPrevious("Too many local variables in function.");
            return;
        }

        var local = &self.current.locals[@intCast(u8, self.current.local_count)];
        local.name = name;
        local.depth = -1;
        local.isCaptured = false;
        self.current.local_count += 1;
    }

    fn resolveLocal(self: *Instance, compiler: *Compiler, name: Token) !u8 {
        var i: i32 = @intCast(i32, compiler.local_count) - 1;
        while (i >= 0) : (i -= 1) {
            const local_index = @intCast(u8, i);
            const local = &compiler.locals[local_index];
            if (identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    self.parser.errorAtPrevious("Cannot read local variable in its own initializer.");
                }
                return @intCast(u8, i);
            }
        }
        return error.NotFound;
    }

    fn addUpvalue(self: *Instance, compiler: *Compiler, index: u8, isLocal: bool) u8 {
        const upvalueCount = compiler.function.data.Function.upvalueCount;

        var i: u8 = 0;
        while (i < upvalueCount) : (i += 1) {
            const upvalue = &compiler.upvalues[i];
            if (upvalue.index == index and upvalue.isLocal == isLocal) {
                return i;
            }
        }

        if (upvalueCount == 255) {
            self.parser.errorAtPrevious("Too many closure variables in function.");
            return 0;
        }

        compiler.upvalues[upvalueCount].isLocal = isLocal;
        compiler.upvalues[upvalueCount].index = index;
        compiler.function.data.Function.upvalueCount += 1;
        return compiler.function.data.Function.upvalueCount - 1;
    }

    fn resolveUpvalue(self: *Instance, compiler: *Compiler, name: Token) anyerror!u8 {
        if (compiler.enclosing == null)
            return error.NoEnclosingScope;

        if (self.resolveLocal(compiler.enclosing.?, name)) |local| {
            compiler.enclosing.?.locals[local].isCaptured = true;
            return self.addUpvalue(compiler, local, true);
        } else |_| if (self.resolveUpvalue(compiler.enclosing.?, name)) |upvalue| {
            return self.addUpvalue(compiler, upvalue, false);
        } else |_| {
            return error.NotFound;
        }
    }

    fn parsePrecedence(self: *Instance, precedence: Precedence) void {
        _ = self.advance();

        const parsePrefix = getRule(self.parser.previous.token_type).prefix;

        if (verbose_parse)
            std.debug.warn("P | {}: {}\n", .{
                @tagName(self.parser.previous.token_type),
                @tagName(precedence),
            });

        if (parsePrefix == null) {
            self.parser.errorAtPrevious("Expect expression.");
            return;
        }

        const canAssign = precedence.isLowerThan(.Assignment);
        parsePrefix.?(self, canAssign);

        while (precedence.isLowerThan(getRule(self.parser.current.token_type).precedence)) {
            _ = self.advance();
            const parseInfix = getRule(self.parser.previous.token_type).infix.?;
            if (verbose_parse)
                std.debug.warn("I | {}: {}\n", .{
                    @tagName(self.parser.previous.token_type),
                    @tagName(precedence),
                });
            parseInfix(self, canAssign);
        }

        if (canAssign and self.match(TokenType.Equal)) {
            self.parser.errorAtPrevious("Invalid assignment target.");
            self.expression();
        }
    }

    fn grouping(self: *Instance, canAssign: bool) void {
        self.expression();
        self.consume(TokenType.RightParen, "Expect ')' after expression.");
    }

    fn call(self: *Instance, canAssign: bool) void {
        const arg_count = self.argumentList();
        self.emitOpCode(.Call);
        self.emitByte(arg_count);
    }

    fn dot(self: *Instance, canAssign: bool) void {
        self.consume(.Identifier, "Expect property name after '.'.");
        const name = self.identifierConstant(self.parser.previous);
        if (canAssign and self.match(.Equal)) {
            self.expression();
            self.emitOpCode(.SetProperty);
            self.emitByte(name);
        } else {
            self.emitOpCode(.GetProperty);
            self.emitByte(name);
        }
    }

    fn argumentList(self: *Instance) u8 {
        var arg_count: u8 = 0;
        if (!self.check(.RightParen)) {
            self.expression();
            arg_count += 1;
            while (self.match(.Comma)) {
                self.expression();
                if (arg_count == 32) {
                    self.parser.errorAtPrevious("Cannot have more than 32 arguments.");
                }
                arg_count += 1;
            }
        }

        self.consume(.RightParen, "Expect ')' after arguments.");
        return arg_count;
    }

    fn number(self: *Instance, canAssign: bool) void {
        const value = std.fmt.parseUnsigned(u8, self.parser.previous.lexeme, 10) catch unreachable;
        self.emitConstant(Value{ .Number = @intToFloat(f64, value) });
    }

    fn unary(self: *Instance, canAssign: bool) void {
        const operator_type = self.parser.previous.token_type;

        // Compile the operand.
        self.parsePrecedence(Precedence.Unary);

        // Emit the operator instruction.
        switch (operator_type) {
            TokenType.Bang => self.emitOpCode(.Not),
            TokenType.Minus => self.emitOpCode(.Negate),
            else => unreachable,
        }
    }

    fn binary(self: *Instance, canAssign: bool) void {
        // Remember the operator.
        const operator_type = self.parser.previous.token_type;

        // Compile the right operand.
        const rule = getRule(operator_type);
        self.parsePrecedence(rule.precedence.next());

        // Emit the operator instruction.
        switch (operator_type) {
            TokenType.BangEqual => self.emitOpCodes(OpCode.Equal, OpCode.Not),
            TokenType.EqualEqual => self.emitOpCode(.Equal),
            TokenType.Greater => self.emitOpCode(.Greater),
            TokenType.GreaterEqual => self.emitOpCodes(OpCode.Less, OpCode.Not),
            TokenType.Less => self.emitOpCode(.Less),
            TokenType.LessEqual => self.emitOpCodes(OpCode.Greater, OpCode.Not),
            TokenType.Plus => self.emitOpCode(.Add),
            TokenType.Minus => self.emitOpCode(.Subtract),
            TokenType.Star => self.emitOpCode(.Multiply),
            TokenType.Slash => self.emitOpCode(.Divide),
            else => unreachable,
        }
    }

    fn getRule(token_type: TokenType) *const ParseRule {
        const rule = &rules[@enumToInt(token_type)];
        return rule;
    }

    fn literal(self: *Instance, canAssign: bool) void {
        switch (self.parser.previous.token_type) {
            TokenType.False => self.emitOpCode(.False),
            TokenType.Nil => self.emitOpCode(.Nil),
            TokenType.True => self.emitOpCode(.True),
            else => unreachable,
        }
    }

    fn variable(self: *Instance, canAssign: bool) void {
        self.namedVariable(self.parser.previous, canAssign);
    }

    fn namedVariable(self: *Instance, name: Token, canAssign: bool) void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;
        const arg = blk: {
            if (self.resolveLocal(self.current, name)) |l| {
                getOp = .GetLocal;
                setOp = .SetLocal;
                break :blk l;
            } else |_| if (self.resolveUpvalue(self.current, name)) |u| {
                getOp = .GetUpvalue;
                setOp = .SetUpvalue;
                break :blk u;
            } else |_| {
                getOp = .GetGlobal;
                setOp = .SetGlobal;
                break :blk self.identifierConstant(name);
            }
        };

        if (canAssign and self.match(TokenType.Equal)) {
            self.expression();
            self.emitBytes(@enumToInt(setOp), arg);
        } else {
            self.emitBytes(@enumToInt(getOp), arg);
        }
    }

    fn string(self: *Instance, canAssign: bool) void {
        const lexeme = self.parser.previous.lexeme;
        self.emitConstant(ObjString.copy(lexeme[0..]).value());
    }

    fn orFn(self: *Instance, canAssign: bool) void {
        const elseJump = self.emitJump(.JumpIfFalse);
        const endJump = self.emitJump(.Jump);

        self.patchJump(elseJump);
        self.emitOpCode(.Pop);

        self.parsePrecedence(.Or);
        self.patchJump(endJump);
    }

    fn andFn(self: *Instance, canAssign: bool) void {
        const endJump = self.emitJump(.JumpIfFalse);

        self.emitOpCode(.Pop);
        self.parsePrecedence(.And);

        self.patchJump(endJump);
    }

    fn emitJump(self: *Instance, op: OpCode) usize {
        self.emitOpCode(op);
        self.emitByte('\xFF');
        self.emitByte('\xFF');

        return self.currentChunk().code.len - 2;
    }

    fn emitLoop(self: *Instance, loopStart: i32) void {
        self.emitOpCode(.Loop);
        const count = @intCast(i32, self.currentChunk().code.len);
        const offset = @intCast(u16, count - loopStart + 2);
        if (offset > std.math.maxInt(u16)) self.parser.errorAtPrevious("Loop body too large.");

        self.emitByte(@truncate(u8, (offset >> 8) & 0xff));
        self.emitByte(@truncate(u8, offset & 0xff));
    }

    fn emitReturn(self: *Instance) void {
        // An initializer automatically returns "this".
        if (self.current.T == .Initializer) {
            self.emitOpCode(.GetLocal);
            self.emitByte(0);
        } else {
            self.emitOpCode(.Nil);
        }

        self.emitOpCode(.Return);
    }

    fn emit(self: *Instance, code: var) void {}

    fn emitOpCode(self: *Instance, op: OpCode) void {
        self.emitByte(@enumToInt(op));
    }

    fn emitOpCodes(self: *Instance, op1: OpCode, op2: OpCode) void {
        self.emitByte(@enumToInt(op1));
        self.emitByte(@enumToInt(op2));
    }

    fn emitByte(self: *Instance, byte: u8) void {
        self.currentChunk().write(byte, self.parser.previous.line) catch unreachable;
    }

    fn emitBytes(self: *Instance, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn emitConstant(self: *Instance, value: Value) void {
        self.emitBytes(@enumToInt(OpCode.Constant), self.makeConstant(value));
    }

    fn makeConstant(self: *Instance, value: Value) u8 {
        // if (constant > UINT8_MAX) {
        //     error("Too many constants in one chunk.");
        //     return 0;
        // }
        return self.currentChunk().addConstant(value);
    }
};

const std = @import("std");

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Obj = @import("./object.zig").Obj;
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;

const debug = @import("./debug.zig");
const tracy = @import("./tracy.zig");

pub const Parser = struct {
    scanner: Scanner,
    current: Token = undefined,
    previous: Token = undefined,
    innermostLoopStart: u32 = 0,
    innermostLoopScopeDepth: u32 = 0,
    had_error: bool = false,
    had_panic: bool = false,

    fn create(source: []const u8) Parser {
        return Parser{
            .scanner = Scanner.create(source),
        };
    }

    fn advance(self: *Parser) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.token_type != .Error) break;
            self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (self.current.token_type == token_type) {
            _ = self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn match(self: *Parser, token_type: TokenType) bool {
        const t = tracy.Zone(@src());
        defer t.End();

        if (!self.check(token_type)) return false;
        self.advance();
        return true;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        const t = tracy.Zone(@src());
        defer t.End();

        return self.current.token_type == token_type;
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.errorAt(&self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (self.had_panic) return;
        self.had_panic = true;

        std.debug.print("[line {}] Error", .{token.line});

        if (token.token_type == .EOF) {
            std.debug.print(" at end", .{});
        } else if (token.token_type == .Error) {
            // Nothing.
        } else {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{message});
        self.had_error = true;
    }
};

pub const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

pub const ParseFn = fn (self: *Context, canAssign: bool) void;

pub const Precedence = enum(u8) {
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

fn makeRule(comptime prefix: ?ParseFn, comptime infix: ?ParseFn, comptime precedence: Precedence) ParseRule {
    return ParseRule{
        .prefix = prefix,
        .infix = infix,
        .precedence = precedence,
    };
}

fn getRule(token: TokenType) ParseRule {
    return switch (token) {
        .LeftParen => makeRule(Context.grouping, Context.call, .Call),
        .RightParen => makeRule(null, null, .None),
        .LeftBracket => makeRule(Context.list, Context.subscript, .Call),
        .RightBracket => makeRule(null, null, .None),
        .LeftBrace => makeRule(null, null, .None),
        .RightBrace => makeRule(null, null, .None),
        .Comma => makeRule(null, null, .None),
        .Dot => makeRule(null, Context.dot, .Call),
        .Minus => makeRule(Context.unary, Context.binary, .Term),
        .Plus => makeRule(null, Context.binary, .Term),
        .Colon => makeRule(null, null, .None),
        .Semicolon => makeRule(null, null, .None),
        .Slash => makeRule(null, Context.binary, .Factor),
        .Star => makeRule(null, Context.binary, .Factor),
        .Bang => makeRule(Context.unary, null, .None),
        .BangEqual => makeRule(null, Context.binary, .Equality),
        .Equal => makeRule(null, null, .None),
        .EqualEqual => makeRule(null, Context.binary, .Equality),
        .Greater => makeRule(null, Context.binary, .Comparison),
        .GreaterEqual => makeRule(null, Context.binary, .Comparison),
        .Less => makeRule(null, Context.binary, .Comparison),
        .LessEqual => makeRule(null, Context.binary, .Comparison),
        .Identifier => makeRule(Context.variable, null, .None),
        .String => makeRule(Context.string, null, .None),
        .Number => makeRule(Context.number, null, .None),
        .And => makeRule(null, Context.andFn, .And),
        .Break => makeRule(null, null, .None),
        .Continue => makeRule(null, null, .None),
        .Class => makeRule(null, null, .None),
        .Const => makeRule(null, null, .None),
        .Else => makeRule(null, null, .None),
        .False => makeRule(Context.literal, null, .None),
        .Fn => makeRule(null, null, .None),
        .For => makeRule(null, null, .None),
        .If => makeRule(null, null, .None),
        .Nil => makeRule(Context.literal, null, .None),
        .Or => makeRule(null, Context.orFn, .Or),
        .Print => makeRule(null, null, .None),
        .Return => makeRule(null, null, .None),
        .Static => makeRule(null, null, .None),
        .Super => makeRule(Context.super, null, .None),
        .This => makeRule(Context.this, null, .None),
        .True => makeRule(Context.literal, null, .None),
        .Var => makeRule(null, null, .None),
        .With => makeRule(null, null, .None),
        .While => makeRule(null, null, .None),
        .Error => makeRule(null, null, .None),
        .EOF => makeRule(null, null, .None),
    };
}

const FunctionType = enum {
    Function,
    Initializer,
    Method,
    Static,
    Abstract,
    TopLevel,
};

pub const Compiler = struct {
    enclosing: ?*Compiler,
    function: *Obj.Function,
    T: FunctionType,

    upvalues: std.ArrayList(Upvalue),
    locals: std.ArrayList(Local),
    scope_depth: u32,
    loop_depth: u32,

    pub fn create(vm: *VM, current: ?*Compiler, scope_depth: u32, T: FunctionType) !Compiler {
        return Compiler{
            .enclosing = current,
            .function = try Obj.Function.create(vm),
            .T = T,
            .upvalues = std.ArrayList(Upvalue).init(vm.allocator),
            .locals = std.ArrayList(Local).init(vm.allocator),
            .scope_depth = scope_depth,
            .loop_depth = 0,
        };
    }

    pub fn init(self: *Compiler, context: *Context) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        context.current = self;

        switch (self.T) {
            .Initializer, .Method, .Static, .Function => {
                self.function.name = try Obj.String.copy(context.vm, context.parser.previous.lexeme);
            },
            .Abstract => unreachable,
            .TopLevel => self.function.name = null,
        }

        // const hasReceiver = self.T != .Function and self.T != .Static;

        try self.locals.append(Local{
            .depth = @intCast(i32, self.scope_depth),
            .isCaptured = false,
            .name = Token.symbol(if (self.T == .Function) "" else "this"),
        });
    }

    pub fn deinit(self: *Compiler) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.locals.deinit();
        self.upvalues.deinit();
    }

    fn chunk(self: *const Compiler) *Chunk {
        return &self.function.chunk;
    }
};

const ClassCompiler = struct {
    enclosing: ?*ClassCompiler = undefined,
    name: Token = undefined,
    hasSuperclass: bool = false,
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

pub const Context = struct {
    vm: *VM = undefined,
    parser: *Parser = undefined,
    current: *Compiler = undefined,
    currentClass: ?*ClassCompiler = null,

    const Error = error{ CompileError, RuntimeError, OutOfMemory };

    pub fn compile(self: *Context, vm: *VM, source: []const u8) !*Obj.Function {
        const t = tracy.Zone(@src());
        defer t.End();

        var compiler = try Compiler.create(vm, null, 0, .TopLevel);
        try compiler.init(self);
        defer compiler.deinit();

        var parser = Parser.create(source);

        self.vm = vm;
        self.current = &compiler;
        self.parser = &parser;

        self.parser.advance();

        while (!self.parser.match(.EOF))
            try self.declaration();

        return self.end();
    }

    fn end(self: *Context) !*Obj.Function {
        const t = tracy.Zone(@src());
        defer t.End();

        self.emitReturn();

        if (self.parser.had_error)
            return error.CompileError;

        const func = self.current.function;

        if (self.current.enclosing) |outer| {
            // Capture the upvalues in the new closure object.
            self.emitUnaryOp(outer, .Closure, self.makeConstant(outer, func.obj.value()));

            // Emit arguments for each upvalue to know whether to capture a local or an upvalue.
            var i: usize = 0;
            while (i < func.upvalueCount) : (i += 1) {
                self.emitByte(outer, if (self.current.upvalues.items[i].isLocal) 1 else 0);
                self.emitByte(outer, self.current.upvalues.items[i].index);
            }
        }

        self.current = self.current.enclosing orelse undefined;

        if (debug.trace_compiler) {
            const name = if (func.name) |n| n.bytes else "Chunk";
            func.chunk.disassemble(name);
        }

        return func;
    }

    fn statement(self: *Context) Error!void {
        if (self.parser.match(.Print)) {
            self.printStatement();
        } else if (self.parser.match(.If)) {
            try self.ifStatement();
        } else if (self.parser.match(.Return)) {
            self.returnStatement();
        } else if (self.parser.match(.While)) {
            try self.whileStatement();
        } else if (self.parser.match(.With)) {
            self.withStatement();
        } else if (self.parser.match(.Break)) {
            self.breakStatement();
        } else if (self.parser.match(.Continue)) {
            self.continueStatement();
        } else if (self.parser.match(.For)) {
            self.forStatement();
        } else if (self.parser.match(.LeftBrace)) {
            self.beginScope();
            try self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn withStatement(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        _ = self;
    }
    fn breakStatement(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        _ = self;
    }
    fn continueStatement(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        _ = self;
    }
    fn forStatement(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        _ = self;
    }

    fn printStatement(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        if (debug.trace_parser) std.debug.print("S | Print\n", .{});
        self.expression();
        self.parser.consume(.Semicolon, "Expect ';' after value.");
        self.emitOp(self.current, .Print);
    }

    fn ifStatement(self: *Context) Error!void {
        const t = tracy.Zone(@src());
        defer t.End();
        if (debug.trace_parser) std.debug.print("S | If\n", .{});
        self.parser.consume(.LeftParen, "Expect '(' after if.");
        self.expression();
        self.parser.consume(.RightParen, "Expect ')' after condition");

        const thenJump = self.emitJump(OpCode.JumpIfFalse);
        self.emitOp(self.current, .Pop);
        try self.statement();

        const elseJump = self.emitJump(.Jump);

        self.patchJump(thenJump);
        self.emitOp(self.current, .Pop);

        if (self.parser.match(.Else)) try self.statement();
        self.patchJump(elseJump);
    }

    fn returnStatement(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        if (debug.trace_parser) std.debug.print("S | Return\n", .{});

        if (self.current.T == .TopLevel) {
            self.parser.errorAtPrevious("Cannot return from top-level code.");
        }

        if (self.parser.match(.Semicolon)) {
            self.emitReturn();
        } else {
            if (self.current.T == .Initializer) {
                self.parser.errorAtPrevious("Cannot return from an initializer");
            }
            self.expression();
            self.parser.consume(.Semicolon, "Expect ';' after return value.");
            self.emitOp(self.current, .Return);
        }
    }

    fn patchJump(self: *Context, offset: usize) void {
        const t = tracy.Zone(@src());
        defer t.End();
        const jump = self.current.chunk().code.items.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            self.parser.errorAtPrevious("Too much code to jump over");
        }
        self.current.chunk().code.items[offset + 0] = @truncate(u8, (jump >> 8) & 0xff);
        self.current.chunk().code.items[offset + 1] = @truncate(u8, jump & 0xff);
    }

    fn whileStatement(self: *Context) !void {
        const t = tracy.Zone(@src());
        defer t.End();
        if (debug.trace_parser)
            std.debug.print("S | While\n", .{});

        self.current.loop_depth += 1;

        const surroundingLoopStart = self.parser.innermostLoopStart;
        const surroundingLoopScopeDepth = self.parser.innermostLoopScopeDepth;
        self.parser.innermostLoopStart = @intCast(u32, self.current.chunk().code.items.len);
        self.parser.innermostLoopScopeDepth = self.current.scope_depth;

        self.parser.consume(.LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.parser.consume(.RightParen, "Expect ')' after condition.");

        const exitJump = self.emitJump(.JumpIfFalse);

        self.emitOp(self.current, .Pop);
        try self.statement();

        self.emitLoop(@intCast(i32, self.parser.innermostLoopStart));

        self.patchJump(exitJump);
        self.emitOp(self.current, .Pop);

        self.parser.innermostLoopStart = surroundingLoopStart;
        self.parser.innermostLoopScopeDepth = surroundingLoopScopeDepth;

        self.current.loop_depth -= 1;
    }

    fn expression(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        self.parsePrecedence(Precedence.Assignment);
    }

    fn beginScope(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        self.current.scope_depth += 1;
    }

    fn endScope(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        self.current.scope_depth -= 1;
        while (self.current.locals.popOrNull()) |l| {
            if (l.depth <= self.current.scope_depth) break;
            if (l.isCaptured) {
                self.emitOp(self.current, .CloseUpvalue);
            } else {
                self.emitOp(self.current, .Pop);
            }
        }
    }

    fn block(self: *Context) !void {
        const t = tracy.Zone(@src());
        defer t.End();
        while (!(self.parser.check(.RightBrace) or self.parser.check(.EOF))) {
            try self.declaration();
        }

        self.parser.consume(.RightBrace, "Expect '}' after block.");
    }

    fn expressionStatement(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        if (debug.trace_parser) std.debug.print("S | Expression\n", .{});
        self.expression();
        self.emitOp(self.current, .Pop);
        self.parser.consume(.Semicolon, "Expect ';' after expression.");
    }

    fn declaration(self: *Context) Error!void {
        const t = tracy.Zone(@src());
        defer t.End();
        if (self.parser.match(.Class)) {
            try self.classDeclaration();
        } else if (self.parser.match(.Fn)) {
            try self.fnDeclaration();
        } else if (self.parser.match(.Var)) {
            try self.varDeclaration();
        } else if (self.parser.match(.Const)) {
            try self.constDeclaration();
        } else {
            try self.statement();
        }

        if (self.parser.had_panic) {
            self.synchronize();
        }
    }

    fn synchronize(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();
        self.parser.had_panic = false;

        while (self.parser.current.token_type != .EOF) {
            if (self.parser.previous.token_type == .Semicolon) return;

            switch (self.parser.current.token_type) {
                .Class, .Fn, .Var, .Const, .For, .If, .While, .Print, .Return => return,
                else => {
                    self.parser.advance();
                },
            }
        }
    }

    fn classDeclaration(self: *Context) Error!void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (debug.trace_parser) std.debug.print("S | Class\n", .{});
        self.parser.consume(.Identifier, "Expect class name.");

        const nameConstant = try self.identifierConstant(self.parser.previous);
        try self.declareVariable();

        self.emitUnaryOp(self.current, .Class, nameConstant);
        self.defineVariable(nameConstant);

        var classCompiler = ClassCompiler{
            .name = self.parser.previous,
            .hasSuperclass = false,
            .enclosing = self.currentClass,
        };

        self.currentClass = &classCompiler;
        defer self.currentClass = self.currentClass.?.enclosing;

        if (self.parser.match(.Less)) {
            if (debug.trace_parser) std.debug.print("S | Superclass\n", .{});
            self.parser.consume(.Identifier, "Expect superclass name");

            self.variable(false);

            if (identifiersEqual(classCompiler.name, self.parser.previous)) {
                self.parser.errorAtPrevious("Class cannot inherit from itself.");
                return error.CompileError;
            }

            self.beginScope();
            try self.addLocal(Token.symbol("super"));
            self.defineVariable(0);

            try self.namedVariable(classCompiler.name, false);
            self.emitOp(self.current, .Inherit);
            classCompiler.hasSuperclass = true;
        }

        try self.namedVariable(classCompiler.name, false);

        self.parser.consume(.LeftBrace, "Expect '{' before class body.");
        while (!self.parser.check(.RightBrace) and !self.parser.check(.EOF)) try self.method();
        self.parser.consume(.RightBrace, "Expect '}' after class body.");

        self.emitOp(self.current, .Pop);

        if (classCompiler.hasSuperclass) self.endScope();
    }

    fn method(self: *Context) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (debug.trace_parser) std.debug.print("S | Method\n", .{});

        var T: FunctionType = undefined;
        if (self.parser.check(.Static)) {
            T = .Static;
            self.parser.consume(.Static, "Expect static.");
        } else {
            T = .Method;
        }

        self.parser.consume(.Fn, "Expect method declaration.");
        self.parser.consume(.Identifier, "Expect method name.");
        const constant = try self.identifierConstant(self.parser.previous);

        if (std.mem.eql(u8, self.parser.previous.lexeme, "init")) {
            T = .Initializer;
        }

        try self.function(T);

        self.emitUnaryOp(self.current, .Method, constant);
    }

    fn fnDeclaration(self: *Context) Error!void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (debug.trace_parser) std.debug.print("S | Function\n", .{});
        const global = try self.parseVariable("Expect function name.");
        self.markInitialized(); // should this be here?
        try self.function(.Function);
        self.defineVariable(global);
    }

    fn function(self: *Context, fn_type: FunctionType) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        var compiler = try Compiler.create(self.vm, self.current, self.current.scope_depth, fn_type);

        try compiler.init(self);
        defer compiler.deinit();

        self.current = &compiler;
        self.current.function.name = try Obj.String.copy(self.vm, self.parser.previous.lexeme);

        self.beginScope();
        self.parser.consume(.LeftParen, "Expect '(' after function name.");

        // Parameters
        if (!self.parser.check(.RightParen)) {
            while (true) {
                if (self.current.function.arity == 32) {
                    self.parser.errorAtCurrent("Cannot have more than 32 parameters.");
                }
                self.current.function.arity += 1;
                const paramConstant = try self.parseVariable("Expect parameter name.");
                if (debug.trace_parser) std.debug.print("S | Parameter\n", .{});
                self.defineVariable(paramConstant);
                if (!self.parser.match(.Comma)) break;
            }
        }

        self.parser.consume(.RightParen, "Expect ')' after parameters.");

        // Body
        self.parser.consume(.LeftBrace, "Expect '{' before function body.");
        try self.block();

        // Function Object
        self.endScope();

        var func = try self.end(); // TODO: Handle static
        _ = func;
    }

    fn constDeclaration(self: *Context) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const global = try self.parseVariable("Expect variable name.");
        if (debug.trace_parser) std.debug.print("K | Const\n", .{});
        if (self.parser.match(.Colon)) self.varType("Expect type name  after ':'.");
        self.parser.consume(.Equal, "Constants must be initialized.");
        self.expression();
        self.parser.consume(.Semicolon, "Expect ';' after variable declaration.");
        self.defineVariable(global);
    }

    fn varDeclaration(self: *Context) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const global = try self.parseVariable("Expect variable name.");
        if (debug.trace_parser) std.debug.print("K | Var\n", .{});
        if (self.parser.match(.Colon)) self.varType("Expect type name after ':'.");
        if (self.parser.match(.Equal)) {
            self.expression();
        } else {
            self.emitOp(self.current, .Nil);
        }
        self.parser.consume(.Semicolon, "Expect ';' after variable declaration.");
        self.defineVariable(global);
    }

    fn parseVariable(self: *Context, errorMessage: []const u8) !u8 {
        const t = tracy.Zone(@src());
        defer t.End();

        self.parser.consume(.Identifier, errorMessage);
        try self.declareVariable();
        if (self.current.scope_depth > 0) return 0;
        return try self.identifierConstant(self.parser.previous);
    }

    fn varType(self: *Context, errorMessage: []const u8) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.parser.consume(.Identifier, errorMessage);
    }

    fn declareVariable(self: *Context) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        // Global variables are implicitly declared.
        if (self.current.scope_depth == 0) return;

        const name = self.parser.previous;

        var i = self.current.locals.items.len - 1;
        while (i >= 0) : (i -= 1) {
            const local_index = @intCast(u8, i);
            const local = &self.current.locals.items[local_index];
            if (local.depth != -1 and local.depth < self.current.scope_depth) break;
            if (identifiersEqual(name, local.name)) {
                self.parser.errorAtPrevious("Cannot redeclare variable in this scope.");
            }
        }

        try self.addLocal(name);
    }

    fn identifiersEqual(a: Token, b: Token) bool {
        const t = tracy.Zone(@src());
        defer t.End();

        return std.mem.eql(u8, a.lexeme, b.lexeme);
    }

    fn identifierConstant(self: *Context, name: Token) Error!u8 {
        const t = tracy.Zone(@src());
        defer t.End();

        const obj_string = try Obj.String.copy(self.vm, name.lexeme);
        return self.makeConstant(self.current, obj_string.obj.value());
    }

    fn markInitialized(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (self.current.scope_depth == 0) return;
        const local_index = @intCast(u8, self.current.locals.items.len - 1);
        self.current.locals.items[local_index].depth = @intCast(i32, self.current.scope_depth);
    }

    fn defineVariable(self: *Context, global: u8) void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (self.current.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        self.emitUnaryOp(self.current, .DefineGlobal, global);
    }

    fn addLocal(self: *Context, name: Token) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (self.current.locals.items.len == 255) {
            self.parser.errorAtPrevious("Too many local variables in function.");
            return;
        }

        try self.current.locals.append(.{ .name = name, .depth = -1, .isCaptured = false });
    }

    fn resolveLocal(self: *Context, compiler: *Compiler, name: Token) ?u8 {
        const t = tracy.Zone(@src());
        defer t.End();

        var i: i32 = @intCast(i32, compiler.locals.items.len) - 1;
        while (i >= 0) : (i -= 1) {
            const local_index = @intCast(u8, i);
            const local = &compiler.locals.items[local_index];
            if (identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    self.parser.errorAtPrevious("Cannot read local variable in its own initializer.");
                }
                return @intCast(u8, i);
            }
        }
        return null;
    }

    fn addUpvalue(self: *Context, compiler: *Compiler, index: u8, isLocal: bool) !u8 {
        const t = tracy.Zone(@src());
        defer t.End();

        for (compiler.upvalues.items) |u, i| {
            if (u.index == index and u.isLocal == isLocal) {
                return @intCast(u8, i);
            }
        }

        if (compiler.upvalues.items.len == std.math.maxInt(u8)) {
            self.parser.errorAtPrevious("Too many closure variables in function.");
            return 0;
        }

        try compiler.upvalues.append(.{ .isLocal = isLocal, .index = index });
        compiler.function.upvalueCount += 1;

        return @intCast(u8, compiler.upvalues.items.len - 1);
    }

    fn resolveUpvalue(self: *Context, compiler: *Compiler, name: Token) ?u8 {
        const t = tracy.Zone(@src());
        defer t.End();

        if (compiler.enclosing == null)
            return null;

        if (self.resolveLocal(compiler.enclosing.?, name)) |local| {
            compiler.enclosing.?.locals.items[local].isCaptured = true;
            return self.addUpvalue(compiler, local, true) catch null;
        } else if (self.resolveUpvalue(compiler.enclosing.?, name)) |upvalue| {
            return self.addUpvalue(compiler, upvalue, false) catch null;
        } else {
            return null;
        }
    }

    fn parsePrecedence(self: *Context, precedence: Precedence) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = self.parser.advance();

        const parsePrefix = getRule(self.parser.previous.token_type).prefix;

        if (debug.trace_parser)
            std.debug.print("P | {s}: {s}\n", .{
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
            _ = self.parser.advance();
            const parseInfix = getRule(self.parser.previous.token_type).infix.?;
            if (debug.trace_parser)
                std.debug.print("I | {s}: {s}\n", .{
                    @tagName(self.parser.previous.token_type),
                    @tagName(precedence),
                });
            parseInfix(self, canAssign);
        }

        if (canAssign and self.parser.match(.Equal)) {
            self.parser.errorAtPrevious("Invalid assignment target.");
            self.expression();
        }
    }

    fn grouping(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        self.expression();
        self.parser.consume(.RightParen, "Expect ')' after expression.");
    }

    fn call(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        const arg_count = self.argumentList();
        self.emitOp(self.current, .Call);
        self.emitByte(self.current, arg_count);
    }

    fn dot(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.parser.consume(.Identifier, "Expect property name after '.'.");
        const name = self.identifierConstant(self.parser.previous) catch unreachable;
        if (canAssign and self.parser.match(.Equal)) {
            self.expression();
            self.emitUnaryOp(self.current, .SetProperty, name);
        } else if (self.parser.match(.LeftParen)) {
            const argCount = self.argumentList();
            self.emitUnaryOp(self.current, .Invoke, name);
            self.emitByte(self.current, argCount);
        } else {
            self.emitUnaryOp(self.current, .GetProperty, name);
        }
    }

    fn argumentList(self: *Context) u8 {
        const t = tracy.Zone(@src());
        defer t.End();

        var arg_count: u8 = 0;
        if (!self.parser.check(.RightParen)) {
            arg_count += 1;
            self.expression();
            while (self.parser.match(.Comma)) {
                self.expression();
                if (arg_count == 32) {
                    self.parser.errorAtPrevious("Cannot have more than 32 arguments.");
                }
                arg_count += 1;
            }
        }

        self.parser.consume(.RightParen, "Expect ')' after arguments.");
        return arg_count;
    }

    fn number(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        const value = std.fmt.parseUnsigned(u8, self.parser.previous.lexeme, 10) catch unreachable;
        self.emitConstant(self.current, Value.fromNumber(@intToFloat(f64, value)));
    }

    fn unary(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        const operator_type = self.parser.previous.token_type;

        // Compile the operand.
        self.parsePrecedence(Precedence.Unary);

        // Emit the operator instruction.
        switch (operator_type) {
            .Bang => self.emitOp(self.current, .Not),
            .Minus => self.emitOp(self.current, .Negate),
            else => unreachable,
        }
    }

    fn binary(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;

        // Remember the operator.
        const operator_type = self.parser.previous.token_type;

        // Compile the right operand.
        const rule = getRule(operator_type);
        self.parsePrecedence(rule.precedence.next());

        // Emit the operator instruction.
        switch (operator_type) {
            .BangEqual => {
                self.emitOp(self.current, .Equal);
                self.emitOp(self.current, .Not);
            },
            .EqualEqual => self.emitOp(self.current, .Equal),
            .Greater => self.emitOp(self.current, .Greater),
            .GreaterEqual => {
                self.emitOp(self.current, .Less);
                self.emitOp(self.current, .Not);
            },
            .Less => self.emitOp(self.current, .Less),
            .LessEqual => {
                self.emitOp(self.current, .Greater);
                self.emitOp(self.current, .Not);
            },
            .Plus => self.emitOp(self.current, .Add),
            .Minus => self.emitOp(self.current, .Subtract),
            .Star => self.emitOp(self.current, .Multiply),
            .Slash => self.emitOp(self.current, .Divide),
            else => unreachable,
        }
    }

    fn literal(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        switch (self.parser.previous.token_type) {
            .Nil => self.emitOp(self.current, .Nil),
            .False => self.emitOp(self.current, .False),
            .True => self.emitOp(self.current, .True),
            else => unreachable,
        }
    }

    fn list(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        self.emitOp(self.current, .NewList);
        while (true) {
            if (self.parser.check(.RightBracket)) break;
            self.expression();
            self.emitOp(self.current, .AddList);
            if (!self.parser.match(.Comma)) break;
        }

        self.parser.consume(.RightBracket, "Expected closing ']'");
    }

    fn subscript(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        self.expression();
        self.parser.consume(.RightBracket, "Expected closing ']'");
        if (self.parser.match(.Equal)) {
            self.expression();
            self.emitOp(self.current, .SubscriptAssign);
        } else {
            self.emitOp(self.current, .Subscript);
        }
    }

    fn variable(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.namedVariable(self.parser.previous, canAssign) catch unreachable;
    }

    fn super(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        if (self.currentClass) |class| {
            if (!class.hasSuperclass) {
                self.parser.errorAtPrevious("Cannot use 'super' in a class with no superclass.");
            }
        } else {
            self.parser.errorAtPrevious("Cannot use 'super' outside of a class.");
        }

        self.parser.consume(.Dot, "Expect '.' after 'super'");
        self.parser.consume(.Identifier, "Expect superclass method name");
        const name = self.identifierConstant(self.parser.previous) catch unreachable;

        self.namedVariable(Token.symbol("this"), false) catch unreachable;
        if (self.parser.match(.LeftParen)) {
            const argCount = self.argumentList();
            self.namedVariable(Token.symbol("super"), false) catch unreachable;
            self.emitUnaryOp(self.current, .SuperInvoke, name);
            self.emitByte(self.current, argCount);
        } else {
            self.namedVariable(Token.symbol("super"), false) catch unreachable;
            self.emitUnaryOp(self.current, .GetSuper, name);
        }
    }

    fn this(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        self.variable(false);
    }

    fn namedVariable(self: *Context, name: Token, canAssign: bool) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        var arg: u8 = undefined;
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;

        if (self.resolveLocal(self.current, name)) |v| {
            getOp = .GetLocal;
            setOp = .SetLocal;
            arg = v;
        } else if (self.resolveUpvalue(self.current, name)) |v| {
            getOp = .GetUpvalue;
            setOp = .SetUpvalue;
            arg = v;
        } else {
            getOp = .GetGlobal;
            setOp = .SetGlobal;
            arg = try self.identifierConstant(name);
        }

        if (canAssign and self.parser.match(.Equal)) {
            self.expression();
            self.emitUnaryOp(self.current, setOp, arg);
        } else {
            self.emitUnaryOp(self.current, getOp, arg);
        }
    }

    fn string(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;
        const lexeme = self.parser.previous.lexeme;
        const str = Obj.String.copy(self.vm, lexeme[0..]) catch unreachable;
        self.emitConstant(self.current, str.obj.value());
    }

    fn orFn(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;

        const elseJump = self.emitJump(.JumpIfFalse);
        const endJump = self.emitJump(.Jump);

        self.patchJump(elseJump);
        self.emitOp(self.current, .Pop);

        self.parsePrecedence(.Or);
        self.patchJump(endJump);
    }

    fn andFn(self: *Context, canAssign: bool) void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = canAssign;

        const endJump = self.emitJump(.JumpIfFalse);

        self.emitOp(self.current, .Pop);
        self.parsePrecedence(.And);

        self.patchJump(endJump);
    }

    fn emitJump(self: *Context, op: OpCode) usize {
        const t = tracy.Zone(@src());
        defer t.End();

        self.emitOp(self.current, op);
        self.emitByte(self.current, '\xFF');
        self.emitByte(self.current, '\xFF');

        return self.current.chunk().code.items.len - 2;
    }

    fn emitLoop(self: *Context, loopStart: i32) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.emitOp(self.current, .Loop);
        const count = @intCast(i32, self.current.chunk().code.items.len);
        const offset = @intCast(u16, count - loopStart + 2);
        if (offset > std.math.maxInt(u16)) self.parser.errorAtPrevious("Loop body too large.");

        self.emitByte(self.current, @truncate(u8, (offset >> 8) & 0xff));
        self.emitByte(self.current, @truncate(u8, offset & 0xff));
    }

    fn emitReturn(self: *Context) void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (self.current.T == .Initializer) {
            // An initializer automatically returns "this".
            self.emitOp(self.current, .GetLocal);
            self.emitByte(self.current, 0);
        } else {
            self.emitOp(self.current, .Nil);
        }

        self.emitOp(self.current, .Return);
    }

    fn emitByte(self: *Context, compiler: *Compiler, byte: u8) void {
        const t = tracy.Zone(@src());
        defer t.End();

        compiler.chunk().write(byte, self.parser.previous.line) catch unreachable;
    }

    fn emitOp(self: *Context, compiler: *Compiler, op: OpCode) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.emitByte(compiler, @enumToInt(op));
    }

    fn emitUnaryOp(self: *Context, compiler: *Compiler, op: OpCode, byte: u8) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.emitByte(compiler, @enumToInt(op));
        self.emitByte(compiler, byte);
    }

    fn emitConstant(self: *Context, compiler: *Compiler, value: Value) void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.emitUnaryOp(compiler, .Constant, self.makeConstant(compiler, value));
    }

    fn makeConstant(self: *Context, compiler: *Compiler, value: Value) u8 {
        const t = tracy.Zone(@src());
        defer t.End();

        //TODO: Constant limit
        return self.addConstant(compiler, value);
    }

    pub fn addConstant(self: *Context, compiler: *Compiler, value: Value) u8 {
        const t = tracy.Zone(@src());
        defer t.End();

        self.vm.push(value);
        compiler.chunk().constants.append(value) catch unreachable;
        _ = self.vm.pop();
        return @intCast(u8, compiler.chunk().constants.items.len - 1);
    }
};

const std = @import("std");

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Obj = @import("./object.zig").Obj;
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Value = @import("./value.zig").Value;
const VM = @import("./vm.zig").VM;
const NativeBinding = @import("./vm.zig").NativeBinding;

const debug = @import("./debug.zig");
const lib = @import("./lib.zig");

const Error = error{ CompileError, OutOfMemory };

pub const Builtin = enum(u8) {
    Plus,
    Minus,
    Times,
    Divide,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Break,
    Class,
    Continue,
    Const,
    Do,
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
};

const Form = struct {
    builtin: Builtin,
    name: []const u8,
    func: *const fn (*Context) Error!void,
};

fn form(
    comptime builtin: Builtin,
    comptime name: []const u8,
    comptime func: *const fn (*Context) Error!void,
) Form {
    return Form{ .builtin = builtin, .name = name, .func = func };
}

fn getForm(text: []const u8) ?Form {
    for (forms) |kw| {
        if (std.mem.eql(u8, kw.name, text)) return kw;
    }
    return null;
}

pub const forms = [_]Form{
    form(.Plus, "+", Context.parseOperator),
    form(.Minus, "-", Context.parseOperator),
    form(.Times, "*", Context.parseOperator),
    form(.Divide, "/", Context.parseOperator),
    form(.Equal, "==", Context.parseOperator),
    form(.NotEqual, "!=", Context.parseOperator),
    form(.Less, "<", Context.parseOperator),
    form(.LessEqual, "<=", Context.parseOperator),
    form(.Greater, ">", Context.parseOperator),
    form(.GreaterEqual, ">=", Context.parseOperator),
    form(.Break, "break", Context.parseBreak),
    form(.Class, "class", Context.parseClass),
    form(.Continue, "continue", Context.parseContinue),
    form(.Const, "const", Context.parseConst),
    form(.Do, "do", Context.parseDo),
    form(.False, "false", Context.parseLiteral),
    form(.Fn, "fn", Context.parseFn),
    form(.For, "for", Context.parseFor),
    form(.If, "if", Context.parseIf),
    form(.Nil, "nil", Context.parseLiteral),
    form(.Or, "or", Context.parseOr),
    form(.Print, "print", Context.parsePrint),
    form(.Return, "return", Context.parseReturn),
    form(.Super, "super", Context.parseSuper),
    form(.This, "this", Context.parseThis),
    form(.True, "true", Context.parseLiteral),
    form(.Var, "var", Context.parseVar),
    form(.With, "with", Context.parseWith),
    form(.While, "while", Context.parseWhile),
};

pub const Parser = struct {
    scanner: Scanner,
    current: Token = undefined,
    previous: Token = undefined,
    innermostLoopStart: u32 = 0,
    innermostLoopScopeDepth: u32 = 0,
    had_error: bool = false,
    had_panic: bool = false,

    fn create(source: []const u8) Parser {
        return Parser{ .scanner = Scanner.create(source) };
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (self.previous.token_type != .EOF) {
            self.current = self.scanner.scanToken();
            if (self.current.token_type != .Error) break;
            self.errorAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) void {
        if (self.current.token_type == token_type) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn match(self: *Parser, token_type: TokenType) bool {
        if (!self.check(token_type)) return false;
        self.advance();
        return true;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        return self.current.token_type == token_type;
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

        std.debug.print("[offset {}] Error", .{token.index});
        switch (token.token_type) {
            .EOF => std.debug.print(" at end", .{}),
            .Error => {},
            else => std.debug.print(" at '{s}'", .{token.lexeme}),
        }
        std.debug.print(": {s}\n", .{message});

        self.had_error = true;
    }
};

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
        context.current = self;

        self.function.name = switch (self.T) {
            .Initializer, .Method, .Static, .Function => try Obj.String.copy(context.vm, context.parser.previous.lexeme),
            .Abstract => unreachable,
            .TopLevel => null,
        };

        try self.locals.append(Local{
            .depth = @as(i32, @intCast(self.scope_depth)),
            .isCaptured = false,
            .name = Token.symbol(if (self.T == .Function) "" else "this"),
        });
    }

    pub fn deinit(self: *Compiler) void {
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
    groupStack: std.ArrayList(TokenType) = undefined,

    pub fn compile(self: *Context, vm: *VM, source: []const u8) !*Obj.Function {
        var parser = Parser.create(source);

        var compiler = try Compiler.create(vm, null, 0, .TopLevel);
        try compiler.init(self);
        defer compiler.deinit();

        self.groupStack = std.ArrayList(TokenType).init(vm.allocator);
        defer self.groupStack.deinit();

        self.vm = vm;
        self.parser = &parser;
        self.current = &compiler;

        self.parser.advance();

        while (!self.parser.match(.EOF))
            try self.parseExpression();

        const func = self.end();

        std.debug.assert(self.groupStack.items.len == 0);

        return if (self.parser.had_error) error.CompileError else func;
    }

    fn end(self: *Context) !*Obj.Function {
        try self.emitReturn();

        if (self.parser.had_error)
            return error.CompileError;

        const func = self.current.function;

        if (self.current.enclosing) |outer| {
            const arg = try self.makeConstant(outer, func.obj.value());
            // Capture the upvalues in the new closure object.
            try self.emitUnaryOp(outer, .Closure, arg);

            // Emit arguments for each upvalue to know whether to capture a local or an upvalue.
            var i: usize = 0;
            while (i < func.upvalueCount) : (i += 1) {
                try self.emitByte(outer, if (self.current.upvalues.items[i].isLocal) 1 else 0);
                try self.emitByte(outer, self.current.upvalues.items[i].index);
            }
        }

        self.current = self.current.enclosing orelse undefined;

        if (debug.trace_compiler) {
            const name = if (func.name) |n| n.bytes else "Chunk";
            func.chunk.disassemble(name);
        }

        return func;
    }

    fn parseExpression(self: *Context) !void {
        self.parser.advance();
        switch (self.parser.previous.token_type) {
            .LeftParen => try self.parseParenExpr(),
            .RightParen => {},
            .LeftBracket => try self.parseBracketExpr(),
            .RightBracket => {},
            .LeftBrace => try self.parseBraceExpr(),
            .RightBrace => {},
            .Quote => unreachable,
            .Quasiquote => unreachable,
            .Unquote => unreachable,
            .Splice => unreachable,
            .Identifier => try self.parseIdentifierExpr(),
            .Number => try self.parseNumber(),
            .String => try self.string(),
            .True, .False, .Nil => try self.parseLiteral(),
            .Error => return error.CompileError,
            .EOF => {},
        }
    }

    fn parseParenExpr(self: *Context) Error!void {
        try self.beginGrouping(.RightParen);
        const curr = self.parser.current;
        if (getForm(curr.lexeme)) |f| {
            self.parser.advance();
            try f.func(self);
        } else {
            var arg_count: u8 = 0;
            try self.parseExpression();
            while (!self.parser.check(.RightParen)) {
                arg_count += 1;
                try self.parseExpression();
                if (arg_count == 32) self.parser.errorAtPrevious("Cannot have more than 32 arguments.");
            }

            try self.emitOp(self.current, .Call);
            try self.emitByte(self.current, arg_count);
        }

        try self.endGrouping(.RightParen, "Expect ')' after arguments.");
    }

    fn parseBracketExpr(self: *Context) Error!void {
        try self.beginGrouping(.RightBracket);
        var expr_count: u8 = 0;
        try self.emitOp(self.current, .NewList);
        try self.parseExpression();
        try self.emitOp(self.current, .AddList);
        while (!self.parser.check(.RightBracket)) {
            expr_count += 1;
            try self.parseExpression();
            try self.emitOp(self.current, .AddList);
        }
        try self.endGrouping(.RightBracket, "Expect ']' after arguments.");
    }

    fn parseBraceExpr(self: *Context) Error!void {
        try self.beginGrouping(.RightBrace);
        var expr_count: u8 = 0;
        try self.parseExpression();
        while (!self.parser.check(.RightBrace)) {
            expr_count += 1;
            try self.parseExpression();
            try self.emitOp(self.current, .Swap);
            try self.parseExpression();
            try self.emitOp(self.current, .Call);
            try self.emitByte(self.current, 2);
        }
        try self.endGrouping(.RightBrace, "Expect '}' after arguments.");
    }

    fn parseIdentifierExpr(self: *Context) Error!void {
        if (getForm(self.parser.previous.lexeme)) |f| {
            try f.func(self);
        } else {
            try self.parseVariable();
        }
    }

    pub fn beginGrouping(self: *Context, token_type: TokenType) !void {
        try self.groupStack.append(token_type);
    }

    pub fn endGrouping(self: *Context, token_type: TokenType, message: []const u8) !void {
        if (self.groupStack.getLast() == token_type) {
            _ = self.groupStack.pop();
            self.parser.consume(token_type, message);
        } else {
            return error.CompileError;
        }
    }

    fn parseOperator(self: *Context) Error!void {
        const special = getForm(self.parser.previous.lexeme) orelse unreachable;
        const op = switch (special.builtin) {
            .Plus => OpCode.Add,
            .Minus => OpCode.Subtract,
            .Times => OpCode.Multiply,
            .Divide => OpCode.Divide,
            .Equal => OpCode.Equal,
            .NotEqual => OpCode.NotEqual,
            .Less => OpCode.Less,
            .LessEqual => OpCode.LessEqual,
            .Greater => OpCode.Greater,
            .GreaterEqual => OpCode.GreaterEqual,
            else => unreachable,
        };
        try self.parseExpression();
        while (!self.parser.check(.RightParen)) {
            try self.parseExpression();
            try self.emitOp(self.current, op);
        }
    }

    fn parsePrint(self: *Context) Error!void {
        try self.parseExpression();
        try self.emitOp(self.current, .Print);
    }

    fn parseIf(self: *Context) Error!void {
        try self.parseExpression();

        const thenJump = try self.emitJump(OpCode.JumpIfFalse);
        try self.emitOp(self.current, .Pop);

        try self.parseExpression();

        const elseJump = try self.emitJump(.Jump);

        self.patchJump(thenJump);
        try self.emitOp(self.current, .Pop);

        if (!self.parser.check(.RightParen))
            try self.parseExpression();

        self.patchJump(elseJump);
    }

    fn parseReturn(self: *Context) Error!void {
        if (self.current.T == .TopLevel) {
            self.parser.errorAtPrevious("Cannot return from top-level code.");
        }

        if (self.parser.match(.RightParen)) {
            try self.emitReturn();
        } else {
            if (self.current.T == .Initializer) {
                self.parser.errorAtPrevious("Cannot return from an initializer");
            }
            try self.parseExpression();
            self.parser.consume(.RightParen, "Expected ')' after return");
            try self.emitOp(self.current, .Return);
        }
    }

    fn patchJump(self: *Context, offset: usize) void {
        const jump = self.current.chunk().code.items.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            self.parser.errorAtPrevious("Too much code to jump over");
        }
        self.current.chunk().code.items[offset + 0] = @as(u8, @truncate((jump >> 8) & 0xff));
        self.current.chunk().code.items[offset + 1] = @as(u8, @truncate(jump & 0xff));
    }

    fn parseWhile(self: *Context) Error!void {
        self.current.loop_depth += 1;

        const surroundingLoopStart = self.parser.innermostLoopStart;
        const surroundingLoopScopeDepth = self.parser.innermostLoopScopeDepth;
        self.parser.innermostLoopStart = @as(u32, @intCast(self.current.chunk().code.items.len));
        self.parser.innermostLoopScopeDepth = self.current.scope_depth;

        try self.parseExpression();

        const exitJump = try self.emitJump(.JumpIfFalse);
        try self.emitOp(self.current, .Pop);

        try self.parseExpression();

        try self.emitLoop(@intCast(self.parser.innermostLoopStart));

        self.patchJump(exitJump);
        try self.emitOp(self.current, .Pop);

        self.parser.consume(.RightParen, "Expect ')' after condition.");

        self.parser.innermostLoopStart = surroundingLoopStart;
        self.parser.innermostLoopScopeDepth = surroundingLoopScopeDepth;

        self.current.loop_depth -= 1;
    }

    fn beginScope(self: *Context) void {
        self.current.scope_depth += 1;
    }

    fn endScope(self: *Context) Error!void {
        self.current.scope_depth -= 1;
        while (self.current.locals.popOrNull()) |l| {
            if (l.depth <= self.current.scope_depth) break;
            if (l.isCaptured) {
                try self.emitOp(self.current, .CloseUpvalue);
            } else {
                try self.emitOp(self.current, .Pop);
            }
        }
    }

    fn block(self: *Context) !void {
        while (!(self.parser.check(.RightParen) or self.parser.check(.EOF))) {
            try self.parseExpression();
        }
    }

    fn parseClass(self: *Context) Error!void {
        self.parser.consume(.Identifier, "Expect class name.");

        const nameConstant = try self.identifierConstant(self.parser.previous);
        try self.declareVariable();

        try self.emitUnaryOp(self.current, .Class, nameConstant);
        try self.defineVariable(nameConstant);

        var classCompiler = ClassCompiler{
            .name = self.parser.previous,
            .hasSuperclass = false,
            .enclosing = self.currentClass,
        };

        self.currentClass = &classCompiler;
        defer self.currentClass = self.currentClass.?.enclosing;

        if (self.parser.match(.LeftBracket)) {
            self.parser.consume(.Identifier, "Expect superclass name");

            try self.parseVariable();

            if (identifiersEqual(classCompiler.name, self.parser.previous)) {
                self.parser.errorAtPrevious("Class cannot inherit from itself.");
                return error.CompileError;
            }

            self.beginScope();
            try self.addLocal(Token.symbol("super"));
            try self.defineVariable(0);

            try self.getVariable(classCompiler.name);
            try self.emitOp(self.current, .Inherit);
            classCompiler.hasSuperclass = true;

            self.parser.consume(.RightBracket, "Expect ']' after superlist.");
        }

        while (!self.parser.check(.RightParen) and !self.parser.check(.EOF)) {
            self.parser.consume(.LeftParen, "Expect declaration");
            if (getForm(self.parser.current.lexeme)) |f| {
                self.parser.advance();
                switch (f.builtin) {
                    .Fn => try self.parseMethod(),
                    .Var => try self.parseVar(),
                    else => return error.CompileError,
                }
            } else {
                return error.CompileError;
            }
        }

        if (classCompiler.hasSuperclass) try self.endScope();
    }

    fn parseMethod(self: *Context) Error!void {
        self.parser.consume(.Identifier, "Expect method name.");
        const constant = try self.identifierConstant(self.parser.previous);

        try self.function(.Method);
        try self.emitUnaryOp(self.current, .Method, constant);
        try self.emitOp(self.current, .Pop);

        self.parser.consume(.RightParen, "Expect closing ')' in method declaration");
    }

    fn parseInit(self: *Context) Error!void {
        self.parser.consume(.Identifier, "Expect initializer name.");
        const constant = try self.identifierConstant(self.parser.previous);

        try self.function(.Initializer);
        try self.emitUnaryOp(self.current, .Initializer, constant);
        self.parser.consume(.RightParen, "Expect closing ')' in method declaration");
    }

    fn parseFn(self: *Context) Error!void {
        const global = try self.parseVariableDeclaration("Expect function name.");
        self.markInitialized();
        try self.function(.Function);
        try self.defineVariable(global);
    }

    fn function(self: *Context, fn_type: FunctionType) !void {
        var compiler = try Compiler.create(self.vm, self.current, self.current.scope_depth, fn_type);

        try compiler.init(self);
        defer compiler.deinit();

        self.current = &compiler;
        self.current.function.name = try Obj.String.copy(self.vm, self.parser.previous.lexeme);

        self.beginScope();
        self.parser.consume(.LeftBracket, "Expect '[' after function name.");

        // Parameters
        while (!self.parser.check(.RightBracket)) {
            if (self.current.function.arity == 32) {
                self.parser.errorAtCurrent("Cannot have more than 32 parameters.");
            }
            self.current.function.arity += 1;
            const paramConstant = try self.parseVariableDeclaration("Expect parameter name.");
            try self.defineVariable(paramConstant);
        }

        self.parser.consume(.RightBracket, "Expect ']' after parameters.");

        // Body
        try self.block();
        // Function Object
        try self.endScope();

        _ = try self.end(); // TODO: Handle static
    }

    fn parseWith(_: *Context) Error!void {}
    fn parseBreak(_: *Context) Error!void {}
    fn parseContinue(_: *Context) Error!void {}
    fn parseFor(_: *Context) Error!void {}

    fn parseDo(self: *Context) Error!void {
        try self.parseExpression();
        while (!self.parser.match(.RightParen)) {
            try self.emitOp(self.current, .Pop);
            try self.parseExpression();
        }
    }

    fn parseConst(self: *Context) Error!void {
        const global = try self.parseVariableDeclaration("Expect variable name.");
        try self.parseExpression();
        try self.defineVariable(global);
    }

    fn parseVar(self: *Context) Error!void {
        const global = try self.parseVariableDeclaration("Expect variable name.");
        try self.parseExpression();
        try self.defineVariable(global);
    }

    fn parseVariableDeclaration(self: *Context, errorMessage: []const u8) !u8 {
        self.parser.consume(.Identifier, errorMessage);
        try self.declareVariable();
        if (self.current.scope_depth > 0) return 0;
        return try self.identifierConstant(self.parser.previous);
    }

    fn declareVariable(self: *Context) Error!void {
        // Global variables are implicitly declared.
        if (self.current.scope_depth == 0) return;

        const name = self.parser.previous;

        var i = self.current.locals.items.len - 1;
        while (i >= 0) : (i -= 1) {
            const local_index = @as(u8, @intCast(i));
            const local = &self.current.locals.items[local_index];
            if (local.depth != -1 and local.depth < self.current.scope_depth) break;
            if (identifiersEqual(name, local.name)) {
                self.parser.errorAtPrevious("Cannot redeclare variable in this scope.");
            }
        }

        try self.addLocal(name);
    }

    fn identifiersEqual(a: Token, b: Token) bool {
        return std.mem.eql(u8, a.lexeme, b.lexeme);
    }

    fn identifierConstant(self: *Context, name: Token) Error!u8 {
        const obj_string = try Obj.String.copy(self.vm, name.lexeme);
        return self.makeConstant(self.current, obj_string.obj.value());
    }

    fn markInitialized(self: *Context) void {
        if (self.current.scope_depth == 0) return;
        const local_index = @as(u8, @intCast(self.current.locals.items.len - 1));
        self.current.locals.items[local_index].depth = @as(i32, @intCast(self.current.scope_depth));
    }

    fn defineVariable(self: *Context, global: u8) Error!void {
        if (self.current.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        try self.emitUnaryOp(self.current, .DefineGlobal, global);
    }

    fn addLocal(self: *Context, name: Token) !void {
        if (self.current.locals.items.len == 255) {
            self.parser.errorAtPrevious("Too many local variables in function.");
            return;
        }

        try self.current.locals.append(.{ .name = name, .depth = -1, .isCaptured = false });
    }

    fn resolveLocal(self: *Context, compiler: *Compiler, name: Token) ?u8 {
        var i: i32 = @as(i32, @intCast(compiler.locals.items.len)) - 1;
        while (i >= 0) : (i -= 1) {
            const local_index = @as(u8, @intCast(i));
            const local = &compiler.locals.items[local_index];
            if (identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    self.parser.errorAtPrevious("Cannot read local variable in its own initializer.");
                }
                return @as(u8, @intCast(i));
            }
        }
        return null;
    }

    fn addUpvalue(self: *Context, compiler: *Compiler, index: u8, isLocal: bool) !u8 {
        for (compiler.upvalues.items, 0..) |u, i| {
            if (u.index == index and u.isLocal == isLocal) {
                return @as(u8, @intCast(i));
            }
        }

        if (compiler.upvalues.items.len == std.math.maxInt(u8)) {
            self.parser.errorAtPrevious("Too many closure variables in function.");
            return 0;
        }

        try compiler.upvalues.append(.{ .isLocal = isLocal, .index = index });
        compiler.function.upvalueCount += 1;

        return @as(u8, @intCast(compiler.upvalues.items.len - 1));
    }

    fn resolveUpvalue(self: *Context, compiler: *Compiler, name: Token) ?u8 {
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

    fn parseNumber(self: *Context) Error!void {
        const value = std.fmt.parseFloat(f64, self.parser.previous.lexeme) catch return error.CompileError;
        try self.emitConstant(self.current, Value.fromNumber(value));
    }

    fn parseLiteral(self: *Context) Error!void {
        switch (self.parser.previous.token_type) {
            .Nil => try self.emitOp(self.current, .Nil),
            .False => try self.emitOp(self.current, .False),
            .True => try self.emitOp(self.current, .True),
            else => return error.CompileError,
        }
    }

    fn parseVariable(self: *Context) Error!void {
        try self.getVariable(self.parser.previous);
    }

    fn parseSuper(self: *Context) Error!void {
        if (self.currentClass) |class| {
            if (!class.hasSuperclass) {
                self.parser.errorAtPrevious("Cannot use 'super' in a class with no superclass.");
            }
        } else {
            self.parser.errorAtPrevious("Cannot use 'super' outside of a class.");
        }

        self.parser.consume(.Identifier, "Expect superclass method name");
        const name = try self.identifierConstant(self.parser.previous);

        try self.getVariable(Token.symbol("this"));
        if (self.parser.match(.LeftParen)) {
            try self.parseExpression();
            var arg_count: u8 = 0;
            while (!self.parser.check(.RightParen)) {
                arg_count += 1;
                try self.parseExpression();
                if (arg_count == 32) self.parser.errorAtPrevious("Cannot have more than 32 arguments.");
            }
            self.parser.consume(.RightParen, "Expect ')' after arguments.");
            try self.getVariable(Token.symbol("super"));
            try self.emitUnaryOp(self.current, .SuperInvoke, name);
            try self.emitByte(self.current, arg_count);
        } else {
            try self.getVariable(Token.symbol("super"));
            try self.emitUnaryOp(self.current, .GetSuper, name);
        }
    }

    fn parseThis(self: *Context) Error!void {
        try self.parseVariable();
    }

    fn setVariable(self: *Context, name: Token) !void {
        if (self.resolveLocal(self.current, name)) |v| {
            try self.emitUnaryOp(self.current, .SetLocal, v);
        } else if (self.resolveUpvalue(self.current, name)) |v| {
            try self.emitUnaryOp(self.current, .SetUpvalue, v);
        } else {
            var arg = try self.identifierConstant(name);
            try self.emitUnaryOp(self.current, .SetGlobal, arg);
        }
    }

    fn getVariable(self: *Context, name: Token) !void {
        if (self.resolveLocal(self.current, name)) |v| {
            try self.emitUnaryOp(self.current, .GetLocal, v);
        } else if (self.resolveUpvalue(self.current, name)) |v| {
            try self.emitUnaryOp(self.current, .GetUpvalue, v);
        } else {
            var arg = try self.identifierConstant(name);
            try self.emitUnaryOp(self.current, .GetGlobal, arg);
        }
    }

    fn string(self: *Context) Error!void {
        const lexeme = self.parser.previous.lexeme;
        const str = try Obj.String.copy(self.vm, lexeme[0..]);
        try self.emitConstant(self.current, str.obj.value());
    }

    fn parseOr(self: *Context) Error!void {
        const elseJump = try self.emitJump(.JumpIfFalse);
        const endJump = try self.emitJump(.Jump);

        self.patchJump(elseJump);
        try self.emitOp(self.current, .Pop);

        try self.parseExpression();
        self.patchJump(endJump);
    }

    fn parseAnd(self: *Context) Error!void {
        const endJump = try self.emitJump(.JumpIfFalse);

        try self.emitOp(self.current, .Pop);
        try self.parseExpression();

        self.patchJump(endJump);
    }

    fn emitJump(self: *Context, op: OpCode) Error!usize {
        try self.emitOp(self.current, op);
        try self.emitByte(self.current, '\xFF');
        try self.emitByte(self.current, '\xFF');

        return self.current.chunk().code.items.len - 2;
    }

    fn emitLoop(self: *Context, loopStart: i32) Error!void {
        try self.emitOp(self.current, .Loop);
        const count = @as(i32, @intCast(self.current.chunk().code.items.len));
        const offset = @as(u16, @intCast(count - loopStart + 2));
        if (offset > std.math.maxInt(u16)) self.parser.errorAtPrevious("Loop body too large.");

        try self.emitByte(self.current, @as(u8, @truncate((offset >> 8) & 0xff)));
        try self.emitByte(self.current, @as(u8, @truncate(offset & 0xff)));
    }

    fn emitReturn(self: *Context) Error!void {
        if (self.current.T == .Initializer) {
            // An initializer automatically returns "this".
            try self.emitOp(self.current, .GetLocal);
            try self.emitByte(self.current, 0);
        } else if (self.current.T == .Function) {
            try self.emitOp(self.current, .Nil);
        }

        try self.emitOp(self.current, .Return);
    }

    fn emitByte(self: *Context, compiler: *Compiler, byte: u8) Error!void {
        try compiler.chunk().write(byte, self.parser.previous.index);
    }

    fn emitOp(self: *Context, compiler: *Compiler, op: OpCode) Error!void {
        try self.emitByte(compiler, @intFromEnum(op));
    }

    fn emitUnaryOp(self: *Context, compiler: *Compiler, op: OpCode, byte: u8) Error!void {
        try self.emitByte(compiler, @intFromEnum(op));
        try self.emitByte(compiler, byte);
    }

    fn emitConstant(self: *Context, compiler: *Compiler, value: Value) Error!void {
        const arg = try self.makeConstant(compiler, value);
        try self.emitUnaryOp(compiler, .Constant, arg);
    }

    fn makeConstant(self: *Context, compiler: *Compiler, value: Value) Error!u8 {
        //TODO: Constant limit
        return try self.addConstant(compiler, value);
    }

    pub fn addConstant(self: *Context, compiler: *Compiler, value: Value) Error!u8 {
        self.vm.push(value);
        try compiler.chunk().constants.append(value);
        _ = self.vm.pop();
        return @intCast(compiler.chunk().constants.items.len - 1);
    }
};

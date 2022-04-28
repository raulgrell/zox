const std = @import("std");
const Allocator = std.mem.Allocator;

const FixedCapacityStack = @import("./stack.zig").FixedCapacityStack;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const ValueType = @import("./value.zig").ValueType;
const Context = @import("./compiler.zig").Context;
const Obj = @import("./object.zig").Obj;
const GCAllocator = @import("./memory.zig").GCAllocator;

const config = @import("./config.zig");
const debug = @import("./debug.zig");
const tracy = @import("./tracy.zig");

pub const CallFrame = struct {
    closure: *Obj.Closure,
    ip: [*]u8,
    slots: u32,

    pub fn readByte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub fn readShort(self: *CallFrame) u16 {
        const value = @intCast(u16, self.ip[0]) << 8 | self.ip[1];
        self.ip += 2;
        return value;
    }

    pub fn readConstant(self: *CallFrame) Value {
        const chunk = self.currentChunk();
        return chunk.constants.items[self.readByte()];
    }

    pub fn readString(self: *CallFrame) *Obj.String {
        const chunk = self.currentChunk();
        return chunk.constants.items[self.readByte()].asObjType(.String);
    }

    pub fn currentChunk(self: CallFrame) *Chunk {
        return &self.closure.function.chunk;
    }
};

pub const VM = struct {
    instance: Context,
    allocator: Allocator,

    gca: GCAllocator,
    grayStack: std.ArrayList(*Obj),

    frames: std.ArrayList(CallFrame),
    strings: std.StringHashMap(*Obj.String),
    globals: std.AutoHashMap(*Obj.String, Value),
    stack: FixedCapacityStack(Value),

    objects: ?*Obj,
    openUpvalues: ?*Obj.Upvalue,

    initString: ?*Obj.String,

    pub const RuntimeError = error{RuntimeError};

    pub fn create(allocator: Allocator) VM {
        return VM{
            .instance = Context{},
            .allocator = allocator,
            .gca = undefined,
            .grayStack = undefined,
            .frames = undefined,
            .stack = undefined,
            .strings = undefined,
            .globals = undefined,
            .openUpvalues = null,
            .objects = null,
            .initString = null,
        };
    }

    pub fn init(self: *VM) !void {
        self.stack = try FixedCapacityStack(Value).init(self.allocator, config.stackMax);
        self.grayStack = std.ArrayList(*Obj).init(self.allocator);

        self.gca = GCAllocator.init(self, self.allocator);
        self.frames = std.ArrayList(CallFrame).init(self.gca.allocator());
        self.strings = std.StringHashMap(*Obj.String).init(self.gca.allocator());
        self.globals = std.AutoHashMap(*Obj.String, Value).init(self.gca.allocator());
        self.initString = try Obj.String.copy(self, "init");
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        self.globals.deinit();
        self.strings.deinit();
        self.frames.deinit();
        self.grayStack.deinit();
        self.freeObjects();
        self.initString = null;
    }

    pub fn interpret(self: *VM, source: []const u8) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        std.debug.assert(self.stack.items.len == 0);
        defer std.debug.assert(self.stack.items.len == 0);

        var function = try self.instance.compile(self, source);
        self.push(function.obj.value());
        const closure = try Obj.Closure.create(self, function);
        _ = self.pop();

        self.push(closure.obj.value());
        try self.callValue(closure.obj.value(), 0);
        try self.run();

        _ = self.pop();
    }

    fn currentFrame(self: *VM) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack.append(value);
    }

    pub fn pop(self: *VM) Value {
        return self.stack.pop();
    }

    pub fn peek(self: *VM, distance: u32) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    pub fn tail(self: *VM, distance: u32) []Value {
        return self.stack.items[self.stack.items.len - 1 - distance ..];
    }

    fn resetStack(self: *VM) void {
        self.stack.resize(0) catch unreachable;
        self.openUpvalues = null;

        // TODO: Dictu
        // self.instance.compiler = null;
    }

    pub fn freeObjects(self: *VM) void {
        var object = self.objects;
        while (object) |o| {
            const next = o.next;
            o.destroy(self);
            object = next;
        }

        self.grayStack.deinit();
    }

    pub fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) RuntimeError {
        @setCold(true);

        std.debug.print(format, args);
        std.debug.print("\n", .{});

        var i = self.frames.items.len;
        while (i != 0) {
            i -= 1;
            const prev_frame = &self.frames.items[i];
            const function = prev_frame.closure.function;
            const offset = @ptrToInt(prev_frame.ip) - @ptrToInt(function.chunk.ptr());
            const line = function.chunk.getLine(@intCast(usize, offset));
            const name = if (function.name) |str| str.bytes else "<script>";
            std.debug.print("[ line {}] in {s}\n", .{ line, name });
        }

        self.resetStack();

        return error.RuntimeError;
    }

    fn printDebug(self: *VM) void {
        const frame = self.currentFrame();
        const chunk = frame.currentChunk();
        const instruction = @ptrToInt(frame.ip) - @ptrToInt(chunk.ptr());
        self.printStack();
        _ = chunk.disassembleInstruction(instruction);
        std.debug.print("\n", .{});
    }

    fn printStack(self: *VM) !void {
        for (self.stack.items) |v, i| std.debug.print("[ {}: {} ]", .{ i, v });
        std.debug.print("\n", .{});
    }

    pub fn print(self: *VM, value: Value) void {
        _ = self;
        std.debug.print("{}", .{value});
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (!callee.isObj()) return self.runtimeError("Can only call functions and classes.", .{});
        const o = callee.asObj();
        switch (o.objType) {
            .Class => try self.callClass(o.asClass(), arg_count),
            .BoundMethod => try self.callBoundMethod(o.asBoundMethod(), arg_count),
            .Closure => try self.callClosure(o.asClosure(), arg_count),
            .Native => try self.callNative(o.asNative(), arg_count),
            .Instance, .String, .Upvalue, .Function, .List => unreachable,
        }
    }

    fn callClass(self: *VM, class: *Obj.Class, arg_count: u8) !void {
        var instance = try Obj.Instance.create(self, class);
        self.stack.items[self.stack.items.len - arg_count - 1] = instance.obj.value();
        if (class.methods.get(self.initString.?)) |method| {
            return self.call(method.asObjType(.Closure), arg_count);
        } else if (arg_count != 0) {
            return self.runtimeError("Expected 0 arguments but got {}.", .{arg_count});
        }
    }

    fn callBoundMethod(self: *VM, method: *Obj.BoundMethod, arg_count: u8) !void {
        self.stack.items[self.stack.items.len - arg_count - 1] = method.receiver;
        return self.call(method.method, arg_count);
    }

    fn callClosure(self: *VM, closure: *Obj.Closure, arg_count: u8) !void {
        return self.call(closure, arg_count);
    }

    fn callNative(self: *VM, native: *Obj.Native, arg_count: u8) !void {
        const result = try native.function(self, self.tail(arg_count));
        self.stack.items.len -= arg_count + 1;
        self.push(result);
        return;
    }

    fn callOther(self: *VM) !void {
        return self.runtimeError("Can only call functions and classes.", .{});
    }

    fn invokeFromClass(self: *VM, class: *Obj.Class, name: *Obj.String, arg_count: u8) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const method = class.methods.get(name) orelse {
            return self.runtimeError("Cannot invoke undefined property {s} in class {s}.", .{
                name.bytes,
                class.name.bytes,
            });
        };

        return self.call(method.asObj().asClosure(), arg_count);
    }

    fn invoke(self: *VM, name: *Obj.String, arg_count: u8) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const receiver = self.peek(arg_count);

        if (!receiver.isObjType(.Instance)) {
            return self.runtimeError("Only instances have methods.", .{});
        }

        const instance = receiver.asObjType(.Instance);
        if (instance.fields.get(name)) |value| {
            self.stack.items[self.stack.items.len - arg_count - 1] = value;
            return self.callValue(value, arg_count);
        }

        return self.invokeFromClass(instance.class, name, arg_count);
    }

    fn bindMethod(self: *VM, class: *Obj.Class, name: *Obj.String) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const method = class.methods.get(name) orelse {
            return self.runtimeError("Cannot bind undefined property {s} in class {s}.", .{
                name.bytes,
                class.name.bytes,
            });
        };

        const bound = try Obj.BoundMethod.create(self, class.obj.value(), method.asObj().asClosure());
        _ = self.pop();
        self.push(bound.obj.value());
    }

    fn captureUpvalue(self: *VM, local: *Value) !*Obj.Upvalue {
        const t = tracy.Zone(@src());
        defer t.End();

        if (self.openUpvalues) |o| {
            var prevUpvalue: ?*Obj.Upvalue = null;
            var upvalue: ?*Obj.Upvalue = o;
            while (upvalue) |u| {
                if (@ptrToInt(u.location) > @ptrToInt(local)) {
                    prevUpvalue = u;
                    upvalue = u.next;
                } else break;
            }

            if (upvalue) |u| if (u.location == local) return u;

            var createdUpvalue = try Obj.Upvalue.create(self, local, upvalue);
            if (prevUpvalue) |p| {
                p.next = createdUpvalue;
            } else {
                self.openUpvalues = createdUpvalue;
            }

            return createdUpvalue;
        } else {
            self.openUpvalues = try Obj.Upvalue.create(self, local, null);
            return self.openUpvalues.?;
        }
    }

    fn closeUpvalues(self: *VM, last: *Value) void {
        const t = tracy.Zone(@src());
        defer t.End();

        while (self.openUpvalues) |u| {
            if (@ptrToInt(u.location) >= @ptrToInt(last)) {
                u.closed = u.location.*;
                u.location = &u.closed;
                self.openUpvalues = u.next;
            } else return;
        }
    }

    fn call(self: *VM, closure: *Obj.Closure, arg_count: u8) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (arg_count != closure.function.arity) {
            return self.runtimeError("Expected {} arguments but got {}.", .{
                closure.function.arity,
                arg_count,
            });
        }

        try self.frames.append(CallFrame{
            .closure = closure,
            .ip = closure.function.chunk.ptr(),
            .slots = @intCast(u32, self.stack.items.len) - arg_count - 1,
        });
    }

    pub fn defineNative(self: *VM, name: []const u8, function: Obj.Native.Fn) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const string = try Obj.String.copy(self, name);
        const native = try Obj.Native.create(self, string, function);
        self.push(string.obj.value());
        self.push(native.obj.value());
        _ = try self.globals.put(string, self.peek(0));
        _ = self.pop();
        _ = self.pop();
    }

    fn defineMethod(self: *VM, name: *Obj.String) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const method = self.peek(0);
        var class = self.peek(1).asObjType(.Class);
        _ = try class.methods.put(name, method);
        _ = self.pop();
    }

    fn createClass(self: *VM, name: *Obj.String, superclass: ?*Obj.Class) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const class = try Obj.Class.create(self, name, superclass);
        self.push(class.obj.value());

        // Inherit methods.
        if (superclass) |s| {
            var it = s.methods.iterator();
            while (it.next()) |e| {
                var slot = try class.methods.getOrPut(e.key_ptr.*);
                // TODO: Review copying
                slot.value_ptr.* = e.value_ptr.*;
            }
        }
    }

    fn concatenate(self: *VM) !void {
        const a = self.peek(1).asObjType(.String);
        const b = self.peek(0).asObjType(.String);

        const length = a.bytes.len + b.bytes.len;
        var bytes = try self.allocator.alloc(u8, length);
        std.mem.copy(u8, bytes[0..a.bytes.len], a.bytes);
        std.mem.copy(u8, bytes[a.bytes.len..], b.bytes);

        const result = try Obj.String.take(self, bytes);

        _ = self.pop();
        _ = self.pop();

        self.push(result.obj.value());
    }

    fn run(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        while (true) {
            comptime if (debug.trace_vm) self.printDebug();
            const op = @intToEnum(OpCode, self.currentFrame().readByte());
            try self.runOp(op);
            if (op == .Return and self.frames.items.len == 0) break;
        }
    }

    fn runOp(self: *VM, op: OpCode) !void {
        switch (op) {
            .Nil => try runNil(self),
            .True => try runTrue(self),
            .False => try runFalse(self),
            .Pop => try runPop(self),
            .Constant => try runConstant(self),
            .GetLocal => try runGetLocal(self),
            .SetLocal => try runSetLocal(self),
            .DefineGlobal => try runDefineGlobal(self),
            .GetGlobal => try runGetGlobal(self),
            .SetGlobal => try runSetGlobal(self),
            .GetUpvalue => try runGetUpvalue(self),
            .SetUpvalue => try runSetUpvalue(self),
            .GetSuper => try runGetSuper(self),
            .Equal => try runEqual(self),
            .NotEqual => try runNotEqual(self),
            .Add, .Subtract, .Multiply, .Divide => |o| try runBinaryMath(self, o),
            .Greater, .Less, .GreaterEqual, .LessEqual => |o| try runBinaryComparison(self, o),
            .And, .Or => |o| try runBinaryBool(self, o),
            .Not => try runNot(self),
            .Negate => try runNegate(self),
            .Print => try runPrint(self),
            .NewList => try runNewList(self),
            .AddList => try runAddList(self),
            .Subscript => try runSubscript(self),
            .SubscriptAssign => try runSubscriptAssign(self),
            .JumpIfFalse => try runJumpIfFalse(self),
            .Jump => try runJump(self),
            .Loop => try runLoop(self),
            .Call => try runCall(self),
            .Invoke => try runInvoke(self),
            .SuperInvoke => try runSuperInvoke(self),
            .Closure => try runClosure(self),
            .CloseUpvalue => try runCloseUpvalue(self),
            .Return => try runReturn(self),
            .Class => try runClass(self),
            // .Subclass => try runSubclass(self), ,
            .Inherit => try runInherit(self),
            .Method => try runMethod(self),
            .GetProperty => try runGetProperty(self),
            .SetProperty => try runSetProperty(self),
        }
    }

    fn runNil(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.push(Value.nil());
    }

    fn runTrue(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.push(Value.fromBool(true));
    }

    fn runFalse(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.push(Value.fromBool(false));
    }

    fn runPop(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        _ = self.pop();
    }

    fn runConstant(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const constant = self.currentFrame().readConstant();
        self.push(constant);
    }

    fn runGetLocal(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const slot = self.currentFrame().readByte();
        const local = self.stack.items[self.currentFrame().slots + slot];
        self.push(local);
    }

    fn runSetLocal(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const slot = self.currentFrame().readByte();
        const value = self.peek(0);
        self.stack.items[self.currentFrame().slots + slot] = value;
    }

    fn runDefineGlobal(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const name = self.currentFrame().readString();
        const value = self.peek(0);
        _ = try self.globals.put(name, value);
        _ = self.pop();
    }

    fn runGetGlobal(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const name = self.currentFrame().readString();
        var value = self.globals.get(name) orelse {
            return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
        };
        self.push(value);
    }

    fn runSetGlobal(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const name = self.currentFrame().readString();
        const value = self.peek(0);
        _ = self.globals.put(name, value) catch {
            return self.runtimeError("Undefined variable '{s}'.", .{name.bytes});
        };
    }

    fn runGetUpvalue(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const slot = self.currentFrame().readByte();
        self.push(self.currentFrame().closure.upvalues[slot].?.location.*);
    }

    fn runSetUpvalue(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const slot = self.currentFrame().readByte();
        self.currentFrame().closure.upvalues[slot].?.location.* = self.peek(0);
    }

    fn runGetSuper(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const name = self.currentFrame().readString();
        const superclass = self.pop();
        try self.bindMethod(superclass.asObjType(.Class), name);
    }

    fn runEqual(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const b = self.pop();
        const a = self.pop();
        self.push(Value.fromBool(a.equals(b)));
    }

    fn runNotEqual(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const b = self.pop();
        const a = self.pop();
        self.push(Value.fromBool(!a.equals(b)));
    }

    fn runBinaryMath(self: *VM, op: OpCode) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (!self.peek(1).isNumber() or !self.peek(0).isNumber()) {
            return self.runtimeError("Operands must be numbers", .{});
        }

        const rhs = self.pop().asNumber();
        const lhs = self.pop().asNumber();

        const number = switch (op) {
            .Add => lhs + rhs,
            .Subtract => lhs - rhs,
            .Multiply => lhs * rhs,
            .Divide => lhs / rhs,
            else => unreachable,
        };

        self.push(Value.fromNumber(number));
    }

    fn runBinaryComparison(self: *VM, op: OpCode) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (!self.peek(1).isNumber() or !self.peek(0).isNumber()) {
            return self.runtimeError("Operands must be numbers", .{});
        }

        const rhs = self.pop().asNumber();
        const lhs = self.pop().asNumber();

        const result = switch (op) {
            .Less => lhs < rhs,
            .LessEqual => lhs <= rhs,
            .Greater => lhs > rhs,
            .GreaterEqual => lhs >= rhs,
            else => unreachable,
        };
        self.push(Value.fromBool(result));
    }

    fn runBinaryBool(self: *VM, op: OpCode) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (!self.peek(1).isBool() or !self.peek(0).isBool()) {
            return self.runtimeError("Operands must be boolean", .{});
        }
        const rhs = self.pop().asBool();
        const lhs = self.pop().asBool();
        const result = switch (op) {
            .And => lhs and rhs,
            .Or => lhs or rhs,
            else => unreachable,
        };
        self.push(Value.fromBool(result));
    }

    fn runNot(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const value = Value.fromBool(self.pop().isFalsey());
        self.push(value);
    }

    fn runNegate(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        if (!self.peek(0).isNumber())
            return self.runtimeError("Operand must be a number.", .{});

        const number = self.pop().asNumber();
        self.push(Value.fromNumber(-number));
    }

    fn runPrint(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const stdout = std.io.getStdOut().writer();
        try stdout.print("{any}\n", .{self.pop()});
    }

    fn runNewList(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const list = try Obj.List.create(self);
        self.push(list.obj.value());
    }

    fn runAddList(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const itemValue = self.pop();
        const listValue = self.pop();

        const list = listValue.asObjType(.List);
        try list.items.append(itemValue);

        self.push(list.obj.value());
    }

    fn runSubscript(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const indexValue = self.pop();
        const listValue = self.pop();

        if (!indexValue.isNumber()) {
            return self.runtimeError("List index must be a number", .{});
        }

        const list = listValue.asObjType(.List);
        const index = indexValue.asInteger();
        const value = list.items.items[index];

        self.push(value);
    }

    fn runSubscriptAssign(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const listValue = self.peek(2);
        if (!listValue.isObj()) {
            return self.runtimeError("Can only subscript lists.", .{});
        }

        const indexValue = self.peek(1);
        if (!indexValue.isNumber()) {
            return self.runtimeError("List index must be a number.", .{});
        }

        const assignValue = self.peek(0);

        const list = listValue.asObjType(.List);
        const index = indexValue.asInteger();

        if (index >= 0 and index < list.items.items.len) {
            list.items.items[index] = assignValue;
            self.push(Value.nil());
        } else {
            _ = self.pop();
            _ = self.pop();
            _ = self.pop();

            self.push(Value.nil());

            return self.runtimeError("List index out of bounds.", .{});
        }
    }

    fn runJumpIfFalse(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const offset = self.currentFrame().readShort();
        if (self.peek(0).isFalsey()) self.currentFrame().ip += offset;
    }

    fn runJump(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const offset = self.currentFrame().readShort();
        self.currentFrame().ip += offset;
    }

    fn runLoop(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const offset = self.currentFrame().readShort();
        self.currentFrame().ip -= offset;
    }

    fn runCall(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const arg_count = self.currentFrame().readByte();
        try self.callValue(self.peek(arg_count), arg_count);
    }

    fn runInvoke(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const method = self.currentFrame().readString();
        const arg_count = self.currentFrame().readByte();
        self.invoke(method, arg_count) catch |err| {
            return self.runtimeError("Could not invoke method - {}", .{err});
        };
    }

    fn runSuperInvoke(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const method = self.currentFrame().readString();
        const arg_count = self.currentFrame().readByte();
        const superclass = self.pop().asObjType(.Class);
        try self.invokeFromClass(superclass, method, arg_count);
    }

    fn runClosure(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const function = self.currentFrame().readConstant().asObj().asFunction();
        var closure = try Obj.Closure.create(self, function);
        self.push(closure.obj.value());
        for (closure.upvalues) |*u| {
            const isLocal = self.currentFrame().readByte() != 0;
            const index = self.currentFrame().readByte();
            if (isLocal) {
                u.* = try self.captureUpvalue(&self.stack.items[self.currentFrame().slots + index]);
            } else {
                u.* = self.currentFrame().closure.upvalues[index];
            }
        }
    }

    fn runCloseUpvalue(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        self.closeUpvalues(&self.stack.items[self.stack.items.len - 2]);
        _ = self.pop();
    }

    fn runReturn(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const result = self.pop();
        const frame = self.frames.pop();

        self.closeUpvalues(&self.stack.items[frame.slots]);

        if (self.frames.items.len == 0)
            return;

        try self.stack.resize(frame.slots);
        self.push(result);
    }

    fn runClass(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        try self.createClass(self.currentFrame().readString(), null);
    }

    fn runSubclass(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const superclass = self.peek(0);
        if (superclass.Obj.data != .Class) {
            return self.runtimeError("Superclass must be a class", .{});
        }
        try self.createClass(self.currentFrame().readString(), &superclass.Obj.data.Class);
    }

    fn runInherit(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const superclassObject = self.peek(1);
        if (!superclassObject.isObjType(.Class))
            return self.runtimeError("Superclass must be a class.", .{});

        const superclass = superclassObject.asObjType(.Class);
        const subclass = self.peek(0).asObjType(.Class);

        var method_it = superclass.methods.iterator();
        while (method_it.next()) |entry| {
            // TODO: Review copying
            try subclass.methods.put(entry.key_ptr.*, entry.value_ptr.*);
        }

        _ = self.pop();
    }

    fn runMethod(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        try self.defineMethod(self.currentFrame().readString());
    }

    fn runGetProperty(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const val = self.peek(0);
        if (!val.isObjType(.Instance)) {
            return self.runtimeError("Only instances have properties.", .{});
        }

        const instance = val.asObj().asInstance();
        const name = self.currentFrame().readString();

        if (instance.fields.get(name)) |value| {
            _ = self.pop(); // Instance
            self.push(value);
        } else {
            try self.bindMethod(instance.class, name);
        }
    }

    fn runSetProperty(self: *VM) !void {
        const t = tracy.Zone(@src());
        defer t.End();

        const val = self.peek(1);
        if (!val.isObjType(.Instance)) {
            return self.runtimeError("Only instances have properties.", .{});
        }

        var instance = val.asObj().asInstance();
        const name = self.currentFrame().readString();

        _ = try instance.fields.put(name, self.peek(0));

        const value = self.pop();
        _ = self.pop();
        self.push(value);
    }
};

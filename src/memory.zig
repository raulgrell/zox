const std = @import("std");
const VM = @import("./vm.zig").VM;
const Compiler = @import("./compiler.zig").Compiler;
const debug = @import("./debug.zig");
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const Obj = @import("./object.zig").Obj;
const Table = @import("./table.zig").Table;

pub const GCAllocator = struct {
    vm: *VM,
    parent_allocator: Allocator,
    bytesAllocated: usize,
    nextGC: usize,

    const Self = @This();
    const HEAP_GROW_FACTOR = 2;

    pub fn init(vm: *VM, parent_allocator: Allocator) GCAllocator {
        return .{
            .vm = vm,
            .parent_allocator = parent_allocator,
            .bytesAllocated = 0,
            .nextGC = 1024 * 1024,
        };
    }

    pub fn allocator(self: *Self) Allocator {
        return Allocator.init(self, alloc, resize, free);
    }

    fn alloc(self: *Self, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) error{OutOfMemory}![]u8 {
        if ((self.bytesAllocated + len > self.nextGC) or debug.stress_gc) {
            try self.collectGarbage();
        }
        var out = try self.parent_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
        self.bytesAllocated += out.len;
        return out;
    }

    fn resize(self: *Self, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
        if (new_len > buf.len) {
            if ((self.bytesAllocated + (new_len - buf.len) > self.nextGC) or debug.stress_gc) {
                self.collectGarbage() catch {
                    return null;
                };
            }
        }

        if (self.parent_allocator.rawResize(buf, buf_align, new_len, len_align, ret_addr)) |resized_len| {
            if (resized_len > buf.len) {
                self.bytesAllocated += resized_len - buf.len;
            } else {
                self.bytesAllocated -= buf.len - resized_len;
            }
            return resized_len;
        } else {
            return null;
        }
    }

    fn free(
        self: *Self,
        buf: []u8,
        buf_align: u29,
        ret_addr: usize,
    ) void {
        self.parent_allocator.rawFree(buf, buf_align, ret_addr);
        self.bytesAllocated -= buf.len;
    }

    fn collectGarbage(self: *Self) !void {
        if (debug.trace_gc) {
            std.debug.print("-- gc begin\n", .{});
        }

        try self.markRoots();
        try self.traceReferences();
        self.removeUnreferencedStrings();
        self.sweep();

        self.nextGC = self.bytesAllocated * HEAP_GROW_FACTOR;

        if (debug.trace_gc) {
            std.debug.print("-- gc end\n", .{});
        }
    }

    fn markRoots(self: *Self) !void {
        for (self.vm.stack.items) |value| {
            try self.markValue(value);
        }

        for (self.vm.frames.items) |frame| {
            try self.markObject(&frame.closure.obj);
        }

        var maybeUpvalue = self.vm.openUpvalues;
        while (maybeUpvalue) |upvalue| {
            try self.markObject(&upvalue.obj);
            maybeUpvalue = upvalue.next;
        }

        try self.markTable(&self.vm.globals);
        try self.markCompilerRoots();
        if (self.vm.initString) |initString| try self.markObject(&initString.obj);
    }

    fn traceReferences(self: *Self) !void {
        while (self.vm.grayStack.items.len > 0) {
            const obj = self.vm.grayStack.pop();
            try self.blackenObject(obj);
        }
    }

    fn removeUnreferencedStrings(self: *Self) void {
        var it = self.vm.strings.iterator();
        while (it.next()) |e| {
            if (!e.value_ptr.*.obj.isMarked) {
                _ = self.vm.strings.remove(e.key_ptr.*);
            }
        }
    }

    fn sweep(self: *Self) void {
        var previous: ?*Obj = null;
        var maybeObject = self.vm.objects;
        while (maybeObject) |object| {
            if (object.isMarked) {
                object.isMarked = false;
                previous = object;
                maybeObject = object.next;
            } else {
                const unreached = object;
                maybeObject = object.next;
                if (previous) |p| {
                    p.next = maybeObject;
                } else {
                    self.vm.objects = maybeObject;
                }

                unreached.destroy(self.vm);
            }
        }
    }

    fn blackenObject(self: *Self, obj: *Obj) !void {
        if (debug.trace_gc) {
            std.debug.print("{} blacken {}\n", .{ @ptrToInt(obj), obj.value() });
        }

        switch (obj.objType) {
            .Upvalue => try self.markValue(obj.asUpvalue().closed),
            .Function => {
                const function = obj.asFunction();
                if (function.name) |name| try self.markObject(&name.obj);
                try self.markArray(function.chunk.constants.items);
            },
            .Closure => {
                const closure = obj.asClosure();
                try self.markObject(&closure.function.obj);
                for (closure.upvalues) |maybeUpvalue| {
                    if (maybeUpvalue) |upvalue| {
                        try self.markObject(&upvalue.obj);
                    }
                }
            },
            .Class => {
                const class = obj.asClass();
                try self.markObject(&class.name.obj);
                try self.markTable(&class.methods);
            },
            .Instance => {
                const instance = obj.asInstance();
                try self.markObject(&instance.class.obj);
                try self.markTable(&instance.fields);
            },
            .BoundMethod => {
                const bound = obj.asBoundMethod();
                try self.markValue(bound.receiver);
                try self.markObject(&bound.method.obj);
            },
            .Native, .String => {},
            .List => unreachable,
        }
    }

    fn markArray(self: *Self, values: []Value) !void {
        for (values) |value| try self.markValue(value);
    }

    fn markValue(self: *Self, value: Value) !void {
        if (value.isObj()) try self.markObject(value.asObj());
    }

    fn markObject(self: *Self, obj: *Obj) !void {
        if (obj.isMarked) return;

        if (debug.trace_gc) {
            std.debug.print("{} mark {}\n", .{ @ptrToInt(obj), obj.value() });
        }

        obj.isMarked = true;

        try self.vm.grayStack.append(obj);
    }

    fn markTable(self: *Self, table: *std.AutoHashMap(*Obj.String, Value)) !void {
        var it = table.iterator();
        while (it.next()) |entry| {
            try self.markObject(&entry.key_ptr.*.obj);
            try self.markValue(entry.value_ptr.*);
        }
    }

    fn markCompilerRoots(self: *Self) !void {
        _ = self;
        var maybeCompiler: ?*Compiler = self.vm.instance.current;
        while (maybeCompiler) |compiler| {
            try self.markObject(&compiler.function.obj);
            maybeCompiler = compiler.enclosing;
        }
    }
};

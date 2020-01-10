const std = @import("std");
const allocator = @import("root").allocator;

const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;
const ValueType = @import("value.zig").ValueType;
const Chunk = @import("chunk.zig").Chunk;

const growth_factor_gc = 2;
const verbose_gc = false;
const stress_gc = true;

fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

extern var vm: VM;

pub const ObjType = enum {
    Instance,
    Class,
    Closure,
    Function,
    Native,
    String,
    Upvalue,
};

pub const ObjString = struct {
    bytes: []const u8,
    hash: u32,

    pub fn copy(bytes: []const u8) *Obj {
        const hash = hashFn(bytes);

        const interned = vm.strings.get(bytes);
        if (interned) |s| return s.value;

        const heapChars = allocator.alloc(u8, bytes.len) catch unreachable;
        std.mem.copy(u8, heapChars, bytes);

        return allocate(heapChars, hash);
    }

    pub fn take(bytes: []const u8) *Obj {
        const hash = hashFn(bytes);

        const interned = vm.strings.get(bytes);
        if (interned) |s| {
            allocator.free(bytes);
            return s.value;
        }

        return allocate(bytes, hash);
    }

    pub fn allocate(bytes: []const u8, hash: u32) *Obj {
        const string = Obj.allocate();
        string.data = Obj.Data{
            .String = ObjString{
                .bytes = bytes,
                .hash = hash,
            },
        };
        vm.push(string.value());
        _ = vm.strings.put(bytes, string) catch unreachable;
        _ = vm.pop();
        return string;
    }

    pub fn eqlFn(a: []const u8, b: []const u8) bool {
        return std.mem.eql(u8, a, b);
    }

    pub fn hashFn(key: []const u8) u32 {
        var hash: u32 = 2166136261;
        var i: u32 = 0;
        while (i < key.len) : (i += 1) {
            hash ^= key[i];
            hash *%= 16777619;
        }
        return hash;
    }
};

pub const ObjUpvalue = struct {
    location: *Value,
    closed: Value,
    next: ?*Obj,

    pub fn allocate(slot: *Value) *Obj {
        var upvalue = Obj.allocate();
        upvalue.data = Obj.Data{
            .Upvalue = ObjUpvalue{
                .closed = Value.Nil,
                .location = slot,
                .next = null,
            },
        };
        return upvalue;
    }
};

pub const ObjClosure = struct {
    function: *ObjFunction,
    upvalues: []*Obj,

    pub fn allocate(function: *ObjFunction) *Obj {
        var upvalues = allocator.alloc(*Obj, function.upvalueCount) catch unreachable;
        const closure = Obj.allocate();
        closure.data = Obj.Data{
            .Closure = ObjClosure{
                .function = function,
                .upvalues = upvalues,
            },
        };
        return closure;
    }
};

pub const ObjClass = struct {
    name: *ObjString,

    pub fn allocate(name: *ObjString) *Obj {
        const closure = Obj.allocate();
        closure.data = Obj.Data{
            .Class = ObjClass{
                .name = name,
            },
        };
        return closure;
    }
};

pub const ObjInstance = struct {
    class: *ObjClass,
    fields: std.HashMap([]const u8, Value, ObjString.hashFn, ObjString.eqlFn),

    pub fn allocate(class: *ObjClass) *Obj {
        const instance = Obj.allocate();
        instance.data = Obj.Data{
            .Instance = ObjInstance{
                .class = class,
                .fields = std.HashMap([]const u8, Value, ObjString.hashFn, ObjString.eqlFn).init(allocator),
            },
        };
        return instance;
    }
};

pub const ObjFunction = struct {
    arity: u8,
    upvalueCount: u8,
    chunk: Chunk,
    name: ?*Obj,

    pub fn allocate() *Obj {
        const function = Obj.allocate();
        function.data = Obj.Data{
            .Function = ObjFunction{
                .arity = 0,
                .upvalueCount = 0,
                .chunk = Chunk.init(),
                .name = null,
            },
        };
        return function;
    }
};

pub const ObjNative = struct {
    function: NativeFn,

    pub const NativeFn = fn (args: []Value) Value;

    pub fn allocate(function: NativeFn) *Obj {
        const func = Obj.allocate();
        func.data = Obj.Data{
            .Native = ObjNative{ .function = function },
        };
        return func;
    }
};

pub const Obj = struct {
    data: Data,
    isMarked: bool,
    next: ?*Obj,

    pub const Data = union(ObjType) {
        Instance: ObjInstance,
        Class: ObjClass,
        Upvalue: ObjUpvalue,
        String: ObjString,
        Function: ObjFunction,
        Closure: ObjClosure,
        Native: ObjNative,
    };

    pub fn value(self: *Obj) Value {
        return Value{ .Obj = self };
    }

    pub fn toString(self: Obj) []const u8 {
        switch (self.data) {
            .Instance => |i| return i.class.name.bytes,
            .Class => |c| return c.name.bytes,
            .Upvalue => |u| return "Upvalue",
            .Closure => |c| return "Closure",
            .Function => |f| return if (f.name) |n| n.data.String.bytes else "Function",
            .Native => |n| return "Native",
            .String => |s| return s.bytes,
        }
    }

    fn equal(self: *const Obj, other: *const Obj) bool {
        switch (self.data) {
            .Upvalue, .Closure, .Function, .Native, .String, .Instance, .Class => return self == other,
        }
    }

    pub fn allocate() *Obj {
        var object = create(Obj);
        object.* = Obj{
            .data = undefined,
            .isMarked = false,
            .next = vm.objects,
        };
        vm.objects = object;

        if (verbose_gc) {
            std.debug.warn("{} allocate object\n", .{@ptrToInt(object)});
        }

        return object;
    }

    pub fn free(self: *Obj) void {
        if (verbose_gc) {
            std.debug.warn("{} free type {}\n", .{ @ptrToInt(self), @tagName(self.data) });
        }

        switch (self.data) {
            .Instance => |i| {
                i.fields.deinit();
                allocator.destroy(self);
            },
            .Class => |c| allocator.destroy(self),
            .Upvalue => |u| allocator.destroy(self),
            .Function => |f| {
                if (f.name) |n| allocator.free(n.data.String.bytes);
                allocator.destroy(self);
            },
            .Native => {
                allocator.destroy(self);
            },
            .String => |s| {
                allocator.free(s.bytes);
                allocator.destroy(self);
            },
            .Closure => |c| {
                allocator.free(c.upvalues);
                allocator.destroy(self);
            },
        }
    }
};

pub fn create(comptime T: type) *T {
    return &reallocate(T, null, 0, 1).?[0];
}

pub fn alloc(comptime T: type, count: usize) []T {
    return reallocate(T, null, 0, count);
}

pub fn dealloc(comptime T: type, pointer: *T) void {
    reallocate(T, pointer, 1, 0);
}

pub fn destroy(comptime T: type, pointer: []T) void {
    reallocate(T, pointer, pointer.len, 0);
}

pub fn reallocate(comptime T: type, previous: ?[]T, oldSize: usize, newSize: usize) ?[]T {
    vm.bytesAllocated += newSize - oldSize;

    if (stress_gc and newSize > oldSize) {
        collectGarbage();
    }

    if (vm.bytesAllocated > vm.nextGC) {
        collectGarbage();
    }

    if (previous) |p| {
        if (newSize == 0) {
            allocator.free(p);
            return null;
        }
        return allocator.alloc(T, newSize) catch null;
    } else {
        return allocator.alloc(T, newSize) catch null;
    }
}

pub fn collectGarbage() void {
    var before: usize = undefined;
    if (verbose_gc) {
        std.debug.warn("-- gc begin\n", .{});
        before = vm.bytesAllocated;
    }

    markRoots();
    traceReferences();
    tableRemoveWhite();
    sweep();

    vm.nextGC = vm.bytesAllocated * growth_factor_gc;

    if (verbose_gc) {
        std.debug.warn("-- gc end\n", .{});
        std.debug.warn("   collected {} bytes (from {} to {}) next at {}\n", .{ before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC });
    }
}

pub fn markRoots() void {
    for (vm.stack.toSlice()) |*v| markValue(v);

    var i: usize = 0;
    while (i < vm.frame_count) : (i += 1) {
        // markObject(vm.frames[i].closure);
    }

    var upvalue: ?*Obj = vm.openUpvalues;
    while (upvalue) |u| : (upvalue = u.next) {
        markObject(u);
    }

    markTable();
}

pub fn markValue(value: *Value) void {
    switch (value.*) {
        .Obj => |o| markObject(o),
        else => return,
    }
}

pub fn markObject(object: *Obj) void {
    if (object.isMarked) return;

    if (verbose_gc) {
        std.debug.warn("{} mark {}\n", .{ @ptrToInt(object), @tagName(object.data) });
    }

    object.isMarked = true;

    if (vm.grayStack.?.len < vm.grayCount + 1) {
        const capacity = growCapacity(vm.grayStack.?.len);
        vm.grayStack = allocator.realloc(vm.grayStack.?, capacity) catch unreachable;
    }

    vm.grayStack.?[vm.grayCount] = object;
    vm.grayCount += 1;
}

pub fn markTable() void {
    var i: usize = 0;
    while (i < vm.strings.entries.len) : (i += 1) {
        var entry = &vm.strings.entries[i];
        // markObject(entry.key);
        // markValue(entry.value);
    }
}

pub fn markCompilerRoots() void {
    const compiler = current;
    while (compiler != null) {
        markObject(compiler.function);
        compiler = compiler.enclosing;
    }
}

pub fn traceReferences() void {
    while (vm.grayCount > 0) {
        vm.grayCount -= 1;
        var object: *Obj = vm.grayStack.?[vm.grayCount];
        blackenObject(object);
    }
}

pub fn blackenObject(object: *Obj) void {
    if (verbose_gc) {
        std.debug.warn("{} blacken {}\n", .{ @ptrToInt(object), @tagName(object.data) });
    }

    switch (object.data) {
        .Instance => |i| {
            //markObject(i.class);
            //markTable(&instance.fields);
        },
        .Class => |c| {},
        .Closure => |c| {
            markObject(object);
            for (object.data.Closure.upvalues) |u| markObject(u);
        },
        .Upvalue => |u| {
            markValue(&object.data.Upvalue.closed);
        },
        .Function => |f| {
            if (f.name) |n| markObject(n);
            for (f.chunk.constants.toSlice()) |*c| markValue(c);
        },
        .Native, .String => {},
    }
}

pub fn markArray(array: *ValueArray) void {}

pub fn sweep() void {
    var previous: ?*Obj = null;
    var object: ?*Obj = vm.objects;
    while (object) |o| {
        if (o.isMarked) {
            o.isMarked = false;
            previous = o;
            object = o.next;
        } else {
            var unreached: *Obj = o;
            object = o.next;
            if (previous) |p| {
                p.next = object;
            } else {
                vm.objects = object;
            }
            allocator.destroy(unreached);
        }
    }
}

pub fn tableRemoveWhite() void {
    var i: usize = 0;
    while (i < vm.strings.entries.len) : (i += 1) {
        const entry = &vm.strings.entries[i];
        // if (entry.kv.key != "" and !entry.key.obj.isMarked) {
        //     tableDelete(table, entry.key);
        // }
    }
}

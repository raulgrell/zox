const std = @import("std");
const allocator = @import("root").allocator;

const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;
const ValueType = @import("value.zig").ValueType;
const Chunk = @import("chunk.zig").Chunk;
const Compiler = @import("compiler.zig").Compiler;

const growth_factor_gc = 2;
const verbose_gc = false;
const stress_gc = true;

fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

extern var vm: VM;


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
        BoundMethod: ObjBoundMethod,
    };

    pub fn value(self: *Obj) Value {
        return Value{ .Obj = self };
    }

    pub fn toString(self: Obj) []const u8 {
        switch (self.data) {
            .Instance => |i| return i.class.name.bytes,
            .BoundMethod => |m| return m.method.function.name.?.data.String.bytes,
            .Class => |c| return c.name.bytes,
            .Upvalue => |u| return "Upvalue",
            .Closure => |c| return "Closure",
            .Function => |f| return if (f.name) |n| n.data.String.bytes else "Function",
            .Native => |n| return "Native",
            .String => |s| return s.bytes,
        }
    }

    pub fn equal(self: *const Obj, other: *const Obj) bool {
        switch (self.data) {
            .BoundMethod, .Upvalue, .Closure, .Function, .Native, .String, .Instance, .Class => return self == other,
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
            .Instance => |*i| {
                i.fields.deinit();
                allocator.destroy(self);
            },
            .BoundMethod => |m| allocator.destroy(self),
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
    for (vm.stack.items) |*v| markValue(v);

    var i: usize = 0;
    while (i < vm.frame_count) : (i += 1) {
        // markObject(vm.frames[i].closure);
    }

    var upvalue: ?*Obj = vm.openUpvalues;
    while (upvalue) |u| : (upvalue = u.next) {
        // markObject(u);
    }

    markTable();
    markCompilerRoots();
    // markObject(vm.initString.?);
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
    // var i: usize = 0;
    // while (i < vm.strings.entries.len) : (i += 1) {
    //     var entry = &vm.strings.entries[i];
    //     markObject(entry.key);
    //     markValue(entry.value);
    // }
}

pub fn markCompilerRoots() void {
    var compiler: ?*Compiler = vm.instance.current;
    // while (compiler) |c| {
    //     markObject(c.function);
    //     compiler = c.enclosing;
    // }
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
        .BoundMethod => |m| {},
        .Class => |c| {},
        .Closure => |c| {
            markObject(object);
            //for (object.data.Closure.upvalues) |u| markObject(u);
        },
        .Upvalue => |u| {
            markValue(&object.data.Upvalue.closed);
        },
        .Function => |f| {
            if (f.name) |n| markObject(n);
            for (f.chunk.constants.items) |*c| markValue(c);
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
    // var i: usize = 0;
    // while (i < vm.strings.entries.len) : (i += 1) {
    //     const entry = &vm.strings.entries[i];
    //     if (entry.kv.key != "" and !entry.key.obj.isMarked) {
    //         tableDelete(table, entry.key);
    //     }
    // }
}

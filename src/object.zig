const std = @import("std");
const allocator = @import("root").allocator;

const VM = @import("vm.zig").VM;
const Value = @import("value.zig").Value;
const ValueType = @import("value.zig").ValueType;
const Chunk = @import("chunk.zig").Chunk;
const Compiler = @import("compiler.zig").Compiler;

const verbose_gc = false;
const stress_gc = true;

pub const Obj = struct {
    objType: Type,
    next: ?*Obj,
    isMarked: bool,

    pub const Type = enum {
        Instance,
        Class,
        Closure,
        Function,
        BoundMethod,
        Native,
        List,
        String,
        Upvalue,
    };

    pub fn create(vm: *VM, comptime T: type, objType: Type) !*Obj {
        const ptr = try vm.allocator.create(T);
        ptr.obj = Obj{
            .next = vm.objects,
            .objType = objType,
            .isMarked = false,
        };

        vm.objects = &ptr.obj;

        if (verbose_gc) {
            std.debug.warn("{} allocate {} for {}\n", .{ @ptrToInt(&ptr.obj), @sizeOf(T), @typeName(T) });
        }

        return &ptr.obj;
    }

    pub fn destroy(self: *Obj, vm: *VM) void {
        if (verbose_gc) {
            std.debug.warn("{} free {} {}\n", .{ @ptrToInt(self), self.objType, self.value() });
        }

        switch (self.objType) {
            .String => self.asString().destroy(vm),
            .List => self.asList().destroy(vm),
            .Function => self.asFunction().destroy(vm),
            .Native => self.asNative().destroy(vm),
            .Closure => self.asClosure().destroy(vm),
            .Upvalue => self.asUpvalue().destroy(vm),
            .Class => self.asClass().destroy(vm),
            .Instance => self.asInstance().destroy(vm),
            .BoundMethod => self.asBoundMethod().destroy(vm),
        }
    }

    pub fn is(self: *Obj, objType: ObjType) bool {
        return self.objType == objType;
    }

    pub fn asString(self: *Obj) *String {
        return @fieldParentPtr(String, "obj", self);
    }

    pub fn asList(self: *Obj) *List {
        return @fieldParentPtr(List, "obj", self);
    }

    pub fn asFunction(self: *Obj) *Function {
        return @fieldParentPtr(Function, "obj", self);
    }

    pub fn asNative(self: *Obj) *Native {
        return @fieldParentPtr(Native, "obj", self);
    }

    pub fn asClosure(self: *Obj) *Closure {
        return @fieldParentPtr(Closure, "obj", self);
    }

    pub fn asUpvalue(self: *Obj) *Upvalue {
        return @fieldParentPtr(Upvalue, "obj", self);
    }

    pub fn asClass(self: *Obj) *Class {
        return @fieldParentPtr(Class, "obj", self);
    }

    pub fn asInstance(self: *Obj) *Instance {
        return @fieldParentPtr(Instance, "obj", self);
    }

    pub fn asBoundMethod(self: *Obj) *BoundMethod {
        return @fieldParentPtr(BoundMethod, "obj", self);
    }

    pub fn value(self: *Obj) Value {
        return Value.fromObj(self);
    }

    pub fn toString(self: *Obj) []const u8 {
        switch (self.objType) {
            .Instance => {
                return self.asInstance().class.name.bytes;
            },
            .BoundMethod => {
                return self.asBoundMethod().method.function.name.?.bytes;
            },
            .Class => {
                return self.asClass().name.bytes;
            },
            .Upvalue => {
                return "Upvalue";
            },
            .Closure => {
                return "Closure";
            },
            .Function => {
                return if (self.asFunction().name) |n| n.bytes else "Function";
            },
            .Native => {
                return "Native";
            },
            .String => {
                return self.asString().bytes;
            },
        }
    }

    pub fn equal(self: *const Obj, other: *const Obj) bool {
        switch (self.objType) {
            .BoundMethod, .Upvalue, .Closure, .Function, .Native, .String, .Instance, .Class => return self == other,
        }
    }

    pub const String = struct {
        obj: Obj,
        bytes: []const u8,

        pub fn allocate(vm: *VM, bytes: []const u8) !*String {
            const obj = try Obj.create(vm, String, .String);
            const string = obj.asString();
            string.bytes = bytes;

            // Make sure string is visible to the GC during allocation
            vm.push(obj.value());
            _ = vm.strings.put(bytes, string) catch unreachable;
            _ = vm.pop();
            return string;
        }

        pub fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.bytes);
            vm.allocator.destroy(self);
        }

        pub fn copy(vm: *VM, bytes: []const u8) !*String {
            const interned = vm.strings.get(bytes);
            if (interned) |s| return s;

            const heapChars = allocator.alloc(u8, bytes.len) catch unreachable;
            std.mem.copy(u8, heapChars, bytes);

            return allocate(vm, heapChars);
        }

        pub fn take(vm: *VM, bytes: []const u8) !*String {
            const interned = vm.strings.get(bytes);
            if (interned) |s| {
                allocator.free(bytes);
                return s;
            }

            return allocate(vm, bytes);
        }
    };

    pub const Upvalue = struct {
        obj: Obj,
        location: *Value,
        closed: Value,
        next: ?*Upvalue,

        pub fn create(vm: *VM, location: *Value, next: ?*Upvalue) !*Upvalue {
            const obj = try Obj.create(vm, Upvalue, .Upvalue);
            const upvalue = obj.asUpvalue();
            upvalue.* = Upvalue{
                .obj = obj.*,
                .location = location,
                .closed = Value.nil(),
                .next = null,
            };

            return upvalue;
        }

        pub fn destroy(self: *Upvalue, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Closure = struct {
        obj: Obj,
        function: *Function,
        upvalues: []?*Upvalue,

        pub fn create(vm: *VM, function: *Function) !*Closure {
            const upvalues = try vm.allocator.alloc(?*Upvalue, function.upvalueCount);
            for (upvalues) |*upvalue| upvalue.* = null;

            const obj = try Obj.create(vm, Closure, .Closure);
            const closure = obj.asClosure();
            closure.* = Closure{
                .obj = obj.*,
                .function = function,
                .upvalues = upvalues,
            };

            return closure;
        }

        pub fn destroy(self: *Closure, vm: *VM) void {
            vm.allocator.free(self.upvalues);
            vm.allocator.destroy(self);
        }
    };

    pub const Class = struct {
        obj: Obj,
        name: *String,
        super: ?*Class,
        methods: std.StringHashMap(Value),

        pub fn create(vm: *VM, name: *String, superclass: ?*Class) !*Class {
            const obj = try Obj.create(vm, Class, .Class);
            const class = obj.asClass();
            class.* = Class{
                .obj = obj.*,
                .name = name,
                .super = superclass,
                .methods = std.StringHashMap(Value).init(vm.allocator),
            };

            return class;
        }

        pub fn destroy(self: *Class, vm: *VM) void {
            self.methods.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const Instance = struct {
        obj: Obj,
        class: *Class,
        fields: std.StringHashMap(Value),

        pub fn create(vm: *VM, class: *Class) !*Instance {
            const obj = try Obj.create(vm, Instance, .Instance);
            const instance = obj.asInstance();
            instance.* = Instance{
                .obj = obj.*,
                .class = class,
                .fields = std.StringHashMap(Value).init(vm.allocator),
            };

            return instance;
        }

        pub fn destroy(self: *Instance, vm: *VM) void {
            self.fields.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const BoundMethod = struct {
        obj: Obj,
        receiver: Value,
        method: *Closure,

        pub fn create(vm: *VM, receiver: Value, method: *Closure) !*BoundMethod {
            const obj = try Obj.create(vm, BoundMethod, .BoundMethod);
            const bound = obj.asBoundMethod();
            bound.* = BoundMethod{
                .obj = obj.*,
                .receiver = receiver,
                .method = method,
            };

            return bound;
        }

        pub fn destroy(self: *BoundMethod, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Function = struct {
        obj: Obj,
        arity: u8,
        upvalueCount: u8,
        chunk: Chunk,
        name: ?*Obj.String,

        pub fn create(vm: *VM) !*Function {
            const obj = try Obj.create(vm, Function, .Function);
            const func = obj.asFunction();
            func.* = Function{
                .obj = obj.*,
                .arity = 0,
                .upvalueCount = 0,
                .name = null,
                .chunk = Chunk.init(),
            };

            return func;
        }

        pub fn destroy(self: *Function, vm: *VM) void {
            self.chunk.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const Native = struct {
        obj: Obj,
        function: Fn,

        pub const Fn = fn (args: []Value) Value;

        pub fn create(vm: *VM, function: Fn) !*Native {
            const obj = try Obj.create(vm, Native, .Native);
            const native = obj.asNative();
            native.* = Native{
                .obj = obj.*,
                .function = function,
            };
            return native;
        }

        pub fn destroy(self: *Native, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };

    pub const List = struct {
        obj: Obj,
        items: std.ArrayList(Value),


        pub fn create(vm: *VM) !*List {
            const obj = try Obj.create(vm, List, .List);
            const list = obj.asList();
            list.* = List{
                .obj = obj.*,
                .items =  std.ArrayList(Value).init(allocator),
            };
            return list;
        }

        pub fn destroy(self: *List, vm: *VM) void {
            vm.allocator.destroy(self);
        }
    };
};

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

const std = @import("std");
const VM = @import("../vm.zig").VM;
const Value = @import("../vm.zig").Value;
const Obj = @import("../object.zig").Obj;

pub fn toString(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("toString() takes no arguments ({} given)", .{args.len});
    }

    const valueString = args[0].asObjType(.Class);
    const string = Obj.String.copy(vm, valueString);

    // free(valueString);

    return Value.fromObj(string);
}

pub fn methods(vm: *VM, args: []Value) Value {
    if (args.len != 0) return vm.runtimeError("methods() takes no arguments ({} given)", .{args.len});

    const class = args[0].asObjType(.Class);
    const list = Obj.List.create(vm);
    // Push to stack to avoid GC
    vm.push(Value.fromObj(list));

    var i = 0;
    while (i <= class.publicMethods.capacityMask) : (i += 1) {
        if (class.publicMethods.entries[i].key == null) continue;
        list.items.append(Value.fromObj(class.publicMethods.entries[i].key));
    }

    _ = vm.pop();

    return Value.fromObj(list);
}

pub fn setup(vm: *VM) void {
    vm.defineNative(&vm.classMethods, "toString", toString);
    vm.defineNative(&vm.classMethods, "methods", methods);
}

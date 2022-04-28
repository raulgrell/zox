const std = @import("std");
const VM = @import("../vm.zig").VM;
const Value = @import("../vm.zig").Value;
const Obj = @import("../object.zig").Obj;

fn toString(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("toString() takes no arguments ({} given)", .{args.len});
    }

    const valueString = args[0];
    const string = Obj.String.copy(vm, valueString);
    // free(valueString);

    return Value.fromObj(string);
}

fn isInstance(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("isInstance() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.Class)) {
        return vm.runtimeError("Argument passed to isInstance() must be a class");
    }

    const object = args[0].asObjType(.Instance);
    const class = args[1].asObjType(.Class);
    const classToFind = object.class;

    while (classToFind != null) {
        if (class == classToFind) {
            return Value.fromBool(true);
        }

        classToFind = classToFind.superclass;
    }

    return Value.fromBool(false);
}

fn copy(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("copy() takes no arguments ({} given)", .{args.len});
    }

    const oldInstance = args[0].asObjType(.Instance);
    const instance = Obj.Instance.copy(vm, oldInstance);

    return Value.fromObj(instance);
}

fn methods(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("methods() takes no arguments ({} given)", .{args.len});
    }

    const instance = args[0].asObjType(.Instance);

    const list = Obj.List.create(vm);
    vm.push(Value.fromObj(list));

    var i = 0;
    while (i <= instance.class.publicMethods.capacityMask) : (i += 1) {
        if (instance.class.publicMethods.entries[i].key == null) {
            continue;
        }

        list.items.append(Value.fromObj(instance.class.publicMethods.entries[i].key));
    }
    _ = vm.pop();

    return Value.fromObj(list);
}

fn declareInstanceMethods(vm: *VM) void {
    vm.defineNative(&vm.instanceMethods, "toString", toString);
    vm.defineNative(&vm.instanceMethods, "isInstance", isInstance);
    vm.defineNative(&vm.instanceMethods, "copy", copy);
    vm.defineNative(&vm.instanceMethods, "methods", methods);
}

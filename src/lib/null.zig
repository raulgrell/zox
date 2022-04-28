const std = @import("std");
const VM = @import("../vm.zig").VM;
const Value = @import("../vm.zig").Value;
const Obj = @import("../object.zig").Obj;

fn toString(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("toString() takes no arguments ({} given)", .{args.len});
    }

    return Obj.String.copy(vm, "nil");
}

fn declareNilMethods(vm: *VM) void {
    vm.defineNative(&vm.nilMethods, "toString", toString);
}
const std = @import("std");
const VM = @import("../vm.zig").VM;
const Value = @import("../vm.zig").Value;
const Obj = @import("../object.zig").Obj;

fn toString(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("toString() takes no arguments ({} given)", .{args.len});
    }

    const number = args[0].asNumber();

    _ = number;

    // Impl
    const numberString = "";

    return Value.fromObj(Obj.String.take(vm, numberString));
}

fn declareNumberMethods(vm: *VM) void {
    vm.defineNative(&vm.numberMethods, "toString", toString);
}

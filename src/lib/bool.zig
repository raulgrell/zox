const std = @import("std");
const VM = @import("../vm.zig").VM;
const Value = @import("../vm.zig").Value;
const Obj = @import("../object.zig").Obj;

pub fn toString(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("bool.toString takes no arguments ({} given)", .{args.len});
    }

    const val = Value.asBool(args[0]);
    const string = try Obj.String.copy(vm, if (val) "true" else "false");
    return Value.fromObj(string.obj);
}

pub fn setupModule(vm: *VM) !void {
    vm.defineNative(.Bool, "toString", toString);
}

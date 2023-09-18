const std = @import("std");
const VM = @import("../vm.zig").VM;
const NativeBinding = @import("../vm.zig").NativeBinding;
const Value = @import("../value.zig").Value;
const Obj = @import("../object.zig").Obj;

const natives = [_]NativeBinding{};

pub fn defineAllNatives(vm: *VM) !void {
    for (natives) |n| try vm.defineNative(n.name, n.function);
}

pub fn toString(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("bool.toString takes 0 arguments ({} given)", .{args.len});
    }

    const val = Value.asBool(args[0]);
    const string = try Obj.String.copy(vm, if (val) "true" else "false");
    return Value.fromObj(string.obj);
}

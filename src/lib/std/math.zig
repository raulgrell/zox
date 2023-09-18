const std = @import("std");
const VM = @import("../../vm.zig").VM;
const NativeBinding = @import("../../vm.zig").NativeBinding;
const Value = @import("../../value.zig").Value;
const Obj = @import("../../object.zig").Obj;

const natives = [_]NativeBinding{};

pub fn defineAllNatives(vm: *VM) !void {
    for (natives) |n| try vm.defineNative(n.name, n.function);
}

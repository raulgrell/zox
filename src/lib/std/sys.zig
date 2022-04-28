const std = @import("std");
const VM = @import("../../vm.zig").VM;
const Value = @import("../../value.zig").Value;
const Obj = @import("../../object.zig").Obj;

pub fn clock(vm: *VM, args: []Value) VM.RuntimeError!Value {
    if (args.len != 1) {
        return vm.runtimeError("clock takes 0 arguments ({} given)", .{args.len - 1});
    }
    return Value.fromNumber(@intToFloat(f64, std.time.milliTimestamp()) / 1000);
}

// TODO - make into builtin
pub fn assert(vm: *VM, args: []Value) Value {
    if (args.len == 0 or !args[0].isBool() or args.len > 1) {
        return vm.runtimeError("assert() takes a single boolean argument.", args.len);
    }

    if (!Value.asBool(args[0])) {
        return vm.runtimeError("assert was false", .{});
    }

    return Value.nil();
}

const natives = .{
    .{ .name = "clock", .function = clock },
    .{ .name = "assert", .function = assert },
};

fn defineAllNatives(vm: *VM) !void {
    for (natives) |n| try vm.defineNative(n.name, n.function);
}

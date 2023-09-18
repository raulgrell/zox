const std = @import("std");
const VM = @import("../vm.zig").VM;
const Value = @import("../value.zig").Value;
const Obj = @import("../object.zig").Obj;
const NativeBinding = @import("../vm.zig").NativeBinding;

pub const natives = [_]NativeBinding{
    .{ .name = "diff", .function = diff },
    .{ .name = "sum", .function = sum },
    .{ .name = "prod", .function = prod },
    .{ .name = "div", .function = div },
};

pub fn defineAllNatives(vm: *VM) !void {
    for (natives) |n| try vm.defineNative(n.name, n.function);
}

fn sum(_: *VM, args: []Value) !Value {
    var result: f64 = 0.0;
    for (args[1..]) |v| {
        result += v.asNumber();
    }
    return Value.fromNumber(result);
}
fn diff(_: *VM, args: []Value) !Value {
    var result = args[1].asNumber();
    for (args[2..]) |v| {
        result -= v.asNumber();
    }
    return Value.fromNumber(result);
}
fn prod(_: *VM, args: []Value) !Value {
    var result: f64 = 0.0;
    for (args[1..]) |v| {
        result *= v.asNumber();
    }
    return Value.fromNumber(result);
}
fn div(_: *VM, args: []Value) !Value {
    var result = args[1].asNumber();
    for (args[2..]) |v| {
        result /= v.asNumber();
    }
    return Value.fromNumber(result);
}

fn toString(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("toString() takes no arguments ({} given)", .{args.len});
    }

    const numberString = args[1].toString();
    return Value.fromObj(Obj.String.take(vm, numberString));
}

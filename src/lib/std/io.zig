const std = @import("std");
const VM = @import("../../vm.zig").VM;
const NativeBinding = @import("../../vm.zig").NativeBinding;
const Value = @import("../../value.zig").Value;
const Obj = @import("../../object.zig").Obj;

const natives = [_]NativeBinding{};

pub fn defineAllNatives(vm: *VM) !void {
    for (natives) |n| try vm.defineNative(n.name, n.function);
}

fn readInput(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("readInput expects 0 arguments ({} given)", .{args.len});
    }

    return Value.nil();
}

fn writeOutput(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("writeOutput must take arguments ({} given)", .{args.len});
    }

    for (args) |value| {
        std.io.getStdOut().write(value);
    }

    return Value.nil();
}

fn writeError(vm: *VM, args: []Value) Value {
    if (args.len == 0) {
        return vm.runtimeError("writeError must take arguments ({} given)", .{args.len});
    }

    for (args) |value| {
        std.io.getStdErr().write(value);
    }

    return Value.nil();
}

fn writeFile(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("write() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.String)) {
        return vm.runtimeError("write() argument must be a string");
    }

    const file = args[0].asObjType(.File);
    const string = args[1].asObjType(.String);

    _ = file;
    _ = string;

    return Value.fromNumber(0);
}

fn writeLineFile(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("writeLine() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.String)) {
        return vm.runtimeError("writeLine() argument must be a string");
    }

    const file = args[0].asObjType(.File);
    const string = args[1].asObjType(.String);

    _ = file;
    _ = string;

    return Value.fromNumber(0);
}

fn readAll(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("read() takes no arguments ({} given)", .{args.len});
    }

    const file = args[0].asObjType(.File);
    _ = file;

    const buffer = "";

    return Value.fromObj(Obj.String.take(vm, buffer));
}

fn readLine(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("readLine() takes no arguments ({} given)", .{args.len});
    }

    return Value.nil();
}

fn declareFileMethods(vm: *VM) void {
    vm.defineNative(&vm.fileMethods, "write", writeFile);
    vm.defineNative(&vm.fileMethods, "writeLine", writeLineFile);
    vm.defineNative(&vm.fileMethods, "read", readAll);
    vm.defineNative(&vm.fileMethods, "readLine", readLine);
}

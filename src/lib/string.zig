const std = @import("std");
const VM = @import("../vm.zig").VM;
const Value = @import("../vm.zig").Value;
const Obj = @import("../object.zig").Obj;

fn len(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("len() takes no arguments ({} given)", .{args.len});
    }

    const string = args[0].asObjType(.String);
    return Value.fromNumber(string.bytes.len);
}

fn toNumber(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("toNumber() takes no arguments ({} given).", .{args.len});
    }

    const numberString = args[0].asObjType(.String);
    _ = numberString;
    
    const number = 0;
    // Impl

    return Value.fromNumber(number);
}

fn format(vm: *VM, args: []Value) Value {
    if (args.len == 0) {
        return vm.runtimeError("format() takes at least 1 argument ({} given)", .{args.len});
    }

    const replaceStrings = Obj.String.copy(vm, []const u8, .{args.len});
    _ = replaceStrings;

    // impl
    const newStr = "";
    return Value.fromObj(Obj.String.take(vm, newStr));
}

fn split(vm: *VM, args: []Value) Value {
    if (args.len != 1 and args.len != 2) {
        return vm.runtimeError("split() takes 1 or 2 arguments ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.String)) {
        return vm.runtimeError("Argument passed to split() must be a string");
    }

    const string = args[0].asObjType(.String);
    const delimiter = args[1].asObjType(.String);

    _ = string;
    _ = delimiter;

    const list = Obj.List.create(vm);
    return Value.fromObj(list);
}

fn contains(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("contains() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.String)) {
        return vm.runtimeError("Argument passed to contains() must be a string");
    }

    const string = args[0].asObjType(.String);
    const delimiter = args[1].asObjType(.String);

    return Value.fromBool(std.mem.containsAtLeast(u8, string, 1, delimiter));
}

fn find(vm: *VM, args: []Value) Value {
    if (args.len < 1 or args.len > 2) {
        return vm.runtimeError("find() takes either 1 or 2 arguments ({} given)", .{args.len});
    }

    var index = 1;

    if (args.len == 2) {
        if (!args[2].isNumber()) {
            return vm.runtimeError("Index passed to find() must be a number");
        }

        index = args[2].asNumber();
    }

    if (!args[1].isObjType(.String)) {
        return vm.runtimeError("Substring passed to find() must be a string");
    }

    const substr = args[1].asObjType(.String);
    const string = args[0].asObjType(.String);

    _ = substr;
    _ = string;

    // Impl

    return Value.fromNumber(0);
}

fn replace(vm: *VM, args: []Value) Value {
    if (args.len != 2) {
        return vm.runtimeError("replace() takes 2 arguments ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.String) or !args[2].isObjType(.String)) {
        return vm.runtimeError("Arguments passed to replace() must be a strings");
    }

    // Pop values off the stack
    const stringValue = args[0];
    const to_replace = args[1].asObjType(.String);
    const replacer = args[2].asObjType(.String);

    _ = to_replace;
    _ = replacer;

    const string = stringValue;

    return Value.fromObj(Obj.String.take(vm, string));
}

fn lower(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("lower() takes no arguments ({} given)", .{args.len});
    }

    const string = args[0].asObjType(.String);
    const temp = Obj.String.copy(vm, string);

    // Impl

    return Value.fromObj(Obj.String.take(vm, temp));
}

fn upper(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("upper() takes no arguments ({} given)", .{args.len});
    }

    const string = args[0].asObjType(.String);
    const temp = Obj.String.copy(vm, string);

    // Impl

    return Value.fromObj(Obj.String.take(vm, temp));
}

fn startsWith(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("startsWith() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.String)) {
        return vm.runtimeError("Argument passed to startsWith() must be a string");
    }

    const string = args[0].asObjType(.String);
    const start = args[1].asObjType(.String);

    // Impl

    return Value.fromBool(std.mem.startsWith(u8, string, start));
}

fn endsWith(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("endsWith() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.String)) {
        return vm.runtimeError("Argument passed to endsWith() must be a string");
    }

    const string = args[0].asObjType(.String);
    const suffix = args[1].asObjType(.String);

    // Impl

    return Value.fromBool(std.mem.endsWith(u8, string, suffix.bytes));
}

fn leftTrim(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("leftTrim() takes no arguments ({} given)", .{args.len});
    }

    const string = args[0].asObjType(.String);
    const temp = Obj.String.copy(vm, string);

    // Impl

    return Value.fromObj(Obj.String.take(vm, temp));
}

fn rightTrim(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("rightTrim() takes no arguments ({} given)", .{args.len});
    }

    const string = args[0].asObjType(.String);
    const temp = Obj.String.copy(vm, string);

    // Impl

    return Value.fromObj(Obj.String.take(vm, temp));
}

fn trim(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("trim() takes no arguments ({} given)", .{args.len});
    }

    const string = leftTrim(vm, 0, args);
    vm.push(string);
    string = rightTrim(vm, 0, &string);
    _ = vm.pop();
    return string;
}

fn count(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("count() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isObjType(.String)) {
        return vm.runtimeError("Argument passed to count() must be a string");
    }

    // Impl

    return Value.fromNumber(count);
}

fn declareStringMethods(vm: *VM) void {
    vm.defineNative(&vm.stringMethods, "len", len);
    vm.defineNative(&vm.stringMethods, "format", format);
    vm.defineNative(&vm.stringMethods, "split", split);
    vm.defineNative(&vm.stringMethods, "contains", contains);
    vm.defineNative(&vm.stringMethods, "find", find);
    vm.defineNative(&vm.stringMethods, "replace", replace);
    vm.defineNative(&vm.stringMethods, "lower", lower);
    vm.defineNative(&vm.stringMethods, "upper", upper);
    vm.defineNative(&vm.stringMethods, "startsWith", startsWith);
    vm.defineNative(&vm.stringMethods, "endsWith", endsWith);
    vm.defineNative(&vm.stringMethods, "leftTrim", leftTrim);
    vm.defineNative(&vm.stringMethods, "rightTrim", rightTrim);
    vm.defineNative(&vm.stringMethods, "trim", trim);
    vm.defineNative(&vm.stringMethods, "count", count);
}

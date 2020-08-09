const std = @import("std");
const Value = @import("./value.zig").Value;

pub fn clockNative(args: []Value) Value {
    return Value.fromNumber(@intToFloat(f64, std.time.milliTimestamp()) / 1000);
}

pub fn assertNative(args: []Value) bool {
    if (args[0] != .Bool) {
        runtimeError("assert() only takes a boolean as an argument.", argCount);
        return false;
    }

    if (!value.Bool) {
        runtimeError("assert() was false!");
        return false;
    }

    return true;
}

fn inputNative(args: []Value) Value {
    if (argCount > 1) {
        runtimeError("input() takes either 0 or 1 argument (%d given)", argCount);
        return NULL_VAL;
    }

    if (argCount != 0) {
        const prompt = args[0];

        if (!IS_STRING(prompt)) {
            runtimeError("input() only takes a string argument");
            return NULL_VAL;
        }

        printf("%s ", AS_CSTRING(prompt));
    }

    const currentSize = 128;
    char * line = malloc(currentSize);

    if (line == NULL) {
        runtimeError("Memory error on input()");
        return NULL_VAL;
    }

    const c = EOF;
    const i = 0;

    // read line

    const input = OBJ_VAL(copyString(line, strlen(line)));
    free(line);
    return input;
}

fn printNative(args: []Value) Value {
    if (argCount == 0) {
        printf("\n");
        return NULL_VAL;
    }

    for (args) |value| {
        printValue(value);
        printf("\n");
    }

    return NULL_VAL;
}

fn errorNative(args: []Value) Value {
    if (argCount == 0) {
        return NULL_VAL;
    }

    runtimeError(AS_CSTRING(args[0]));
    exit(70);

    return NULL_VAL;
}

const NativeBinding = struct {
    name: []const u8, function: NativeFn
};

const natives = [_]NativeBinding{
    .{ .name = "clock", .function = clockNative },
    .{ .name = "input", .function = inputNative },
    .{ .name = "print", .function = printNative },
    .{ .name = "write", .function = writeNative },
    .{ .name = "error", .function = errorNative },
    .{ .name = "assert", .function = assertNative },
};

fn defineAllNatives() !void {
    for (natives) |n| try defineNative(n.name, n.function);
}

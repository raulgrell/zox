const std = @import("std");
const Value = @import("./value.zig").Value;

pub fn clockNative(args: []Value) Value {
    return Value.fromNumber(@intToFloat(f64, std.time.milliTimestamp()) / 1000 );
}

// void

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
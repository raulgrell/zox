const std = @import("std");
const Value = @import("./value.zig").Value;

pub fn clockNative(args: []Value) Value {
    return Value { .Number = @intToFloat(f64, std.time.milliTimestamp()) / 1000 };
}
const std = @import("std");

const Obj = @import("object.zig").Obj;

pub const ValueType = enum {
  Bool,
  Number,
  Obj,
  Nil,
};

pub const Value = union(ValueType) {
    Bool: bool,
    Number: f64,
    Obj: *Obj,
    Nil,

    fn toString(value: Value, buffer: []u8) []const u8 {
      switch (value) {
        .Bool => |b| return std.fmt.bufPrint(buffer, "{}", b) catch unreachable,
        .Number => |n|  return std.fmt.bufPrint(buffer, "{}", n) catch unreachable,
        .Obj => |o| return o.toString(),
        .Nil => return "nil",
      }
    }

    fn equals(a: Value, b: Value) bool {
      if (ValueType(a) != ValueType(b)) return false;
      switch (a) {
        .Bool => return a.Bool == b.Bool,
        .Number => return a.Number == b.Number,
        .Nil => return true,
        .Obj => |o| return o.equal(b.Obj),
      }
    }

    fn isTruthy(value: Value) bool {
      switch (value) {
        .Nil => return false,
        .Bool => |b| return b,
        .Number => |n| return true,
        .Obj => |o| return true, 
      }
    }
};
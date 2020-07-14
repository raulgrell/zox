const std = @import("std");

const Obj = @import("object.zig").Obj;

pub const ValueType = enum {
  Bool,
  Number,
  Obj,
  Nil,
};

var buffer = [_]u8 {0} ** 1024;

pub const Value = union(ValueType) {
    Bool: bool,
    Number: f64,
    Obj: *Obj,
    Nil,

    pub fn nil() Value {
      return Value.Nil;
    }

    pub fn toString(value: Value) []const u8 {
      switch (value) {
        .Bool => |b| return std.fmt.bufPrint(buffer[0..], "{}", .{b}) catch unreachable,
        .Number => |n|  return std.fmt.bufPrint(buffer[0..], "{}", .{n}) catch unreachable,
        .Obj => |o| return o.toString(),
        .Nil => return "nil",
      }
    }

    pub fn equals(a: Value, b: Value) bool {
      if (@as(ValueType, a) != @as(ValueType, b)) return false;
      switch (a) {
        .Bool => return a.Bool == b.Bool,
        .Number => return a.Number == b.Number,
        .Obj => |o| return o.equal(b.Obj),
        .Nil => return true,
      }
    }

    pub fn isTruthy(value: Value) bool {
      switch (value) {
        .Bool => |b| return b,
        .Number => |n| return true,
        .Obj => |o| return true, 
        .Nil => return false,
      }
    }
};
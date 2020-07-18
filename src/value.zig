const std = @import("std");
const Obj = @import("./object.zig").Obj;

pub const NAN_TAGGING = true;

pub const Value = if (NAN_TAGGING) NanTaggedValue else UnionValue;

pub const ValueType = enum {
  Bool,
  Number,
  Obj,
  Nil,
};

pub const NanTaggedValue = packed struct {
    data: u64,

    const SIGN_BIT: u64 = 0x8000000000000000;
    const QNAN: u64 = 0x7ffc000000000000;

    const TAG_NIL = 1; // 01.
    const TAG_FALSE = 2; // 10.
    const TAG_TRUE = 3; // 11.

    const NIL_VAL = NanTaggedValue{ .data = QNAN | TAG_NIL };
    const TRUE_VAL = NanTaggedValue{ .data = QNAN | TAG_TRUE };
    const FALSE_VAL = NanTaggedValue{ .data = QNAN | TAG_FALSE };

    pub fn isBool(self: NanTaggedValue) bool {
        return (self.data & FALSE_VAL.data) == FALSE_VAL.data;
    }

    pub fn isNil(self: NanTaggedValue) bool {
        return self.data == NIL_VAL.data;
    }

    pub fn isNumber(self: NanTaggedValue) bool {
        return (self.data & QNAN) != QNAN;
    }

    pub fn isObj(self: NanTaggedValue) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub fn asNumber(self: NanTaggedValue) f64 {
        std.debug.assert(self.isNumber());
        return @bitCast(f64, self.data);
    }

    pub fn asInteger(self: NanTaggedValue) u32 {
        std.debug.assert(self.isNumber());
        return @floatToInt(u32, @bitCast(f64, self.data));
    }

    pub fn asBool(self: NanTaggedValue) bool {
        std.debug.assert(self.isBool());
        return self.data == TRUE_VAL.data;
    }

    pub fn asObj(self: NanTaggedValue) *Obj {
        std.debug.assert(self.isObj());
        return @intToPtr(*Obj, @intCast(usize, self.data & ~(SIGN_BIT | QNAN)));
    }

    pub fn fromNumber(x: f64) NanTaggedValue {
        return NanTaggedValue{ .data = @bitCast(u64, x) };
    }

    pub fn fromBool(x: bool) NanTaggedValue {
        return if (x) TRUE_VAL else FALSE_VAL;
    }

    pub fn fromObj(x: *Obj) NanTaggedValue {
        return NanTaggedValue{ .data = SIGN_BIT | QNAN | @ptrToInt(x) };
    }

    pub fn nil() NanTaggedValue {
        return NIL_VAL;
    }

    pub fn isFalsey(self: NanTaggedValue) bool {
        if (self.isBool()) return !self.asBool();
        if (self.isNil()) return true;
        return false;
    }

    pub fn equals(self: NanTaggedValue, other: NanTaggedValue) bool {
        // Be careful about IEEE NaN equality semantics
        if (self.isNumber() and other.isNumber()) return self.asNumber() == other.asNumber();
        return self.data == other.data;
    }

    pub fn format(self: NanTaggedValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) !void {
        if (self.isNumber()) {
            try out_stream.print("{d}", .{self.asNumber()});
        } else if (self.isBool()) {
            try out_stream.print("{}", .{self.asBool()});
        } else if (self.isNil()) {
            try out_stream.print("nil", .{});
        } else {
            const obj = self.asObj();
            try printObject(obj, out_stream);
        }
    }
};

pub const UnionValue = union(enum) {
    Bool: bool,
    Nil,
    Number: f64,
    Obj: *Obj,

    pub fn isBool(self: UnionValue) bool {
        return self == .Bool;
    }

    pub fn isNil(self: UnionValue) bool {
        return self == .Nil;
    }

    pub fn isNumber(self: UnionValue) bool {
        return self == .Number;
    }

    pub fn isObj(self: UnionValue) bool {
        return self == .Obj;
    }

    pub fn asBool(self: UnionValue) bool {
        std.debug.assert(self.isBool());
        return self.Bool;
    }

    pub fn asNumber(self: UnionValue) f64 {
        std.debug.assert(self.isNumber());
        return self.Number;
    }

    pub fn asInteger(self: UnionValue) u32 {
        std.debug.assert(self.isNumber());
        return @floatToInt(u32, self.Number);
    }

    pub fn asObj(self: UnionValue) *Obj {
        std.debug.assert(self.isObj());
        return self.Obj;
    }

    pub fn fromBool(x: bool) UnionValue {
        return UnionValue{ .Bool = x };
    }

    pub fn nil() UnionValue {
        return .Nil;
    }

    pub fn fromNumber(x: f64) UnionValue {
        return UnionValue{ .Number = x };
    }

    pub fn fromObj(x: *Obj) UnionValue {
        return UnionValue{ .Obj = x };
    }

    pub fn isFalsey(self: UnionValue) bool {
        return switch (self) {
            .Bool => |x| !x,
            .Nil => true,
            .Number => false,
            .Obj => false,
        };
    }

    pub fn equals(aBoxed: UnionValue, bBoxed: UnionValue) bool {
        return switch (aBoxed) {
            .Bool => |a| {
                return switch (bBoxed) {
                    .Bool => |b| a == b,
                    else => false,
                };
            },
            .Nil => |a| {
                return switch (bBoxed) {
                    .Nil => true,
                    else => false,
                };
            },
            .Number => |a| {
                return switch (bBoxed) {
                    .Number => |b| a == b,
                    else => false,
                };
            },
            .Obj => |a| {
                return switch (bBoxed) {
                    .Obj => |b| a == b,
                    else => false,
                };
            },
        };
    }

    pub fn format(self: UnionValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, out_stream: var) std.os.WriteError!void {
        switch (self) {
            .Number => |value| try out_stream.print("{d}", .{value}),
            .Bool => |value| try out_stream.print("{}", .{value}),
            .Nil => try out_stream.print("nil", .{}),
            .Obj => |obj| try printObject(obj, out_stream),
        }
    }
};

// Shared between the two value representations
fn printObject(obj: *Obj, out_stream: var) std.os.WriteError!void {
    switch (obj.objType) {
        .String => try out_stream.print("{}", .{obj.asString().bytes}),
        .List => {
            const list = obj.asList();
            try out_stream.print("[", .{});
            for (list.items.items) |item, i| {
                try item.format("{}", .{}, out_stream);
                if (i + 1 == list.items.items.len) break;
                try out_stream.print(", ", .{});
            }
            try out_stream.print("]", .{});
        },
        .Function => {
            const name = if (obj.asFunction().name) |str| str.bytes else "<script>";
            try out_stream.print("<fn {}>", .{name});
        },
        .Native => {
            try out_stream.print("<native fn>", .{});
        },
        .Closure => {
            const name = if (obj.asClosure().function.name) |str| str.bytes else "<script>";
            try out_stream.print("<fn {}>", .{name});
        },
        .Upvalue => {
            try out_stream.print("upvalue", .{});
        },
        .Class => {
            try out_stream.print("{}", .{obj.asClass().name.bytes});
        },
        .Instance => {
            try out_stream.print("{} instance", .{obj.asInstance().class.name.bytes});
        },
        .BoundMethod => {
            const name = if (obj.asBoundMethod().method.function.name) |str| str.bytes else "<script>";
            try out_stream.print("<fn {}>", .{name});
        },
    }
}

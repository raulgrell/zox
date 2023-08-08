const std = @import("std");
const Obj = @import("./object.zig").Obj;

const config = @import("./config.zig");
const debug = @import("./debug.zig");
const tracy = @import("./tracy.zig");

pub const Value = if (config.nanTagging) NanTaggedValue else UnionValue;

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

    const TAG_NIL = 0b01;
    const TAG_FALSE = 0b10;
    const TAG_TRUE = 0b11;

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

    pub fn isInteger(self: NanTaggedValue) bool {
        return (self.data & QNAN) != QNAN;
    }

    pub fn isObj(self: NanTaggedValue) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub fn isObjString(self: NanTaggedValue) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub fn isObjType(self: NanTaggedValue, objType: Obj.Type) bool {
        return self.isObj() and self.asObj().objType == objType;
    }

    pub fn asNumber(self: NanTaggedValue) f64 {
        std.debug.assert(self.isNumber());
        return @bitCast(self.data);
    }

    pub fn asInteger(self: NanTaggedValue) u32 {
        std.debug.assert(self.isNumber());
        return @intFromFloat(@as(f64, @bitCast(self.data)));
    }

    pub fn asBool(self: NanTaggedValue) bool {
        std.debug.assert(self.isBool());
        return self.data == TRUE_VAL.data;
    }

    pub fn asObj(self: NanTaggedValue) *Obj {
        std.debug.assert(self.isObj());
        return @as(*Obj, @ptrFromInt(@as(usize, @intCast(self.data & ~(SIGN_BIT | QNAN)))));
    }

    pub fn asObjType(self: NanTaggedValue, comptime objType: Obj.Type) *Obj.ObjType(objType) {
        return self.asObj().asObjType(objType);
    }

    pub fn fromNumber(x: f64) NanTaggedValue {
        return NanTaggedValue{ .data = @as(u64, @bitCast(x)) };
    }

    pub fn fromBool(x: bool) NanTaggedValue {
        return if (x) TRUE_VAL else FALSE_VAL;
    }

    pub fn fromObj(x: *Obj) NanTaggedValue {
        return NanTaggedValue{ .data = SIGN_BIT | QNAN | @intFromPtr(x) };
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

    pub fn format(self: NanTaggedValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = options;
        _ = fmt;
        if (self.isNumber()) {
            try writer.print("{d}", .{self.asNumber()});
        } else if (self.isBool()) {
            try writer.print("{}", .{self.asBool()});
        } else if (self.isNil()) {
            try writer.print("nil", .{});
        } else {
            const obj = self.asObj();
            try printObject(obj, writer);
        }
    }
};

pub const PointerTaggedValue = packed struct {
    data: u64,

    const SIGN_BIT: u64 = 0x8000000000000000;
    const QNAN: u64 = 0x7ffc000000000000;

    const TAG_NIL = 1; // 001.
    const TAG_FALSE = 2; // 010.
    const TAG_TRUE = 3; // 011.

    const NIL_VAL = PointerTaggedValue{ .data = QNAN | TAG_NIL };
    const TRUE_VAL = PointerTaggedValue{ .data = QNAN | TAG_TRUE };
    const FALSE_VAL = PointerTaggedValue{ .data = QNAN | TAG_FALSE };

    pub fn isBool(self: PointerTaggedValue) bool {
        return (self.data & FALSE_VAL.data) == FALSE_VAL.data;
    }

    pub fn isNil(self: PointerTaggedValue) bool {
        return self.data == NIL_VAL.data;
    }

    pub fn isNumber(self: PointerTaggedValue) bool {
        return (self.data & QNAN) != QNAN;
    }

    pub fn isInteger(self: PointerTaggedValue) bool {
        return (self.data & QNAN) != QNAN;
    }

    pub fn isObj(self: PointerTaggedValue) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub fn isObjType(self: PointerTaggedValue, objType: Obj.Type) bool {
        return self.isObj() and self.asObj().objType == objType;
    }

    pub fn asNumber(self: PointerTaggedValue) f64 {
        std.debug.assert(self.isNumber());
        return @as(f64, @bitCast(self.data));
    }

    pub fn asInteger(self: PointerTaggedValue) u32 {
        std.debug.assert(self.isNumber());
        return @as(u32, @intFromFloat(@as(f64, @bitCast(self.data))));
    }

    pub fn asBool(self: PointerTaggedValue) bool {
        std.debug.assert(self.isBool());
        return self.data == TRUE_VAL.data;
    }

    pub fn asObj(self: PointerTaggedValue) *Obj {
        std.debug.assert(self.isObj());
        return @as(*Obj, @ptrFromInt(@as(usize, @intCast(self.data & ~(SIGN_BIT | QNAN)))));
    }

    pub fn asObjType(self: PointerTaggedValue, comptime objType: Obj.Type) *Obj.ObjType(objType) {
        return self.asObj().asObjType(objType);
    }

    pub fn fromNumber(x: f64) PointerTaggedValue {
        return PointerTaggedValue{ .data = @as(u64, @bitCast(x)) };
    }

    pub fn fromBool(x: bool) PointerTaggedValue {
        return if (x) TRUE_VAL else FALSE_VAL;
    }

    pub fn fromObj(x: *Obj) PointerTaggedValue {
        return PointerTaggedValue{ .data = SIGN_BIT | QNAN | @intFromPtr(x) };
    }

    pub fn nil() PointerTaggedValue {
        return NIL_VAL;
    }

    pub fn isFalsey(self: PointerTaggedValue) bool {
        if (self.isBool()) return !self.asBool();
        if (self.isNil()) return true;
        return false;
    }

    pub fn equals(self: PointerTaggedValue, other: PointerTaggedValue) bool {
        // Be careful about IEEE NaN equality semantics
        if (self.isNumber() and other.isNumber()) return self.asNumber() == other.asNumber();
        return self.data == other.data;
    }

    pub fn format(self: PointerTaggedValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = options;
        _ = fmt;
        if (self.isNumber()) {
            try writer.print("{d}", .{self.asNumber()});
        } else if (self.isBool()) {
            try writer.print("{}", .{self.asBool()});
        } else if (self.isNil()) {
            try writer.print("nil", .{});
        } else {
            const obj = self.asObj();
            try printObject(obj, writer);
        }
    }
};

pub const UnionValue = union(ValueType) {
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

    pub fn isObjType(self: UnionValue, objType: Obj.Type) bool {
        return self.isObj() and self.asObj().objType == objType;
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
        return @as(u32, @intFromFloat(self.Number));
    }

    pub fn asObj(self: UnionValue) *Obj {
        std.debug.assert(self.isObj());
        return self.Obj;
    }

    pub fn asObjType(self: UnionValue, comptime objType: Obj.Type) *Obj.ObjType(objType) {
        return self.asObj().asObjType(objType);
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
            .Bool => |a| switch (bBoxed) {
                .Bool => |b| a == b,
                else => false,
            },
            .Nil => switch (bBoxed) {
                .Nil => true,
                else => false,
            },
            .Number => |a| switch (bBoxed) {
                .Number => |b| a == b,
                else => false,
            },
            .Obj => |a| switch (bBoxed) {
                .Obj => |b| a == b,
                else => false,
            },
        };
    }

    pub fn format(self: UnionValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Number => |value| try writer.print("{d}", .{value}),
            .Bool => |value| try writer.print("{}", .{value}),
            .Nil => try writer.print("nil", .{}),
            .Obj => |obj| try printObject(obj, writer),
        }
    }
};

// Shared between the two value representations
fn printObject(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    switch (obj.objType) {
        .String => try printString(obj, writer),
        .List => try printList(obj, writer),
        .Function => try printFunction(obj, writer),
        .Native => try printNative(obj, writer),
        .Closure => try printClosure(obj, writer),
        .Upvalue => try printUpvalue(obj, writer),
        .Class => try printClass(obj, writer),
        .Instance => try printInstance(obj, writer),
        .BoundMethod => try printBoundMethod(obj, writer),
    }
}

fn printString(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    try writer.print("{s}", .{obj.asString().bytes});
}

fn printList(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    const list = obj.asList();
    try writer.print("[", .{});
    for (list.items.items, 0..) |item, i| {
        try item.format("{}", .{}, writer);
        if (i + 1 == list.items.items.len) break;
        try writer.print(", ", .{});
    }
    try writer.print("]", .{});
}

fn printFunction(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    const name = if (obj.asFunction().name) |str| str.bytes else "<script>";
    try writer.print("<fn {s}>", .{name});
}

fn printNative(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    const func = obj.asNative();
    try writer.print("<native fn {s}>", .{func.name.bytes});
}

fn printClosure(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    const name = if (obj.asClosure().function.name) |str| str.bytes else "<script>";
    try writer.print("<fn {s}>", .{name});
}

fn printUpvalue(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    const val = obj.asUpvalue().location;
    try writer.print("upvalue {}", .{val});
}

fn printClass(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    try writer.print("{s}", .{obj.asClass().name.bytes});
}

fn printInstance(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    try writer.print("{s} instance", .{obj.asInstance().class.name.bytes});
}

fn printBoundMethod(obj: *Obj, writer: anytype) @TypeOf(writer).Error!void {
    const t = tracy.Zone(@src());
    defer t.End();

    const name = if (obj.asBoundMethod().method.function.name) |str| str.bytes else "<script>";
    try writer.print("<fn {s}>", .{name});
}

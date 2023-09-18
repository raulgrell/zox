const std = @import("std");
const Value = @import("../vm.zig").Value;
const Obj = @import("../object.zig").Obj;
const VM = @import("../vm.zig").VM;
const NativeBinding = @import("../vm.zig").NativeBinding;

const natives = [_]NativeBinding{};

pub fn defineAllNatives(vm: *VM) !void {
    for (natives) |n| try vm.defineNative(n.name, n.function);
}

fn str(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("toString() takes no arguments ({} given)", .{args.len});
    }

    const valueString = args[0].to_string();
    const string = Obj.String.take(vm, valueString);
    return Value.fromObj(string);
}

fn len(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("len() takes no arguments ({} given)", .{args.len});
    }

    const dict = args[0].asObjType(.Map);
    return Value.fromNumber(dict.count);
}

fn keys(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("keys() takes no arguments ({} given)", .{args.len});
    }

    const map = args[0].asObjType(.Map);
    const list = Obj.List.create(vm);
    vm.push(Value.fromObj(list));

    var it = map.hash_map.iterator();
    while (it.next()) |v| {
        list.values.append(v.key_ptr.*);
    }

    _ = vm.pop();
    return Value.fromObj(list);
}

fn get(vm: *VM, args: []Value) Value {
    if (args.len != 1 and args.len != 2) {
        return vm.runtimeError("get() takes 1 or 2 arguments ({} given)", .{args.len});
    }

    const defaultValue = Value.nil();
    if (args.len == 2) {
        defaultValue = args[2];
    }

    if (!args[1].isValidKey()) {
        return vm.runtimeError("Dictionary key passed to get() must be an immutable type");
    }

    const map = args[0].asObjType(.Map);
    return map.hashMap.get(args[1]) orelse defaultValue;
}

fn remove(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("remove() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isValidKey()) {
        return vm.runtimeError("Dictionary key passed to remove() must be an immutable type");
    }

    const map = args[0].asObjType(.Map);
    if (map.hashMap.remove(args[1])) {
        return Value.nil();
    }

    const string = args[1].to_string();
    defer string.destroy();
    return vm.runtimeError("Key '%s' passed to remove() does not exist within the dictionary", string);
}

fn exists(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("exists() takes 1 argument ({} given)", .{args.len});
    }

    if (!args[1].isValidKey()) {
        return vm.runtimeError("Dictionary key passed to exists() must be an immutable type");
    }

    return Value.fromBool(false);
}

fn copy(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("copy() takes no arguments ({} given)", .{args.len});
    }

    const oldDict = args[0].asObjType(.Map);
    const newDict = Obj.Dict.copy(vm, oldDict);
    return Value.fromObj(newDict);
}

fn clone(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("clone() takes no arguments ({} given)", .{args.len});
    }

    const oldDict = args[0].asObjType(.Map);
    const newDict = Obj.Dict.copy(vm, oldDict);
    return Value.fromObj(newDict);
}

fn setup(vm: *VM) !void {
    vm.defineNative(vm.mapMethods, "str", str);
    vm.defineNative(vm.mapMethods, "len", len);
    vm.defineNative(vm.mapMethods, "keys", keys);
    vm.defineNative(vm.mapMethods, "get", get);
    vm.defineNative(vm.mapMethods, "remove", remove);
    vm.defineNative(vm.mapMethods, "exists", exists);
    vm.defineNative(vm.mapMethods, "copy", copy);
    vm.defineNative(vm.mapMethods, "clone", clone);

    vm.interpret(src);
}

const src =
    \\  extern class Map {
    \\      extern fn str(this);
    \\      extern fn len(this);
    \\      extern fn keys(this);
    \\      extern fn get(this, item);
    \\      extern fn remove(this, item);
    \\      extern fn has(this, item);
    \\      extern fn copy(this);
    \\      extern fn clone(this);
    \\
    \\      fn each(this, func) {
    \\          for (this.keys()) |k| {
    \\              func(k, map[k]);
    \\          }
    \\      }
    \\      
    \\      fn merge(this, anotherDict) {
    \\          var newDict = this.copy();
    \\          each(anotherDict, fn (key, value) => {
    \\             newDict[key] = value;
    \\          });
    \\      
    \\          return newDict;
    \\      }
    \\  }
;

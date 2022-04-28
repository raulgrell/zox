const std = @import("std");
const VM = @import("../vm.zig").VM;
const Value = @import("../vm.zig").Value;
const Obj = @import("../object.zig").Obj;

fn toString(vm: *VM, args: []Value) Value {
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

    const list = args[0].toObjType(.List);
    return Value.fromNumber(list.values.items.len);
}

fn push(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("push() takes 1 argument ({} given)", .{args.len});
    }

    const list = args[0].toObjType(.List);
    list.values.items.append(args[1]);

    return Value.nil();
}

fn insert(vm: *VM, args: []Value) Value {
    if (args.len != 2) {
        return vm.runtimeError("insert() takes 2 arguments ({} given)", .{args.len});
    }

    if (!args[2].isNumber()) {
        return vm.runtimeError("insert() second argument must be a number");
    }

    const list = args[0].toObjType(.List);
    const insertValue = args[1];
    const index = args[2].asNumber();

    if (index < 0 or index > list.values.items.len) {
        return vm.runtimeError("Index passed to insert() is out of bounds for the list given");
    }

    try list.values.insert(index, insertValue);
    return Value.nil();
}

fn pop(vm: *VM, args: []Value) Value {
    if (args.len != 0 and args.len != 1) {
        return vm.runtimeError("pop() takes either 0 or 1 arguments ({} given)", .{args.len});
    }

    const list = args[0].toObjType(.List);

    if (list.values.items.len == 0) {
        return vm.runtimeError("pop() called on an empty list");
    }

    var element = undefined;

    if (args.len == 1) {
        if (!args[1].isNumber()) {
            return vm.runtimeError("pop() index argument must be a number");
        }

        const index = args[1].asNumber();

        if (index < 0 or index > list.values.items.len) {
            return vm.runtimeError("Index passed to pop() is out of bounds for the list given");
        }

        element = list.values.values[index];

        var i = index;
        while (i < list.values.items.len - 1) : (i += 1) {
            list.values.values[i] = list.values.values[i + 1];
        }
    } else {
        element = list.values.values[list.values.items.len - 1];
    }

    list.values.items.len -= 1;

    return element;
}

fn remove(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("remove() takes 1 argument ({} given)", .{args.len});
    }

    _ = args[0].toObjType(.List); // list
    _ = args[1]; // value

    return vm.runtimeError("Value passed to remove() does not exist within the list");
}

fn contains(vm: *VM, args: []Value) Value {
    if (args.len != 1) {
        return vm.runtimeError("contains() takes 1 argument ({} given)", .{args.len});
    }

    _ = args[0].toObjType(.List); // list
    _ = args[1]; // search

    return error.NotImplemented;
}

fn join(vm: *VM, args: []Value) Value {
    if (args.len != 0 and args.len != 1) {
        return vm.runtimeError("join() takes 1 optional argument ({} given)", .{args.len});
    }

    return error.NotImplemented;
}

fn copy(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("copy() takes no arguments ({} given)", .{args.len});
    }

    const oldList = args[0].toObjType(.List);
    const list = Obj.List.copy(oldList);
    return Value.fromObj(list);
}

fn clone(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("copy() takes no arguments ({} given)", .{args.len});
    }

    const oldList = args[0].toObjType(.List);
    const list = Obj.List.copy(oldList);
    return Value.fromObj(list);
}

fn sort(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("sort() takes no arguments ({} given)", .{args.len});
    }

    _ = args[0].toObjType(.List); // list

    return error.NotImplemented;
}

fn reverse(vm: *VM, args: []Value) Value {
    if (args.len != 0) {
        return vm.runtimeError("reverse() takes no arguments ({} given)", .{args.len});
    }

    _ = args[0].toObjType(.List); // list

    return error.NotImplemented;
}

fn declareListMethods(vm: *VM) void {
    vm.defineNative(&vm.listMethods, "str", toString);
    vm.defineNative(&vm.listMethods, "len", len);
    vm.defineNative(&vm.listMethods, "push", push);
    vm.defineNative(&vm.listMethods, "insert", insert);
    vm.defineNative(&vm.listMethods, "pop", pop);
    vm.defineNative(&vm.listMethods, "remove", remove);
    vm.defineNative(&vm.listMethods, "contains", contains);
    vm.defineNative(&vm.listMethods, "join", join);
    vm.defineNative(&vm.listMethods, "copy", copy);
    vm.defineNative(&vm.listMethods, "clone", clone);
    vm.defineNative(&vm.listMethods, "sort", sort);
    vm.defineNative(&vm.listMethods, "reverse", reverse);

    vm.interpret(src);
}

const src =
    \\  extern class List {
    \\      extern fn str(this)
    \\      extern fn len(this)
    \\      extern fn push(this, item)
    \\      extern fn insert(this, index, item)
    \\      extern fn pop(this)
    \\      extern fn remove(this, item)
    \\      extern fn contains(this, item)
    \\      extern fn join(this)
    \\      extern fn copy(this)
    \\      extern fn clone(this)
    \\      extern fn sort(this)
    \\      extern fn reverse(this)

    \\      fn map(list, func) {
    \\          const temp = [];
    \\          for (list) |o| {
    \\              temp.push(func(o));
    \\          }
    \\          return temp;
    \\      }
    \\      
    \\      fn filter(list, func=fn(x) => x) {
    \\          const temp = [];
    \\          for (list) |o| {
    \\              if (func(o)) temp.push(o);
    \\          }
    \\          return temp;
    \\      }
    \\      
    \\      fn reduce(list, func, initial=0) {
    \\          var accumulator = initial;
    \\          for (list) |o| {
    \\              accumulator = func(accumulator, o);
    \\          }
    \\          return accumulator;
    \\      }
    \\      
    \\      fn each(list, func) {
    \\          for (list) |o| {
    \\              func(o);
    \\          }
    \\      }
    \\      
    \\      fn find(list, func, start, end) {
    \\          for (list[start..end]) {
    \\              if (func(list[i])) return list[i];
    \\          }
    \\      }
    \\      
    \\      fn findIndex(list, func, start, end) {
    \\          for (list[start..end]) {
    \\              if (func(list[i])) return i;
    \\          }
    \\      }
    \\  }
;

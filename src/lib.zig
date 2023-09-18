const std = @import("std");
const Value = @import("./value.zig").Value;
const NativeBinding = @import("./vm.zig").NativeBinding;
const VM = @import("./vm.zig").VM;
const Obj = @import("./object.zig").Obj;

pub const stdModule = @import("./lib/std.zig");
pub const boolModule = @import("./lib/bool.zig");
pub const classModule = @import("./lib/class.zig");
pub const instanceModule = @import("./lib/instance.zig");
pub const listModule = @import("./lib/list.zig");
pub const mapModule = @import("./lib/map.zig");
pub const numberModule = @import("./lib/number.zig");
pub const stringModule = @import("./lib/string.zig");

const SlotType = enum {
    Bool,
    Num,
    Foreign,
    List,
    Map,
    String,
    Null,
};

fn copyDict(vm: *VM, oldDict: *Obj.Dict) *Obj.Dict {
    const dict = Obj.Dict.create(vm);

    // Push to stack to avoid GC
    vm.push(Value.fromObj(dict));

    _ = oldDict;

    // var i = 0;
    // while (i <= oldDict.capacityMask) : (i += 1) {
    //     if (IS_EMPTY(oldDict.entries[i].key)) {
    //         continue;
    //     }

    //     const val = oldDict.entries[i].value;

    //     // Push to stack to avoid GC
    //     vm.push(val);
    //     dictSet(vm, dict, oldDict.entries[i].key, val);
    //     _ = vm.pop();
    // }

    _ = vm.pop();
    return dict;
}

fn copyList(vm: *VM, oldlist: *Obj.List) *Obj.List {
    const list = Obj.List.create(vm);

    // Push to stack to avoid GC
    vm.push(Value.fromObj(list.obj));

    _ = oldlist;

    // var i = 0;
    // while (i < oldList.values.count) : (i += 1) {
    //     const val = oldList.values.values[i];

    //     // Push to stack to avoid GC
    //     vm.push(val);
    //     writeValueArray(vm, &list.values, val);
    //     _ = vm.pop();
    // }

    _ = vm.pop();
    return list;
}

fn copyInstance(vm: *VM, oldInstance: *Obj.Instance) *Obj.Instance {
    const instance = Obj.Instance.create(vm, oldInstance.class);

    // Push to stack to avoid GC
    vm.push(Value.fromObj(instance.obj));

    // if (shallow) {
    //     tableAddAll(vm, &oldInstance.publicFields, &instance.publicFields);
    // } else {
    //     var i = 0;
    //     while (i <= oldInstance.publicFields.capacityMask) : (i += 1) {
    //         const entry = &oldInstance.publicFields.entries[i];
    //         if (entry.key != NULL) {
    //             const val = entry.value;

    //             if (IS_LIST(val)) {
    //                 val = Value.fromObj(copyList(vm, AS_LIST(val)));
    //             } else if (IS_DICT(val)) {
    //                 val = Value.fromObj(copyDict(vm, AS_DICT(val)));
    //             } else if (IS_INSTANCE(val)) {
    //                 val = Value.fromObj(copyInstance(vm, AS_INSTANCE(val)));
    //             }

    //             // Push to stack to avoid GC
    //             vm.push(val);
    //             tableSet(vm, &instance.publicFields, entry.key, val);
    //             _ = vm.pop();
    //         }
    //     }
    // }

    _ = vm.pop();
    return instance;
}

const natives = [_]NativeBinding{};

pub fn defineAllNatives(vm: *VM) !void {
    for (natives) |n| try vm.defineNative(n.name, n.function);
    try stdModule.io.defineAllNatives(vm);
    try stdModule.math.defineAllNatives(vm);
    try stdModule.net.defineAllNatives(vm);
    try stdModule.sys.defineAllNatives(vm);
    try boolModule.defineAllNatives(vm);
    try classModule.defineAllNatives(vm);
    try instanceModule.defineAllNatives(vm);
    try listModule.defineAllNatives(vm);
    try mapModule.defineAllNatives(vm);
    try numberModule.defineAllNatives(vm);
    try stringModule.defineAllNatives(vm);
}

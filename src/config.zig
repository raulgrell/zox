// The callback used to resolve a module name.
// pub const resolveModuleFn: WrenResolveModuleFn = undefined;

// The callback used to load a module.
// pub const loadModuleFn: WrenLoadModuleFn = undefined;

// The callback used to find a foreign method and bind it to a class.
// pub const bindForeignMethodFn: WrenBindForeignMethodFn = undefined;

// The callback used to find a foreign class and get its foreign methods.
// pub const bindForeignClassFn: WrenBindForeignClassFn = undefined;

// The callback used to display text
// pub const writeFn: WrenWriteFn = undefined;

// The callback used to report errors.
// pub const errorFn: WrenErrorFn = undefined;

// The number of bytes Wren will allocate before triggering the first garbage collection.
// pub const initialHeapSize: size_t = undefined;

// After a collection occurs, the threshold for the next collection is
// determined based on the number of bytes remaining in use.
// pub const minHeapSize: size_t = undefined;

// Wren will resize the heap automatically as the number of bytes
// remaining in use after a collection changes.
// pub const heapGrowthPercent: u32 = undefined;

// Whether to use NaN-Boxing
pub const nanTagging = true;
pub const stackMax = 256 * 256;

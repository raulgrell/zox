const fs = require('fs');
const path = require('path');
const readline = require('readline');

const source = fs.readFileSync(path.join(__dirname, '..', 'build/wasm-lib.wasm'));
const typedArray = new Uint8Array(source);

let wasm;
WebAssembly.instantiate(typedArray, {
  env: {
    writeOut: (ptr, len) => {
      process.stdout.write(new Uint8Array(wasm.memory.buffer.slice(ptr, ptr + len)));
    },
    writeErr: (ptr, len) => {
      process.stderr.write(new Uint8Array(wasm.memory.buffer.slice(ptr, ptr + len)));
    },
    now: () => Date.now(),
  },
}).then((result) => {
  wasm = result.instance.exports;
  main(wasm);
});

function main(wasm) {
  switch (process.argv.length) {
    case 2: {
      repl(wasm);
      break;
    }
    case 3: {
      runSource(wasm, fs.readFileSync(process.argv[2], { encoding: 'utf-8' }));
      break;
    }
    default: {
      process.stdout.write('Usage: node main.js [path]\n');
      process.exit(1);
    }
  }
}

function repl(wasm) {
  var vm = wasm.createVM();

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: '> ',
  });

  rl.prompt();

  rl.on('line', (line) => {
    if (line.length === 0) {
      rl.prompt();
      return;
    }

    var slice = allocateString(wasm, line);
    var exitCode = wasm.interpret(vm, slice.ptr, slice.len);
    wasm.dealloc(slice.ptr, slice.len);

    switch (exitCode) {
      case 0: // Success
      case 65: // Compile error
      case 70: // Runtime error
        rl.prompt();
        break;
      default: {
        // Other errors
        wasm.destroyVM(vm);
        process.exit(exitCode);
      }
    }
  }).on('close', () => {
    wasm.destroyVM(vm);
    process.exit(0);
  });
}

function runSource(wasm, source) {
      var slice = allocateString(wasm, source);
      var exitCode = wasm.run(slice.ptr, slice.len);
      wasm.dealloc(slice.ptr, slice.len);
  process.exit(exitCode);
}

function allocateString(wasm, str) {
  // convert source to Uint8Array
  const textEncoder = new TextEncoder();
  const sourceArray = textEncoder.encode(str);

  // get memory from wasm
  const len = sourceArray.length;
  const ptr = wasm.alloc(len);
  if (ptr === 0) {
    throw 'Cannot allocate memory';
  }

  // copy sourceArray to wasm
  var mem = new Uint8Array(wasm.memory.buffer);
  for (let i = 0; i < len; ++i) {
    mem[ptr + i] = sourceArray[i];
  }

  return { ptr, len };
}

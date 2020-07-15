
Because OP_CONSTANT only uses a single byte for its operand, a chunk may only contain up to 256 different constants. That’s small enough that people writing real-world code will hit that limit. We could use two or more bytes to store the operand, but that makes every constant instruction take up more space. Most chunks won’t need that many unique constants, so that wastes space and sacrifices some locality in the common case to support the rare case.

To balance those two competing aims, many instruction sets feature multiple instructions that perform the same operation but with operands of different sizes. Leave our existing one-byte OP_CONSTANT instruction alone, and define a second OP_CONSTANT_LONG instruction. It stores the operand as a 24-bit number, which should be plenty.

Implement this function:

void writeConstant(Chunk* chunk, Value value, int line) {
  // Implement me...
}

It adds value to chunk’s constant array and then writes an appropriate instruction to load the constant. Also add support to the disassembler for OP_CONSTANT_LONG instructions.

Defining two instructions seems to be the best of both worlds. What sacrifices, if any, does it force on us?



    What bytecode instruction sequences would you generate for the following expressions:

    1 * 2 + 3
    1 + 2 * 3
    3 - 2 - 1
    1 + 2 * 3 - 4 / -5

    (Remember that Lox does not have a syntax for negative number literals, so the -5 is negating the number 5.)

    If we really wanted a minimal instruction set, we could eliminate either OP_NEGATE or OP_SUBTRACT. Show the bytecode instruction sequence you would generate for:

    4 - 3 * -2

    First, without using OP_NEGATE. Then, without using OP_SUBTRACT.

    Given the above, do you think it makes sense to have both instructions? Why or why not? Are there any other redundant instructions you would consider including?

    Our VM’s stack has a fixed size, and we don’t check if pushing a value overflows it. This means the wrong series of instructions could cause our interpreter to crash or go into undefined behavior. Avoid that by dynamically growing the stack as needed.

    What are the costs and benefits of doing so?

    To interpret OP_NEGATE, we pop the operand, negate the value, and then push the result. That’s a simple implementation, but it increments and decrements ip unnecessarily, since the stack ends up the same height in the end. It might be faster to simply negate the value in place on the stack and leave ip alone. Try that and see if you can measure a performance difference.

    Are there other instructions where you can do a similar optimization?




    Many newer languages support string interpolation. Inside a string literal, you have some sort of special delimiters—most commonly ${ at the beginning and } at the end. Between those delimiters, any expression can appear. When the string literal is executed, the inner expression is evaluated, converted to a string, and then merged with the surrounding string literal.

    For example, if Lox supported string interpolation, then this:

    var drink = "Tea";
    var steep = 4;
    var cool = 2;
    print "${drink} will be ready in ${steep + cool} minutes.";

    Would print:

    Tea will be ready in 6 minutes.

    What token types would you define to implement a scanner for string interpolation? What sequence of tokens would you emit for the above string literal?

    What tokens would you emit for:

    "Nested ${"interpolation?! Are you ${"mad?!"}"}"

    Consider looking at other language implementations that support interpolation to see how they handle it.

    Several languages use angle brackets for generics and also have a >> right shift operator. This led to a classic problem in early versions of C++:

    vector<vector<string>> nestedVectors;

    This would produce a compile error because the >> was lexed to a single right shift token, not two > tokens. Users were forced to avoid this by putting a space between the closing angle brackets.

    Later versions of C++ are smarter and can handle the above code. Java and C# never had the problem. How do those languages specify and implement this?

    Many languages, especially later in their evolution, define “contextual keywords”. These are identifiers that act like reserved words in some contexts but can be normal user-defined identifiers in others.

    For example, await is a keyword inside an async method in C#, but in other methods, you can use await as your own identifier.

    Name a few contextual keywords from other languages, and the context where they are meaningful. What are the pros and cons of having contextual keywords? How would you implement them in your language’s front end if you needed to?




    To really understand the parser, you need to see how execution threads through the interesting parsing functions—parsePrecedence() and the parser functions stored in the table. Take this (strange) expression:

    (-1 + 2) * 3 - -4

    Write a trace of how those functions are called. Show the order they are called, which calls which, and the arguments passed to them.

    The ParseRule row for TOKEN_MINUS has both prefix and infix function pointers. That’s because - is both a prefix operator (unary negation) and an infix one (subtraction).

    In the full Lox language, what other tokens can be used in both prefix and infix positions? What about in C or another language of your choice?

    You might be wondering about more complex “mixfix” expressions that have more than two operands separated by tokens. C’s conditional or “ternary” operator, ? : is a widely-known one.

    Add support for that operator to the compiler. You don’t have to generate any bytecode, just show how you would hook it up to the parser and handle the operands.




    We could reduce our binary operators even further than we did here. Which other instructions can you eliminate, and how would the compiler cope with their absence?

    Conversely, we can improve the speed of our bytecode VM by adding more specific instructions that correspond to higher-level operations. What instructions would you define to speed up the kind of user code we added support for in this chapter?




    Each string requires two separate dynamic allocations—one for the ObjString and a second for the character array. Accessing the characters from a value requires two pointer indirections, which can be bad for performance.

    A more efficient solution relies on a technique called “flexible array members”. Use that to store the ObjString and its character array in a single contiguous allocation.

    When we create the ObjString for each string literal, we copy the characters onto the heap. That way, when the string is later freed, we know it is safe to free the characters too.

    This is a simpler approach, but wastes some memory, which might be a problem on very constrained devices. Instead, we could keep track of which ObjStrings own their character array and which are “constant strings” that just point back to the original source string or some other non-freeable location. Add support for this.

    If Lox was your language, what would you have it do when a user tries to use + with one string operand and the other some other type? Justify your choice. What do other languages do?






    In clox, we happen to only need keys that are strings, so the hash table we built is hardcoded for that key type. If we exposed hash tables to Lox users as a first-class collection, it would be useful to support different kinds of keys.

    Add support for keys of the other primitive types: numbers, Booleans, and nil. Later, clox will support user-defined classes. If we want to support keys that are instances of those classes, what kind of complexity does that add?

    Hash tables have a lot of knobs you can tweak that affect their performance. You decide whether to use separate chaining or open addressing. Depending on which fork in that road you take, you can tune how many entries are stored in each node, or the probing strategy you use. You control the hash function, load factor, and growth rate.

    All of this variety wasn’t created just to give CS doctoral candidates something to publish theses on: each has its uses in the many varied domains and hardware scenarios where hashing comes into play. Look up a few hash table implementations in different open source systems, research the choices they made, and try to figure out why they did things that way.

    Well, at least that wasn’t the only reason they were created. Whether that was the main reason is up for debate.

    Benchmarking a hash table is notoriously difficult. A hash table implementation may perform well with some keysets and poorly with others. It may work well at small sizes but degrade as it grows, or vice versa. It may choke when deletions are common, but fly when they aren’t. Creating benchmarks that accurately represent how your users will use the hash table is a challenge.

    Write a handful of different benchmark programs to validate our hash table implementation. How does the performance vary between them? Why did you choose the specific test cases you chose?





    The compiler adds a global variable’s name to the constant table as a string every time an identifier is encountered. It creates a new constant each time, even if that variable name is already in a previous slot in the constant table. That’s wasteful in cases where the same variable is referenced multiple times by the same function. That in turn increases the odds of filling up the constant table and running out of slots, since we only allow 256 constants in a single chunk.

    Optimize this. How does your optimization affect the performance of the compiler compared to the runtime? Is this the right trade-off?

    Looking up a global variable by name in a hash table each time it is used is pretty slow, even with a good hash table. Can you come up with a more efficient way to store and access global variables without changing the semantics?

    When running in the REPL, a user might write a function that references an unknown global variable. Then, in the next line, they declare the variable. Lox should handle this gracefully by not reporting an “unknown variable” compile error when the function is first defined.

    But when a user runs a Lox script, the compiler has access to the full text of the entire program before any code is run. Consider this program:

    fun useVar() {
      print oops;
    }

    var ooops = "too many o's!";

    Here, we can tell statically that oops will not be defined because there is no declaration of that global anywhere in the program. Note that useVar() is never called either, so even though the variable isn’t defined, no runtime error will occur because it’s never used either.

    We could report mistakes like this as compile errors, at least when running from a script. Do you think we should? Justify your answer. What do other scripting languages you know do?





    Our simple local array makes it easy to calculate the stack slot of each local variable. But it means that when the compiler resolves a reference to a variable, we have to do a linear scan through the array.

    Come up with something more efficient. Do you think the additional complexity is worth it?

    How do other languages handle code like:

    var a = a;

    What would you do? Why?

    Many languages make a distinction between variables that can be reassigned and those that can’t. In Java, the final modifier prevents you from assigning to a variable. In JavaScript, a variable declared with let can be assigned but one declared using const can’t. Swift treats let as single-assignment and uses var for assignable variables. Scala and Kotlin use val and var.

    Pick a keyword for a single-assignment variable form to add to Lox. Justify your choice, then implement it. An attempt to assign to a variable declared using your new keyword should cause a compile error.

    Extend clox to allow more than 255 local variables to be in scope at a time.




    In addition to if statements, most C-family languages have a multi-way switch statement. Add one to clox. The grammar is:

    switchStmt  → "switch" "(" expression ")"
                  "{" switchCase* defaultCase? "}" ;
    switchCase  → "case" expression ":" statement* ;
    defaultCase → "default" ":" statement* ;

    To execute a switch statement, first evaluate the parenthesized switch value expression. Then walk the cases. For each case, evaluate its value expression. If the case value is equal to the switch value, execute the statements under the case and then exit the switch statement. Otherwise, try the next case. If no case matches and there is a default clause, execute its statements.

    To keep things simpler, we’re omitting fallthrough and break statements. Each case automatically jumps to the end of the switch statement after its statements are done.

    In jlox, we had a challenge to add support for break statements. This time, let’s do continue:

    continueStmt → "continue" ";" ;

    A continue statement jumps directly to the top of the nearest enclosing loop, skipping the rest of the loop body. Inside a for loop, a continue jumps to the increment clause, if there is one. It’s a compile-time error to have a continue statement not enclosed in a loop.

    Make sure to think about scope. What should happen to local variables declared inside the body of the loop or in blocks nested inside the loop when a continue is executed?

    Control flow constructs have been mostly unchanged since Algol 68. Language evolution since then has focused on making code more declarative and high level, so imperative control flow hasn’t gotten much attention.

    For fun, try to invent a useful novel control flow feature for Lox. It can be a refinement of an existing form or something entirely new. In practice, it’s hard to come up with something useful enough at this low expressiveness level to outweigh the cost of forcing a user to learn an unfamiliar notation and behavior, but it’s a good chance to practice your design skills.






    Reading and writing the ip field is one of the most frequent operations inside the bytecode loop. Right now, we access it through a pointer to the current CallFrame. That requires a pointer indirection which may force the CPU to bypass the cache and hit main memory. That can be a real performance sink.

    Ideally, we’d keep the ip in a native CPU register. C doesn’t let us require that without dropping into inline assembly, but we can structure the code to encourage the compiler to make that optimization. If we store the ip directly in a C local variable and mark it register, there’s a good chance the C compiler will accede to our polite request.

    This does mean we need to be careful to load and store the local ip back into the correct CallFrame when starting and ending function calls. Implement this optimization. Write a couple of benchmarks and see how it affects the performance. Do you think the extra code complexity is worth it?

    Right now, there’s no way for a native function to signal a runtime error. In a real implementation, this is something we’d need to support because native functions live in the statically-typed world of C but are called from dynamically-typed Lox land. If a user, say, tries to pass a string to sin(), that native function needs to report a runtime error.

    Extend the native function system to support that. How does this capability affect performance of native calls?

    Add some more native functions to do things you find useful. Write some programs using those. What did you add? How do they affect the feel of the language and how practical it is?





    Wrapping every ObjFunction in an ObjClosure introduces a level of indirection that has a performance cost. That cost isn’t necessary for functions that do not close over any variables, but it does let the runtime treat all calls uniformly.

    Change clox to only wrap functions in ObjClosures that need upvalues. How does the code complexity and performance compare to always wrapping functions? Take care to benchmark programs that do and do not use closures. How should you weight the importance of each benchmark? If one gets slower and one faster, how do you decide what trade-off to make to choose an implementation strategy?

    Read the design note below. I’ll wait. Now, how do you think Lox should behave? Change the implementation to create a new variable for each loop iteration.

    A famous koan teaches us that “objects are a poor man’s closure” (and vice versa). Our VM doesn’t support objects yet, but now that we have closures we can approximate them. Using closures, write a Lox program that expresses two-dimensional vector “objects”. It should:

        Define a “constructor” function to create a new vector with the given x and y coordinates.

        Provide “methods” to access the x and y coordinates of values returned from that constructor.

        Define an addition “method” that adds two vectors and produces a third.



    The Obj header struct at the top of each object now has three fields: type, isMarked, and next. How much memory do those take up (on your machine)? Can you come up with something more compact? Is there a runtime cost to doing so?

    When the sweep phase traverses a live object, it clears the isMarked field to prepare it for the next collection cycle. Can you come up with a more efficient approach?

    Mark-sweep is only one of a variety of garbage collection algorithms out there. Explore those by replacing or augmenting the current collector with another one. Good candidates to consider are reference counting, Cheney’s algorithm, or the Lisp 2 mark-compact algorithm.






    Trying to access a non-existent field on an object immediately aborts the entire VM. The user has no way to recover from this runtime error, nor is there any way to see if a field exists before trying to access it. It’s up to the user to ensure on their own that only valid fields are read.

    How do other dynamically-typed languages handle missing fields? What do you think Lox should do? Implement your solution.

    Fields are accessed at runtime by their string name. But that name must always appear directly in the source code as an identifier token. A user program cannot imperatively build a string value and then use that as the name of a field. Do you think they should be able to? Devise a language feature that enables that and implement it.

    Conversely, Lox offers no way to remove a field from an instance. You can set a field’s value to nil, but the entry in the hash table is still there. How do other languages handle this? Choose and implement a strategy for Lox.

    Because fields are accessed by name at runtime, working with instance state is slow. It’s technically a constant-time operation—thanks, hash tables—but the constant factors are relatively large. This is a major component of why dynamic languages are slower than statically-typed ones.

    How do sophisticated implementations of dynamically-typed languages cope with and optimize this?






    The hash table lookup to find a class’s init() method is constant time, but still fairly slow. Implement something faster. Write a benchmark and measure the performance difference.

    In a dynamically-typed language like Lox, a single callsite may invoke a variety of methods on a number of classes throughout a program’s execution. Even so, in practice, most of the time a callsite ends up calling the exact same method on the exact same class for the duration of the run. Most calls are actually not polymorphic even if the language says they can be.

    How do advanced language implementations optimize based on that observation?

    When interpreting an OP_INVOKE instruction, the VM has to do two hash table lookups. First, it looks for a field that could shadow a method and only if that fails does it look for a method. The former check is rarely useful—most fields do not contain functions. But it is necessary because the language says fields and methods are accessed using the same syntax and fields shadow methods.

    That is a language choice that affects the performance of our implementation. Was it the right choice? If Lox were your language, what would you do?






A tenet of object-oriented programming is that a class should ensure new objects are in a valid state. In Lox, that means defining an initializer that populates the instance’s fields. Inheritance complicates invariants because the instance must be in a valid state according to all of the classes in the object’s inheritance chain.

The easy part is remembering to call super.init() in each subclass’s init() method. The harder part is fields. There is nothing preventing two classes in the inheritance chain from accidentally claiming the same field name. When this happens, they will step on each other’s fields and possibly leave you with an instance in a broken state.

If Lox was your language, how would you address this, if at all? If you would change the language, implement your change.

Our copy-down inheritance optimization is only valid because Lox does not permit you to modify a class’s methods after its declaration. This means we don’t have to worry about the copied methods in the subclass getting out of sync with later changes to the superclass.

Other languages like Ruby do allow classes to be modified after the fact. How do implementations of languages like that support class modification while keeping method resolution efficient?



In the jlox chapter on inheritance, we had a challenge to implement the BETA language’s approach to method overriding. Solve the challenge again, but this time in clox. Here’s the description of the previous challenge:

In Lox, as in most other object-oriented languages, when looking up a method, we start at the bottom of the class hierarchy and work our way up—a subclass’s method is preferred over a superclass’s. In order to get to the superclass method from within an overriding method, you use super.

The language BETA takes the opposite approach. When you call a method, it starts at the top of the class hierarchy and works down. A superclass method wins over a subclass method. In order to get to the subclass method, the superclass method can call inner, which is sort of like the inverse of super. It chains to the next method down the hierarchy.

The superclass method controls when and where the subclass is allowed to refine its behavior. If the superclass method doesn’t call inner at all, then the subclass has no way of overriding or modifying the superclass’s behavior.

Take out Lox’s current overriding and super behavior and replace it with BETA’s semantics. In short:

    When calling a method on a class, prefer the method highest on the class’s inheritance chain.

    Inside the body of a method, a call to inner looks for a method with the same name in the nearest subclass along the inheritance chain between the class containing the inner and the class of this. If there is no matching method, the inner call does nothing.





    Fire up your profiler, run a couple of benchmarks, and look for other hotspots in the VM. Do you see anything in the runtime that you can improve?

    Many strings in real-world user programs are small, often only a character or two. This is less of a concern in clox because we intern strings, but most VMs don’t. For those that don’t, heap-allocating a tiny character array for each of those little strings and then representing the value as a pointer to that array is wasteful. Often the pointer is larger than the string’s characters. A classic trick is to have a separate value representation for small strings that stores the characters inline in the value.

    Starting from clox’s original tagged union representation, implement that optimization. Write a couple of relevant benchmarks and see if it helps.

    Reflect back on your experience with this book. What parts of it worked well for you? What didn’t? Was it easier for you to learn bottom-up or top-down? Did the illustrations help or distract? Did the analogies clarify or confuse?

    The more you understand your personal learning style, the more effectively you can upload knowledge into your head. You can specifically target material that teaches the way you learn best.



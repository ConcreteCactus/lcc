# General Description of the Project

This is a proof-of-concept implementation of a more usable form of a simply typed lambda calculus. The usual three building blocks of the model (variables, lambdas and abstractions) will be present as well as a fourth one that we call *references*. Most significantly, the structure of the to be compiled program code will be a list of definitions attached to unique names. When we use these names throughout the program code that is when we use a reference. Using references should generally be thought of as using the definition attached to the reference in place of the reference's position with minor differences in the two.

The language will be statically typed, most similar to a simply typed lambda calculus, with the exception of the support for parametric polymorphism.

The implementation will consist of a compiler that can compile to C code, which can be further compiled to machine code by the compiler of the reader's choice. The language will be garbage collected.
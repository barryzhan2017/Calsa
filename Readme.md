### Development Management

For Calsa, we could use [Visual Studio Code](https://code.visualstudio.com), [Merlin](https://github.com/ocaml/merlin), and [vscode-reasonml](https://github.com/reasonml-editor/vscode-reasonml) to provide modern IDE features for OCaml development, including auto completion, static analysis, and source browsing ("jump to the definition").

To setup, please refer to [Setting up VS Code for OCaml development](https://www.cosmiccode.blog/blog/vscode-for-ocaml/) by Andrew Barnes for more help. We could ignore the VS Code configuration to simplify setup process.

After setting up the development environement, please see the `.merlin` file under root directory, this is the file where we manage the source code path, compiled code path, and dependencies. Currently, we only have the following configuration:

```
S src
B _build/src
PKG llvm
```

which means Source code path: `src`, Built code path: `_build/src`, and our imported package `llvm`. Note that if we need to add more packages, the directive should probably be lower-case (e.g. Llvm should be written as llvm). Besides, we should install `ocamlfind` package to support the package management. For more help, please refer to [Merlin Project configuration](https://github.com/ocaml/merlin/wiki/project-configuration) for more information.

Note that Merlin will work after we compiled the code, which means only after we built the compiler with the command given in next section, we could notice there will be no errors and no warnings, and it could support important features such as source browsing (e.g. we could see the definition of Ast and Sast in semant.ml) and type inference.

### Build the MicroC compiler

```
ocamlbuild -pkgs llvm src/microc.native
```

### Run the MicroC compiler and generate llvm code
```
./microc.native -l example.mc > example.out
```

### Run the llvm code
```
lli example.out
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST) definition
-  `scanner.mll`: scanner
-  `microcparse.mly`: parser
-  `sast.ml`: definition of the semantically-checked AST
-  `semant.ml`: semantic checking
-  `irgen.ml`: LLVM IR code generator

### Other files

- `test1.ml`: the file to test the scanner and parser
- `test2.ml`: the file to test the semantic checker
- `microc.ml`: top-level file to test and run microc compiler
- `example.mc`: a sample microc source code
- `example.out`: a sample compiled code of example.mc

### Tips in linking c code into LLVM IR code
 
- `xxx.c and xxx.h` write down your c files in src folder
- `Makefile`: add your .c and .h file for object file and library file creation
- `microc.ml`: add your library by specifying -lxxx in command of gcc.
- `irgen.ml`: add the declaration of functions and add Call of your functions in build expression. If you define a any type, define it, too
- `semant.ml`: check your function declaration here
- `scanner.ml, microcparse.mly, ast.ml`: define your custom type here

Run 
```
make
```
to compile .c and .h files to a .a file and build our complier files into a microc.native file. Then follow the step in "Run the MicroC compiler and generate llvm code" to use linked compiler for example code.

Run
```
make clean 
```
to clean the created files during "make" and running our compiler.



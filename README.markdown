# PIC - PIC Is a Compiler for 8-bit PIC micro controllers

A native compiler for 8-bit PIC micro controllers written in Common Lisp. The host language is a pretty small subset of ML-like language and the target language is 8-bit PIC micro controller assembly. Common Lisp is the compiler language.

## Usage

Following is an example of LED blinking with PIC12F683 micro controller. `init` function is one of the compiler's special functions, where the micro controller's SFR(Special Function Registers) are initialized. Following `main` function is also the compiler's special function and program's main routine is executed here.

`mdelay1` function and `mdelay` macro are for delaying. Note that since 8-bit PIC handles only 8-bit unsigned integers, nested loops are needed for delaying more than 255 msec (950 msec here). `progn` and `loop` are predefined macros for the compiler.

    (defpic init ()
      (progn
        (setreg :gpio #x0)                  ; clera GP0-5
        (setreg :cmcon0 #x07)               ; disable comparator
        (setbank1)                          ; switch to bank 1
        (setreg :trisio #x08)               ; only GP3 is outputinputmode
        (setreg :ansel #x00)                ; disable analog IO
        (setreg :ioc #x00)                  ; disable interruption
        (setbank0)                          ; switch to bank 0
        (setreg :intcon #x00)))             ; disable interruption

    (defpic main ()
      (progn
        (setreg :gpio #x20)                 ; set GP5 to high
        (mdelay 50)                         ; delay for 50 msec
        (setreg :gpio #x00)                 ; set GP5 to low
        (mdelay 950)                        ; delay for 950 msec
        (main)))                            ; repeat

    (defpic mdelay1 ()
      (loop 52                              ; 52 is a magic number to delay
        0))                                 ;  for 1 msec

    (defpicmacro mdelay (n)
      (unless (<= 0 n 65535)
        (error "The value ~S is invalid." n))
      (multiple-value-bind (q r) (truncate n 256)
        (if (= q 0)
            `(loop ,r (mdelay1))
            `(loop ,q (loop ,r (mdelay1))))))

Then `pic-compile` function compiles and outputs the complete assembly for the PIC functions to standard output. The output assembly is expected to be assembled with Microchip's MPASM assembler.

    PIC> (pic-compile)
        INCLUDE"p12f683.inc"
        list p=12f683
    
        __CONFIG _CP_OFF & _CPD_OFF & _WDT_OFF & _BOD_ON & _IESO_OFF& _PWRTE_ON & _INTOSCIO & _MCLRE_OFF
    
        CBLOCK  020h
        L0,L1,L2,L3,L4,L5,L6,L7 ; local registers
        I0,I1,I2,I3,I4,I5,I6,I7 ; input registers
        SP,STMP,STK             ; stack registers
        ENDC
    
        ORG 0
        GOTO    MAIN
        ...
        END
    ; No value

## Installation

Since PIC is not available on Quicklisp distribution yet, please use its local-projects feature.

    $ ~/quicklisp/local-projects
    $ git clone git://github.com/takagi/pic.git

Then `(ql:quickload :pic)` from `REPL` to load it. I will request PIC to Quicklisp soon.

## API

### [Macro] defpic

    DEFPIC name arguments expression

Defines a PIC function. At least, `main` special function must be defined. `pic-disassemble` shows the compiled assembly of a PIC function.

### [Macro] defpicmacro

    DEFPICMACRO name arguments form

Defines a PIC macro. `pic-macroexpand` returns an expansion for a PIC macro form.

### [Function] pic-compile

    PIC-COMPILE

Compiles and outputs the complete assembly for PIC functions defined with `defpic` macro to `*standard-output*`. The output assembly is expected to be assembled with Microchip's MPASM assembler.

### [Function] pic-disassemble

    PIC-DISASSEMBLE name

Shows the compiled assembly of a function specified with `name`.

### [Function] pic-macroexpand, pic-macroexpand1

    PIC-MACROEXPAND form => expansion, expanded-p

    PIC-MACROEXPAND1 form => expansion, expanded-p

Returns a macro expansion of `form`. If `form` is a macro form, then `pic-macroexpand1` expands the macro form call once. `pic-macroexpand` repeatedly expands `form` until it is no longer a macro form. If `form` is a macro form, then the `expansion` is a macro expansion and `expanded-p` is `t`. Otherwise, the `expansion` is the given `form` and `expanded-p` is `nil`.

### [Function] pic-clear

    PIC-CLEAR

Clears all defined PIC functions. PIC macros are not cleared, which individually do not affect compiled assembly.

## Language

### Data

The compiler has only unsigned 8-bit integer as its data structure.

### Syntax

The compiler has the following syntax.

* literal
* arithmetic operations
* conditional branches
* variable bindings
* variable reference
* local function definitions
* function applications
* writing to registers

#### Literal

    42

Literal for 8-bit unsigned integers is only allowed for now.

#### Arithmetic operations

    (- 2 1)

Subtraction for two 8-bit unsigned integers is only allowed for now.

#### Conditional branches

    (if (= x 0)
      42
      0)

Equality testing for 8-bit unsigned integers is only allowed for now. 

#### Variable bindings and its reference

    (let ((x 42))
      (let ((y 1))
        (- x y)))

Binds an expression to a variable and reference it. Only one variable is bound for each `let` form for now.


#### Local function definitions

    (let ((foo (x) (+ x 42)))
      (foo 100))

Locally defines a function. It can be called recursively. Making closures and having free variables are not allowed for now.


#### Function applications

    (foo 42)

Calls a function which is defined with `defpic` macro or a local function definition. Tail calls are compiled into `GOTO` instruction, not `CALL` instruction.

#### Writing to registers

    (setreg :gpio #x20)

Writes a value into the micro controller's SFR (Special Function Registers). This is an only syntax that causes side effects.

### Macro

The compiler has the macro feature as well as Common Lisp does. Its macros are defined with `defpicmacro` Common Lisp macro. The following PIC macros are predefined.

#### [PIC-Macro] progn

    PROGN expression*

Sequentially evaluates the given expressions. Actually they are expanded into a series of let bindings, where temporal assignments should be removed properly in optimization.

    (progn
      (setreg :gpio #x00)
      (setreg :gpio #x01)
      (setreg :gpio #x02)
      (setreg :gpio #x04))

    ==>

    (let ((tmp (setreg :gpio #x00)))
      (let ((tmp (setreg :gpio #x01)))
        (let ((tmp (setreg :gpio #x02)))
          (setreg :gpio #x03))))

#### [PIC-Macro] loop

    LOOP n expression

Processes `expression` for `n` times. Actually a loop form is expanded into a local function definition and its recursive call. Loop facility may be redesigned as a syntax to be compiled into prefarable assembly, using `DECFSZ` instruction.

    (loop 10
      (do-something))

    ==>

    (let ((_loop (i)
            (if (= i 0)
              (progn
                (do-something)
                (_loop (- i 1)))
              0)))
      (_loop 10))

#### [PIC-Macro] setbank0

    SETBANK0

Sets the current bank to bank 0.

    (setbank0) ==> (setreg :status #x00)

#### [PIC-Macro] setbank1

    SETBANK1

Sets the current bank to bank 1.

    (setbank1) ==> (setreg :status #x20)

### Special functions

The compiler has the following special functions. Required to be defined is only `main`. `init` and `intr` are optional.

* `init`
* `main`
* `intr`

Just after the micro controller is powered, `init` special function is called. Then, `main` special function is called soon after `init` special function has returned. `intr` special function is called when an interruption occurs.

### [PIC-Function] init

    INIT

Optional. `init` is called soon after the micro controller is powered. It must have no arguments. Expected is that some initializing operations are done in `init` special function.

    (defpic init ()
      (setbank1)
      (setreg :trisio #x08)   ; set pin 4 only to output mode
      (setbank0)
      (setreg :gpio #x00))    ; set GPIO to low

### [PIC-Function] main

    MAIN

Required. `main` is called next to `init` special function. It must have no arguments. Expected is that main operations are done in `main` special function. Often it ends with a recursive call to itself to make infinite loop.

    (defpic main ()
      (do-main-operations)
      (main))                ; call itself to repeat the main operations

### [PIC-Function] intr

    INTR

Optional. `intr` is called when an interruption occurs. It must have no arguments. Expected is that some operations to accept interruptions are done in `intr` special function. It naturally returns with `RETFIE` instruction to the address where the interruption has occured.

    (defpic intr ()
      (do-some-interruption-operations))

## Design

### Overview

The host language is a The host language is a pretty small subset of ML-like language and the target language is 8-bit PIC micro controller assembly. Common Lisp is the compiler language. Overall design of the compiler is based on [MinCaml](http://esumii.github.io/min-caml/).

### Calling convention

The compiler uses the following 'pseudo-registers' allocated in a particular part in the data memory for function calling.

* input pseudo-registers `I0-I7`
* local pseudo-registers `L0-L7`

On calling a function, its parameters are stored to be passed in the input pseudo-registers. Return values are stored in W register. Functions can use the local pseudo-registers freely for themselves. To avoid destroying the values in a caller function's 'alive' local pseudo-registers, they are saved in the software stack before calling. After returning from the callee function, they are restored. Here 'alive' means the registers are used after returning from the callee function. About the software stack, see the next section.

### Software stack

The compiler uses a software stack for saving the values in local pseudo-registers. The term 'software stack' means distinguishing it from the micro controller's hardware stack. It begins from the address `STK` upwords and the stack pointer is stored in the address `SP`. For pushing a content in W register on top of the stack, `_PUSH_STACK` assembler macro does the work. Conversely, `_POP_STACK` assembler macro pops back a value on top of the stack into W register. Currently, saving values in local pseudo-registers is the only usage of the software stack.

### Why no closures?

To adopt closures, calling to indirect address is required. However 8-bit PIC assembly's `CALL` instruction accepts only immediates for its destination address. Although calling to indirect address is possible with writing an address directly into the program counter, it is accompanied by the following instricates:

* managing not only `PCL` but `PCLATH`
* calculating the return address
* managing return address manually without the hardware stack
* considering `PCL` carry out

Because of them, closures are not adopted for now.

## Slide

* [Lisp Meet Up #25, 8-bit PIC マイコン用ネイティブコンパイラの作成](http://www.slideshare.net/masayukitakagi/2015-0225-45131616)

## See also

* [MinCaml](http://esumii.github.io/min-caml/)

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)

# License

Licensed under the MIT License.

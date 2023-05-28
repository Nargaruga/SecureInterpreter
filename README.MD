# Secure Interpreter
## Introduction
This is an interpreter for a simple functional language with primitive abstractions to grant the isolation of sensitive information from untrusted code. It was developement as an assignment for the Language Based Technology for Security course at UniPi.

## Basic Features
The language provides the following primitive types:
* integers;
* booleans;
* strings.

and supports the following binary operators:
* arithmetic operators for usage with integers;
* comparison operators for usage with any of the primitive types;
* logical operators for usage with booleans.

The only unary operator present in the language is the `not` operator, used to negate boolean values. For variable assignment and function definition, the `let` keyword is used. A conditional statement in the form of `if guard then c1 else c2` is present, but neither loops nor recursion were included as they did not seem particularly relevant in the context of the assignment. Note that functions can only be passed exactly one argument.

## Security Features
What follows is an overview of the security features of the language.

### Security Levels
The language defines three trust levels: `High`, `Normal` and `Low`. The current trust level passed to the evaluation function depends on the nature of the code being executed and influences how certain constructs are evaluated.

### Enclaves
The language allows for the definition of enclaves: sections of highly trusted code, created by the `enclave` primitive, in which variables can be declared using the `secret` keyword instead of `let`,  hwich renders them invisible to the outside environment.

Functions defined inside of an enclave are, by default, only visible inside of the enclave itself, just like secret variables. In order to make such functions globally available, we must mark them as gateways using the `gateway` keyword. The idea behind this approach is the following: we hide confidential information by not adding it to the outside environment, but we still allow controlled access to enclave-defined functionalities through functions we explicitly declare as gateway. The code inside of an enclave is executed with `High` trust level and closures are marked as `Enclaved`.

For example, the following piece of code successfully evaluates to `5`:

```
enclave
    secret x = 3 in
    let f y = x + y in
    gateway f in 0
end

f(2)
```

While this second piece of code raises an `UnboundException`, as we are trying to call a non-gateway function from outside the enclave:
```
enclave
    let f x = x + 1 in 0
end

f(2)
```

### Execution of Untrusted Code
The `include` primitive allows us to import an untrusted piece of code in our program, assigning an identifier to it. This code can only be executed by passing such identifier to an `execute` call, which proceeds to evaluate the code with `Low` trust level. Such code is not allowed to interact with enclaves in any way, and as such it cannot directly nor indirectly call any gateway function.  To further emphasize the isolation of untrusted code, any variable or function defined inside the included piece of code is not added into the environment and is thus inaccessible outside of the included code itself. This is done under the assumption that the untrusted code will have side effects and will thus be able to perform useful work even at this level of isolation. 

The following program includes a piece of untrusted code and successfully executes it:
```
include untr_code
    let x = 1 in
    x
end

execute(untr_code)
0
```

While this other program raises a `SecurityException` as a gateway function was called from within untrusted code:
```
enclave
    secret x = 3 in
    let f y = x + y in
    gateway f in 0
end

include untr_code =
    let v = f(3) in v
end

execute(untr_code)
0
```

## Running Tests
First, build the project with
```
dune build
```
Then, run tests with
```
dune test
```
You can add your own tests in the `tests/tests.ml` file.

# The Phi Programming Language

[![Hex pm](http://img.shields.io/hexpm/v/phi.svg?style=flat)](https://hex.pm/packages/phi)

* **Phi** - PureScript-like functional programming language compiled to BEAM using Erlang AST.
* **Phi** is a strongly-typed with compile-time type-checking and built-in support for concurrency and distribution.
* **Phi** empowers SYNRC to build scalable, reliable, real-time applications, especially for 5G, IoT, and edge computing.

## Motivation

* Speed over Simplicity.
* Reliability and Predictivness over Featureness.
* Avoid Bullshit at All Costs.
* Back to Miranda Roots

```
$ time mix phi.base  2.28s user 1.18s system 111% cpu 3.113 total
$ time mix phi.test  2.75s user 1.36s system 101% cpu 4.067 total
```

## Comments

This meant to be learning excercise on how to implement HM language for Erlang/OTP naturally, but things got out of control eventually. Same excercise could and should be applied for Idris for Pi/Sigma capabilities for those who blatantly dislike glorious Coq's syntax. As for Miranda-like syntax for Erlang/OTP dependently typed prover I name Sigma project and for Coq-inspired architecture there is already project in Groupoid, Christine. The Christine synax should be compatible with Erlang/OTP implementation once it emerges.

As for technical hints please take following:

* Start with classical HM typechecker
* Lear Erlang AST complitation by Henk or Per examples
* Don't use Erlang guards for anything as they slowdown compilation
* Don't use Topological sort for anything
* Pay Attention to Curring
* Pay Attention to FFI
* Pay Attention to Process modality

## Features

- Compilation of Base Library in 5 seconds
- Functional programming
- PureScript style
- Compile-time type Checking/Inference
- Algebraic data type (ADT)
- Functions, higher-order functions
- Currying and partial application
- Pattern matching, and Guards
- List comprehension
- Featured Absence of HKT
- Applicative and Monad
- Advanced module system
- Built-in concurrency

## Credits

This language was created by non-human impersonated spirits of these guys:

* Miranda Team (David Turner)
* Haskell Team (Simon Peyton Jones)
* PureScript Team (Phil Freeman)
* Hamler Team (Feng Lee)
* ML/LCF Team (Robin Milner)
* Prolog Team (Alain Colmerauer with Philippe Roussel)
* Erlang Team (Joe Armstrong)
* Elixir Team (José Valim)
* Groupoid Team (Namdak Tonpa)

## License

DHARMA LICENSE

# Boop

**Boop** is a glorified calculator for all your numbery needs. You can either
use its CLI, or add it to your own project as a library.

## Installation

To use **Boop**, you'll first need to clone this repository and then build it
with **Cargo**, nothing fancy here:

```shell
$ git clone https://github.com/leonskidev/boop
$ cargo build --release
```

You can find the compiled executable in `.../boop/target/release/`.

## Getting Started

**Boop**'s syntax is very simple, it's just maths. Since **Boop** has a REPL,
let's do a quick calculation and follow what happens:

```rs
// define the variable `a` and set it to `10`
=> a := 10
  a := 10
// define the function `square` which takes in the variable `x`;
// it should square `x`
=> square(x) := x * x
  square(x) := x*x
// then apply the function `square` to the variable `a`
=> square(a)
  100
// let's compare it against our existing knowledge
=> 100 = square(a)
  100 = 100
```

Internally, **Boop** uses a 128-bit fixed-point number split into two 64-bits
for the integral and decimal parts.

## Licence

All code in this project is dual-licenced under either:

- MIT Licence ([LICENCE-MIT](./LICENCE-MIT) or
  https://opensource.org/licenses/MIT)
- Apache Licence, Version 2.0 ([LICENCE-APACHE](./LICENCE-APACHE) or
  https://www.apache.org/licenses/LICENSE-2.0)

at your option.

### Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licenced as above, without any additional terms or conditions.

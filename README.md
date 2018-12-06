# Parses a C/Rust like language into ALang

[![Build Status](https://travis-ci.org/ohua-dev/alang-clike-parser.svg?branch=master)](https://travis-ci.org/ohua-dev/alang-clike-parser)

An example of a standalone Ohua module in the C/Rust like syntax.

```rs
ns some_ns;

use sf ohua.math::{add, isZero, plus};
use algo some::module::anAlgo;

fn sqare (x) {
    add(x, x)
}

fn algo1 (someParam) {
    let a = square(anAlgo(someParam));

    let coll0 = for i in coll {
        square(i)
    };

    let plusThree = | a | { plus(a, 3) };

    if (isZero(plusThree(a))) {
        coll0
    } else {
        a
    }

}

fn main (param) {
    algo0(param)
}
```

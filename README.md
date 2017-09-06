# Parses a C/Rust like language into ALang

An example of a standalone Ohua module in the C/Rust like syntax.

```rs
ns some_ns;

use ohua.math (add, isZero);

fn sqare (x) {
    add(x, x)
}

fn algo1 (someParam) {
    let a = square(someParam);

    let coll0 = for i in coll {
        square(i)
    };

    if (isZero(a)) {
        coll0
    } else {
        a
    }
    
}

fn main (param) {
    algo0(param)
}
```

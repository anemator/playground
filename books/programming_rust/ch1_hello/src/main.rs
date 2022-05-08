// NOTE `use` declarations bringing two traits into scope which is required in order to use a trait's methods.
use std::io::Write;
use std::str::FromStr;

// NOTE Compile with `cargo build`, run with `cargo run`.
// NOTE main can return nothing or a type that implements std::process::Termination.
fn main() {
    let mut numbers = Vec::new();

    // NOTE args() returns an iterator
    for arg in std::env::args().skip(1) {
        // NOTE u64 implements FromStr (from_str).
        // NOTE from_str returns a Result type (Ok, Err) and `expect` unpacks it returning the value or `panic`ing with the message otherwise.
        numbers.push(u64::from_str(&arg).expect("error parsing argument"));
    }

    if numbers.len() == 0 {
        // NOTE Stderr implements Write (write_fmt)
        // NOTE `unwrap` checks that the write call did not fail similar to `expect` but shorter.
        writeln!(std::io::stderr(), "Usage: gcd NUMBER ...").unwrap();
        std::process::exit(1);
    }

    let mut d = numbers[0];
    // NOTE '&' means borrow a reference to the vector's elements. '*' means dereference the borrowed reference to get the value it contains. `numbers` owns the vector and `m` borrows the elements one at a time.
    for m in &numbers[1..] {
        d = gcd(d, *m);
    }

    // NOTE `{:?}` is a template that will insert a formatted version of
    // the variable into it.
    // QUESTION `numbers` isn't explicitly borrowed here, but it compiles
    // with and without `&` prefixed to it, is it borrow inference??
    println!("greatest common divisor of {:?} is {}", numbers, d);
}

fn gcd(mut n: u64, mut m: u64) -> u64 {
    // NOTE This will cause the program to panic (abort) with a nice error message when the condition is false in ALL builds. See also debug_assert! for a debug-build only assertion.
    assert!(n != 0 && m != 0);
    while m != 0 {
        if m < n {
            let t = m;
            m = n;
            n = t;
        }
        m = m % n;
    }
    // NOTE This is the return value because it is the last statement in the function and it does not end in a semicolon.
    n
}

// NOTE An attribute directing the compiler to ignore this during normal builds similar to #ifdef in C/C++. Compile and run with `cargo test`.
#[test]
fn test_gcd() {
    assert_eq!(gcd(14, 15), 1);
    assert_eq!(gcd(2 * 3 * 5 * 11 * 17,
                   3 * 7 * 11 * 13 * 19),
               3 * 11);
}

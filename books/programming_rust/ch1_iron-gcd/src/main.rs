// NOTE Allow use of mime (with macros) and iron crates in this file.
extern crate iron;
#[macro_use] extern crate mime;
extern crate router;
extern crate urlencoded;

// NOTE It's generally safe to import all functions of a prelude.
use iron::prelude::*;
use iron::status;
use router::Router;
use std::str::FromStr;
use urlencoded::UrlEncodedBody;

fn main() {
    let mut router = Router::new();
    router.get("/", get_form, "root");
    router.post("/gcd", post_gcd, "gcd");

    println!("Serving on http://0.0.0.0:3000...");
    Iron::new(router).http("0.0.0.0:3000").unwrap();
}

fn get_form(_request: &mut Request) -> IronResult<Response> {
    let mut response = Response::new();

    // NOTE set_mut figures out the section of the response to set
    // based on the type of the argument passed to it.
    response.set_mut(status::Ok); // http status
    response.set_mut(mime!(Text/Html; Charset=Utf8)); // media type header
    // NOTE raw text is `r` followed by zero or more hash marks
    response.set_mut(r#"
        <title>GCD Calculator</title>
        <form action="/gcd" method="post">
          <input type="text" name="n"/>
          <input type="text" name="n"/>
          <button type="submit">Compute GCD</button>
        </form>
    "#); // response body

    Ok(response)
}

// From ch1_hello
fn gcd(mut n: u64, mut m: u64) -> u64 {
    assert!(n != 0 && m != 0);
    while m != 0 {
        if m < n {
            let t = m;
            m = n;
            n = t;
        }
        m = m % n;
    }
    n
}

fn post_gcd(request: &mut Request) -> IronResult<Response> {
    let mut response = Response::new();

    // NOTE ::<...> is called a type parameter indicating which part get_ref should retrieve.
    let form_data = match request.get_ref::<UrlEncodedBody>() {
        Err(e) => {
            response.set_mut(status::BadRequest);
            response.set_mut(format!("Error parsing form data: {:?}\n", e));
            return Ok(response);
        }
        Ok(map) => map
    };

    // NOTE query parameters can be repeated so this will retrieve all those with key `n`.
    let unparsed_numbers = match form_data.get("n") {
        None => {
            response.set_mut(status::BadRequest);
            response.set_mut(format!("form data has no 'n' paramater\n"));
            return Ok(response);
        }
        Some(nums) => nums
    };

    let mut numbers = Vec::new();
    for unparsed in unparsed_numbers {
        match u64::from_str(&unparsed) {
            Err(_) => {
                response.set_mut(status::BadRequest);
                response.set_mut(format!("Value for 'n' parameter not a number: {:?}\n", unparsed));
                return Ok(response);
            }
            Ok(n) => { numbers.push(n); }
        }
    }

    let mut d = numbers[0];
    for m in &numbers[1..] {
        d = gcd(d, *m);
    }

    response.set_mut(status::Ok);
    response.set_mut(mime!(Text/Html; Charset=Utf8));
    response.set_mut(format!("The greatest common divisor of the numbers {:?} is <b>{}</b>\n", numbers, d));

    Ok(response)
}

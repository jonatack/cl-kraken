# cl-kraken
An API wrapper for the Kraken cryptocurrency exchange in Common Lisp.

Currently a side project for learning Common Lisp: Language, packages system, unit testing, interfacing with outside libraries and the real world, and so on. Suggestions and pull requests welcome!

## Dependencies

CL-KRAKEN imports a small number of functions from the following Common Lisp libraries: DEXADOR, YASON, QURI, LOCAL-TIME, IRONCLAD, and CL-BASE64.

## Portability

Developed with SBCL 1.4.15 and tested with ClozureCL 1.11.5 and CLISP 2.49, all on Linux x86/64.

Not tested with other Common Lisps yet. For now I am not aware of portability limitations with other implementations.

## Usage

To use, git clone the repo into your `~/quicklisp/local-projects` directory, then:

```lisp
(ql:quickload :cl-kraken)
(in-package :cl-kraken)
(assets)
(server-time)
```

## Tests

To run the test suite, the ROVE test library needs to be loaded.

```lisp
(ql:quickload :rove)
```

Then run the tests with either

```lisp
(asdf:test-system :cl-kraken)
```

or

```lisp
(rove:run :cl-kraken/tests)
```

## Author

* Jon Atack (jon@atack.com)

## Copyright

Copyright (c) 2019 Jon Atack (jon@atack.com)

# cl-kraken
An API wrapper for the Kraken cryptocurrency exchange in Common Lisp.

Currently a side project for learning Common Lisp: Language, packages system, unit testing, interfacing with outside libraries and the real world, and so on. Suggestions and pull requests welcome!

## Dependencies

CL-KRAKEN imports a small number of functions from the following Common Lisp libraries: DEXADOR, YASON, QURI, LOCAL-TIME, IRONCLAD, and CL-BASE64.

## Compatibility

Currently developed using SBCL 1.4.15. Not tested with other Common Lisps yet, but for now I am not aware of any compatibility limitations with current versions of other implementations. No SBCL-specific system calls are used.

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

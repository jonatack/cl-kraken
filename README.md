# cl-kraken
An API wrapper for the Kraken exchange in Common Lisp

Currently a side project for learning Common Lisp: Language, packages system, unit testing, interfacing with outside libraries and the real world, and so on. Suggestions and pull requests welcome!

To use, git clone the repo into your `~/quicklisp/local-projects` directory, then:

```lisp
(ql:quickload :cl-kraken)
(in-package :cl-kraken)
(assets)
```

To run the test suite:

```lisp
(ql:quickload :rove)
(rove:run :cl-kraken/tests)
```

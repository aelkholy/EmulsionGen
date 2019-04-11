# Simple SAT #
## Description ##
EmulsionGen is an assistive tool for the analysis and making of photographic, silver gelatin based emulsions.

## Getting Started ##
### Prerequisites ###

This project is written in [Haskell](https://docs.haskellstack.org/en/stable/README/). There are no other major prerequisites.

### Installing ###

For installing this project, first clone this repository,

```bash
$ git clone git@github.com:dlewissandy/simple-sat.git
$ cd simple-sat
```

At this point, the only thing left to do is to build it using stack,

```bash
$ stack build
```

### Running the tests ###
You can run the unit tests from the command line using the following commands:
```bash
$ stack test
```

The unit-tests can be invoked with options that fine tune the test behavior. For a complete list of testing options, you can invoke the following:
```bash
$ stack test --test-arguments "--help"
```

<!-- # Example Usage #
For example usage, please consult the code simple-sat source code in the app
folder, or execute the simple-sat application from the command line:

```bash
$ stack build
$ stack exec -- simple-sat queens4.logic
```
The example can be invoked with options that control the source file and output.
For a complete list of options, consult the program's help command:
```bash
$ stack exec -- simple-sat --help
``` -->

# Contributing #
Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of
conduct, and the process for submitting pull requests to us.

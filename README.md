# EmulsionGen #
## Description ##
EmulsionGen is an assistive tool for the analysis and making of photographic, silver gelatin based emulsions. Its main use cases are:
- Analyze and compare emulsions 
  - Infer expected properties of emulsion (Reasoning)
  - Check 'correctness' of emulsion (Solver)
- Generate emulsions with desired properties (Solver)

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

## Example usage

## Project TODOs ##
Cleanup inputs in the data JSON files. E.g. the user shouldn't have to specify the silver halides for each step in a solution, and there should be more optional arguments.

Better handling of pH

Multiple command line arguments

# Contributing #
Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of
conduct, and the process for submitting pull requests to us.

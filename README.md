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
$ git clone git@github.com:aelkholy/EmulsionGen.git
$ cd EmulsionGen
```

At this point, the only thing left to do is to build it using stack,

```bash
$ stack build
```

### Running the tests ###
Code changing too much to worry about tests for now.
<!-- You can run the unit tests from the command line using the following commands:
```bash
$ stack test
```

The unit-tests can be invoked with options that fine tune the test behavior. For a complete list of testing options, you can invoke the following:
```bash
$ stack test --test-arguments "--help"
```
 -->

## Example usage

```bash
$ stack exec EmulsionGen data/MowreyBrovira.json
```

## Project TODOs ##
-Support stirring
-ChemicalModifiers in something other than water

# Contributing #
Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of
conduct, and the process for submitting pull requests to us.

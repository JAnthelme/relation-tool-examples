# Relation Toolbox
> A lightweight relational algebra operators library.

## Installation

Install the Haskell Tool [Stack](https://docs.haskellstack.org/en/stable/README/).

Install the [relation-tool library](https://github.com/JAnthelme/relation-tool).

From the shell prompt:
```sh
git clone https://github.com/janthelme/relation-tool-examples.git
cd relation-tool-examples
stack build
stack exec basics-exe
```
There are 3 executable examples available: basics-exe, csv-exe and jcdate-exe. Each can be run via the command `stack exec <executable name>`.

- basics-exe : basic examples to get started.
- csv-exe : shows how to import a csv file into a relation and work on the data.
- jcdate-exe : examples from C.J. Date's book, "*An Introduction to Database Systems*" Eighth Edition.

## Documentation
Check the [relation-tool library README](https://github.com/JAnthelme/relation-tool/blob/master/README.md) and the [Haddock documentation](https://janthelme.github.io/relation-tool/)

## Built With

* [Stack](https://docs.haskellstack.org/en/stable/README/)

Github: https://github.com/JAnthelme/relation-toolbox-examples
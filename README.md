# Advent of Code 2023

https://adventofcode.com/2023

# Dev workflow

## New workflow

To continously build and test the library for a given day, say `lib/day2/*`,
execute the following command:
```
dune runtest -w --auto-promote lib/day2
```
or just
```
dune runtest -w --auto-promote
```
to do the same for all days.

## Old workflow

To continously build and run the executable for a given day, say `bin/day2.ml`,
execute the following:

```
dune exec -w bin/day2.exe
```

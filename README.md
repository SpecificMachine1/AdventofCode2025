# Advent of Code 2025

- trying r6rs this year using Chez scheme
- haven't really looked at r6rs libraries, I know the record
  system is different since it doesn't rely on syntax rules
- not sure how much I will try to use syntax-case, but am
   going to look at other features
- so far have noticed these differences:
    - no `read-line` `make-vector` `vector->string` `make-parameter` `parameterize`
    - `exact->inexact` instead of `inexact`
    - different record system

## aoc
    Helper libraries: (aoc char-list), (aoc data), (aoc file), (aoc matrix)

## bench
    Files to run benchmarks and results

## data
    Input files- repo only has example data and samples made to test libraries. Input data is .gitignored

## day
    Solutions for each day, as libraries like (day day01)

## test
    Files for testing and results

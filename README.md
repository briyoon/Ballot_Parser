# Ballot Parser

This is a ballot parser for a custom ballot language for CS460 (software engineering)

The parser is built in Haskell using the parsec library. It takes in a ballot built off of the grammar specified in ballot_grammar.txt and outputs it as json. an example is provided in examples/example_ballot.txt

Build: cabal build

Usage: ./ballot_parser < inputfile > outputfile

## TODO

- Double check that it is not sensitive to newlines inbetween lines

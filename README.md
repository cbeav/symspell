### Haskell SymSpell

The [SymSpell](https://github.com/wolfgarbe/SymSpell) algorithm pre-computes all
deletions within fixed distance of a source dictionary. Leveraging a symmetry in
edit distance, this can massively restrict the search space for spell checking
arbitrary.

This project implements the algorithm in Haskell, where it is much less verbose
than the original C#.

## Getting Started
We use [Stack](https://docs.haskellstack.org/en/stable/README/) as our build system.
To compile from source:
```sh
stack build
```

## Example Server
You can bring up the server with:
```sh
stack exec symspell-server
```

To issue a request:
```sh
curl -d '{"word":"hamburgre"}' -H 'Content-type: application/json' -X POST localhost:8080/top
```

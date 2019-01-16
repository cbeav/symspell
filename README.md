### Haskell SymSpell

The [SymSpell](https://github.com/wolfgarbe/SymSpell) algorithm pre-computes all
deletions within fixed distance of a source dictionary. Leveraging a symmetry in
edit distance, this can massively restrict the search space for spell checking
arbitrary.

This project implements the algorithm in Haskell, where it is much less verbose
than the original C#.

## Building
To compile with [Stack](https://docs.haskellstack.org/en/stable/README/):
```sh
stack build
```

## Test Server
You can bring up a simple REST API with:
```sh
stack exec symspell-server
```

To issue a request:
```sh
curl -d '{"word":"hamburgre"}' -H 'Content-type: application/json' -X POST localhost:8080/top
```

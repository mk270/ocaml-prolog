
A trivial Prolog interpreter in OCaml.

This tool optionally provides a *non-deterministic* implementation of
unification. The motivation for this is to generate dissimilar strings 
which satisfy a grammar, without all the strings looking practically
identical.

Here's some output from a *trivial* attempt at generating plausible
words from Tolkien's Quenya language, based on three minutes of
research on Wikipedia's claims about its phonotactic constraints:

nordordir
hyescar
tholduldel
tormus
ngworvus
cultolter
pallat


Derived from an original at: http://code.google.com/p/prologinterp/

Main changes:
  - ripped out the floating point data type
  - the original authors eschewed certain OCaml syntactic sugar => boilerplate

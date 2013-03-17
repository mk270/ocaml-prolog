OCaml-Prolog
============

A trivial Prolog interpreter in OCaml, originally by Karol Stosiek and
Szymon Fogiel.

This tool optionally provides a *non-deterministic* ordering of results of
evaluation. The motivation for this is to generate dissimilar strings 
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
  - remove the floating point data type / arithmetic
  - factor out conservative syntax in favour of more sugar, less boilerplate

Licensing
=========

This code is distributed under the terms of the GNU GPL v3

(see claim at: http://code.google.com/p/prologinterp/ )

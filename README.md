Logical Juggling
================

* specify periodic (juggling) patterns in first-order logic
* use a SAT solver to find a model (a pattern that realizes the specification)

Example
-------

Contents of a pattern description file (examples/simple.lj)
```
persons 4
period 3

forall throw t : height t == time 3 || height t == time 4

forall person p : exactly 1 throw t : p == from t && self t && height t == time 3

atmost 5 throw t : height t == time 4
```

Running the console version:
```
logical-juggling-console examples/simple.lj
```

will output this:
```
A : 3->D , 3    , 3->D , 
B : 3    , 3->C , 4->A , 
C : 4->B , 4->A , 3    , 
D : 3->C , 3    , 3->B , 
```

Model
-----

* a Place is a pair of Person and Time (all places are points inside a rectangle, one axis is time, other axis is person)
* a Pattern is a bijection between Places and Places (expressed by arrows in this rectangle.
  Bijection means that each place has exactly one incoming arrow (a catch), and exactly one outgoing arrow (a throw)).
* a single Throw `t` has four parameters:
    * `from t`: the person it comes from, 
    * `begin t`: the time it is thrown,
    * `to t`: the person it goes to,
    * `end t`: the time it is caught.

Restrictions
------------

* no physical modelling (no position of jugglers, no walk-around, no collision detection for objects)
* uniform time (all persons throw in-synch, no "french" patterns) with .5 displacement)
* all timing information is modulo period (e.g., if period is 3, there is no distinction between a 2 and a 5.
  Patterns will always be printed such that throws are >= 3)
* left/right-handedness is not represented, but you can think of each person using left and right hand strictly alternating.

These restrictions are not inherent to the method (they could be lifted, given some effort and determination) but we have to start somewhere.

Why
---

* it's an alternative to pattern generators with fixed-form queries like <http://prechacthis.org/>
* it's an application of things I research and teach (semantics of programming languages,
compiler construction, constraint programming, SAT encoding)

Of course (first-order) predicate logic is the most clear, most natural, etc., specification language.
The challenge is to make it "more practical" (for this application),
which presumably means to move it more towards natural language,
without destryoing its nice semantical and syntactical properties.

Discussion
----------

For instance, people might want to say, "each person throws (at least) one pass".
This is (conceptually) shorter than the exact formal specification
```
for each person p : exists throw t : p == from t && pass t
```
because of
* names (`p`, `t`) are implicit
* some quantifiers (`exists throw t`) are implicit 
* arguments (`from t`) are implicit

But try making a formal specification for such a language!
I think "Description Logics" <https://arxiv.org/abs/1201.4089> does something similar,
at least superficially.
They avoid variables not just for convenience in notation,
but mainly in order to stay in a decidable fragment -
which is irrelevant for us since we have a finite universe anyway.

I welcome proposals for improving the language. Use this project's issue tracker.

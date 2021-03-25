# Functional Programming in Scala Book group

## Chapter 6: Purely Functional State

### Core concepts: 
* Why care about internal state? 
    - Difficulty in testing due to difficulty in reproducing
* To recover referential transparency, make state change explicit 
    - return a new state along with the value you're generating
    - carrying state with you
* `RNG => (A, RNG)` or more generally `State => (A, State)`
* Combinators == functions whose input is a function  


### Exercises: 
6.2 & 6.1 Generating random numbers 

6.4 list of random numbers 

6.5 map

6.6 map2

6.8 flatMap

6.10 attempt a State class (We will go over it)

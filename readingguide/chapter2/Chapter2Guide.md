# Functional Programming in Scala Book group

## Chapter 2 

### Core concepts: 

* Higher Order Functions 
* Unit ()
* Polymorphism

### Exercises: 

2.2
```scala
def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = ??? 
```

2.3
```scala 
def curry[A,B,C](f: (A, B) => C): A => (B => C) = ??? 
```

2.5
```scala
def compose[A,B,C](f: B => C, g: A => B): A => C = ??? 
```

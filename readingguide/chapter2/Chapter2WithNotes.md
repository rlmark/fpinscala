# Functional Programming in Scala Book group

## Chapter 2 

### Core concepts: 

* Higher Order Functions 
    - functions are first class citizens in the language
    - functions are values, they can be assigned to variables, passed in as parameters to other functions, or returned as results of another function's execution
* Unit ()
    - A hint that the function is doing something side-effectful
* Polymorphism

### Exercises: 

2.2
```scala
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i == as.length - 1) {
        true
      } else {
        gt(as(i), as(i + 1)) && loop(i + 1) 
      }
    }
    if (as.length == 0) true
    else loop(0)
  } 
```

2.3
```scala 
 def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
   a: A => b: B => f(a,b)
 }
```

2.5
```scala
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def compose2[A,B,C](f: B => C, g: A => B): A => C = {
    def inner[A,C](a: A): C = {
      f(g(a)))
    }
  inner
  }
```

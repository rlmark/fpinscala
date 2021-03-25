# Functional Programming in Scala Book group

## Chapter 1 

### Core concepts: 

* Referential transparency 
    - Describes a property of purely functional code: any expression can be replaced with the result of that expression and the program will return the same output.
* Substitution model 
    - Enables us to reason about our code locally because the effects of code evaluation are not located outside a functions execution
* What are side effects
    - When a function changes state outside of local scope of the program. (Writing to disk, calling api, getting info from database) 
* But where do we run our side effects? 
    - At the edges of the world! 

Are the following code snippets referentially transparent? Why or why not? 

```
1.) 
```scala
BookTitle.parse(value) match {
  case Some(book) => book
  case None     => throw new IllegalArgumentException("bookTitle should not be empty")
}
```

2.) 
```scala
case Some(i) => Future(…) 
case None =>
  Future.failed(new IllegalArgumentException(s"Invalid query params: $params"))
```
There's some hand-waving to be done here about Future not being referentially transparent because it's eagerly evaluated.  

3.) 
```scala
def getRecord(databaseKey: DatabaseKey): Future[Option[DatabaseRecord]] = {
  database.run(
    searchQuery(databaseKey.id).result.headOption
  )
}
```
4.) 
```scala
def publishFormUpdate(beforeFormMaybe: Option[Form], afterForm: Form): Try[PublishResponse] = {
    val diff = FormDiff(before = beforeFormMaybe.orNull, after = afterForm)
    pubsubClient.publishSafe("myTopic", diff)
  }
```

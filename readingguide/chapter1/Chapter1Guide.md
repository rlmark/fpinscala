# Functional Programming in Scala Book group

## Chapter 1 

### Core concepts: 

* Referential transparency 
* Substitution model 
* But where do we run our side effects? 

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

# Stateモナド in Scala

あまりよくわかってない

```scala
val stack: State[Stack[Int], Int] =
  for {
    _ <- push(4)
    _ <- push(5)
    a <- pop
  } yield a

assert(stack.runState(List(3, 2, 1)) == (5, List(4, 3, 2, 1)))
```

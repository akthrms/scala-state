object MainApp extends App {

  import State._

  type Stack[A] = List[A]

  def pop: State[Stack[Int], Int] =
    for {
      stack <- get
      _ <-
        if (stack.isEmpty) sys.error("stack is empty.")
        else put(stack.tail)
    } yield stack.head

  def push(n: Int): State[Stack[Int], Unit] =
    for {
      stack <- get
      _ <- put(n :: stack)
    } yield ()

  val stack1: State[Stack[Int], Int] =
    for {
      _ <- push(4)
      _ <- push(5)
      a <- pop
    } yield a

  assert(stack1.runState(List(3, 2, 1)) == (5, List(4, 3, 2, 1)))

  val stack2: State[Stack[Int], Int] =
    push(4).flatMap { _ =>
      push(5).flatMap { _ =>
        pop.flatMap { a =>
          pure(a)
        }
      }
    }

  assert(stack2.runState(List(3, 2, 1)) == (5, List(4, 3, 2, 1)))
}

case class State[S, A](runState: S => (A, S)) {

  def flatMap[B](fn: A => State[S, B]): State[S, B] =
    State { s =>
      val (result, state) = runState(s)
      fn(result).runState(state)
    }

  def map[B](fn: A => B): State[S, B] =
    State { s =>
      val (result, state) = runState(s)
      State.pure(fn(result)).runState(state)
    }
}

object State {

  def get[S]: State[S, S] = State { s => (s, s) }

  def put[S](s: S): State[S, Unit] = State { _ => ((), s) }

  def modify[S](fn: S => S): State[S, S] = State { s => (s, fn(s)) }

  def pure[S, A](a: A): State[S, A] = State { s => (a, s) }
}

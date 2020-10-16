object MainApp extends App {}

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

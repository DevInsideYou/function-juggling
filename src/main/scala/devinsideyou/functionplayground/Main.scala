package devinsideyou
package functionplayground

import Library._
object Main extends App {
  println("─" * 100)

  def f(int: Int): String =
    int.toString

  def g(string: String): Option[Char] =
    string.headOption

  val ff: Int => String = f
  val gg: String => Option[Char] = g

  def combine1(int: Int): Option[Char] =
    g(f(int))

  def combine2(int: Int): Option[Char] =
    (gg compose f)(int)

  def combine2a(int: Int): Option[Char] =
    (gg after f)(int)

  def combine2b(int: Int): Option[Char] =
    (gg <-- f)(int)

  def combine3(int: Int): Option[Char] =
    (ff andThen g)(int)

  def combine3a(int: Int): Option[Char] =
    (ff --> g)(int)

  def combine4(int: Int): Option[Char] =
    int pipe f pipe g

  def combine4a(int: Int): Option[Char] =
    int --> f --> g

  Seq[Int => Option[Char]](
    combine1,
    combine2,
    combine2a,
    combine2b,
    combine3,
    combine3a,
    combine4,
    combine4a
  ).map(f => f(1337)).foreach(println)

  def plus(x: Int, y: Int): Int =
    x + y

  println(plus(1, 2))

  def plusCurried(x: Int)(y: Int): Int =
    x + y

  println(plusCurried(1)(2))

  def plusCurriedManually(x: Int): Int => Int =
    y => x + y

  println(plusCurriedManually(1)(2))

  def plusCurriedEvenMoreManually: Int => Int => Int =
    x => y => x + y

  println(plusCurriedEvenMoreManually(1)(2))

  def plusCurriedAutomatically: Int => Int => Int =
    (plus _).curried

  println(plusCurriedAutomatically(1)(2))

  def plusUncurried: (Int, Int) => Int =
    Function.uncurried(plusCurried _)

  println(plusUncurried(1, 2))

  final case class Plus(x: Int) extends (Int => Int) {
    final override def apply(y: Int): Int =
      x + y
  }

  println(Plus(1)(2))

  def plusTupled(tuple: (Int, Int)): Int = tuple match {
    case (x, y) => x + y
  }

  println(plusTupled((1, 2)))
  println(plusTupled(1 -> 2))

  val plusTupledFunc: ((Int, Int)) => Int = {
    case (x, y) => x + y
  }

  println(plusTupledFunc((1, 2)))

  val plusTupledAutomatically: ((Int, Int)) => Int =
    (plus _).tupled

  println(plusTupledAutomatically((1, 2)))

  val plusUntupled: (Int, Int) => Int =
    Function.untupled(plusTupled _)

  println(plusUntupled(1, 2))

  final case class PlusTupled(tuple: (Int, Int)) extends (() => Int) {
    final override def apply: Int = tuple match {
      case (x, y) => x + y
    }
  }

  println(PlusTupled((1, 2)).apply)

  println(1 -> 2 --> plusTupledAutomatically)

  final case class Person(name: String, startedOnDayOfYear: Int)

  val isEven: Int => Boolean =
    _ % 2 == 0

  val didPersonStartOnDayOfYear: Person => Boolean =
    isEven <<- (_.startedOnDayOfYear)

  didPersonStartOnDayOfYear(Person("Alice", 1)) --> println
  didPersonStartOnDayOfYear(Person("Bob", 2)) --> println

  val aliceWithStartedOnDayOfYear: Int => Person =
    // Person("Alice", _)
    Person curried "Alice"

  aliceWithStartedOnDayOfYear(1) --> println
  aliceWithStartedOnDayOfYear(2) --> println

  val createPersonStepByStep: String => Int => Person =
    Person.curried

  createPersonStepByStep("Alice")(1) --> println
  createPersonStepByStep("Bob")(2) --> println

  println(Person.curried("Alice")(1))
  println(Person.curried("Bob")(2))

  println(Person.curried.flipped(1)("Alice"))
  println(Person.curried.flipped(2)("Bob"))

  val a: Seq[Option[Char]] = 1 to 3 map f map gg tap println
  val b: Seq[Option[Char]] = 1 to 3 map ff ->> g tap println
  val c: Seq[Option[Char]] = 1 to 3 map (gg <<- ff) tap println

  implicit def MonadForOption[A]: Monad[Option] = new Monad[Option] {
    def pure[B](b: B): Option[B] = Some(b)
    def map[B, C](fa: Option[B])(f: B => C): Option[C] = fa.map(f)
    def flatten[B](ffb: Option[Option[B]]): Option[B] = ffb.flatten
  }

  val fKleisli: Int => Option[String] =
    // ff map the[Applicative[Option]].pure
    ff map (_.pure[Option])

  val fKleisliAndThenG: Int => /* through Option[String]*/ Option[Char] =
    fKleisli >=> g

  println(fKleisliAndThenG(1337))

  val renderedPerson: Boolean => Person => String = {
    startedOnEvenDay => person =>
      val didOrDidNotStart: String =
        if (startedOnEvenDay)
          "started"
        else
          "did not start"

      s"$person $didOrDidNotStart on even day"
  }

  List("Alpha", "Bravo", "Charlie")
    .zip(1 to 3)
    .map(Person.tupled)
    .map(didPersonStartOnDayOfYear flatMap renderedPerson)
    .foreach(println)

  println("─" * 100)

  List("Alpha", "Bravo", "Charlie")
    .zip(1 to 3)
    .map(Person.tupled)
    .map(p => renderedPerson(didPersonStartOnDayOfYear(p))(p))
    .foreach(println)

  println("─" * 100)
}

// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.essentials

import java.io.File
import java.time.LocalDate

object types {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // List all values of the type `Unit`.
  //
  val UnitValues: Set[Unit] = Set(())

  //
  // EXERCISE 2
  //
  // List all values of the type `Nothing`.
  //
  val NothingValues: Set[Nothing] = Set()

  //
  // EXERCISE 3
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: Set[Boolean] = Set(true, false)

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: Set[Either[Unit, Boolean]] = Set(Left(()), Right(true), Right(false))

  //
  // EXERCISE 5
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: Set[(Boolean, Boolean)] = Set((true, true), (true, false), (false, true), (false, false))

  //
  // EXERCISE 6
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: Set[Either[Either[Unit, Unit], Unit]] =
    Set(Left(Left(())), Left(Right(())), Right(()))

  //
  // EXERCISE 7
  //
  // Given:
  // A = { true, false }
  // B = { "red", "green", "blue" }
  //
  // List all the elements in `A * B`.
  //
  val AProductB: Set[(Boolean, String)] =
    Set((true, "red"), (true, "green"), (true, "blue"), (false, "red"), (false, "green"), (false, "blue"))

  //
  // EXERCISE 8
  //
  // Given:
  // A = { true, false }
  // B = { "red", "green", "blue" }
  //
  // List all the elements in `A + B`.
  //
  val ASumB: Set[Either[Boolean, String]] = Set(Left(true), Left(false), Right("red"), Right("green"), Right("blue"))

  //
  // EXERCISE 9
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person1 = (Int, String)
  final case class Person2(age: Int, name: String)

  //
  // EXERCISE 10
  //
  // Prove that `A * 1` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to1[A](t: (A, Unit)): A = t._1
  def from1[A](a: A): (A, Unit) = (a, ())

  //
  // EXERCISE 11
  //
  // Prove that `A * 0` is equivalent to `0` by implementing the following two
  // functions.
  //
  def to2[A](t: (A, Nothing)): Nothing = t._2
  def from2[A](n: Nothing): (A, Nothing) = n

  //
  // EXERCISE 12
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or the identifier of a person (a name).
  //
  type Identifier1 = Either[Int, String]
  sealed trait Identifier2
  final case class RobotId(number: Int) extends Identifier2
  final case class PersonId(name: String) extends Identifier2

  //
  // EXERCISE 13
  //
  // Prove that `A + 0` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to3[A](t: Either[A, Nothing]): A =
    t match {
      case Left(a) => a
      case Right(n) => n
    }

  def from3[A](a: A): Either[A, Nothing] = Left(a)

  //
  // EXERCISE 14
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  final case class CreditCard(number: Long, expirationDate: LocalDate, securityCode: String)

  //
  // EXERCISE 15
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  sealed trait PaymentMethod
  case object Card extends PaymentMethod
  case object BankAccount extends PaymentMethod
  case object Cryptocurrency extends PaymentMethod

  //
  // EXERCISE 16
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, and employment date.
  //
  final case class Employee(title: String, salary: Double, name: String, employmentDate: LocalDate)

  //
  // EXERCISE 17
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // piece on a chess board, which could be a pawn, rook, bishop, knight,
  // queen, or king.
  //
  sealed trait ChessPiece
  case object Pawn extends ChessPiece
  case object Rook extends ChessPiece
  case object Bishop extends ChessPiece
  case object Knight extends ChessPiece
  case object Queen extends ChessPiece
  case object King extends ChessPiece

  //
  // EXERCISE 18
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  type GameWorld = ???
}

object functions {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Convert the following non-function into a function.
  //
  def parseInt1(s: String): Int = s.toInt
  def parseInt2(s: String): Option[Int] = try Some(s.toInt) catch { case _: NumberFormatException => None }

  //
  // EXERCISE 2
  //
  // Convert the following non-function into a function.
  //
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit = arr.update(i, f(arr(i)))
  def arrayUpdate2[A](arr: Array[A], i: Int, f: A => A): Option[Array[A]] =
    if (i < 0 || i >= arr.length) None
    else {
      val clone = arr.clone()
      clone(i) = f(arr(i))
      Some(clone)
    }

  //
  // EXERCISE 3
  //
  // Convert the following non-function into a function.
  //
  def divide1(a: Int, b: Int): Int = a / b
  def divide2(a: Int, b: Int): Option[Int] = if (b == 0) None else Some(a / b)

  //
  // EXERCISE 4
  //
  // Convert the following non-function into a function.
  //
  var id = 0
  def freshId1(): Int = {
    val newId = id
    id += 1
    newId
  }
  def freshId2(id: Int): (Int, Int) = (id, id+1)

  //
  // EXERCISE 5
  //
  // Convert the following non-function into a function.
  //
  import java.time.LocalDateTime
  def afterOneHour1: LocalDateTime = LocalDateTime.now.plusHours(1)
  def afterOneHour2(now: LocalDateTime): LocalDateTime = now.plusHours(1)

  //
  // EXERCISE 6
  //
  // Convert the following non-function into function.
  //
  def head1[A](as: List[A]): A = {
    if (as.length == 0) println("Oh no, it's impossible!!!")
    as.head
  }
  def head2[A](as: List[A]): Either[String, A] =
    as.headOption.fold[Either[String, A]](Left("Oh no, it's impossible!!!"))(Right(_))

  //
  // EXERCISE 7
  //
  // Convert the following non-function into a function.
  //
  trait Account
  trait Processor {
    def charge(account: Account, amount: Double): Unit
  }
  case class Coffee() {
    val price = 3.14
  }
  def buyCoffee1(processor: Processor, account: Account): Coffee = {
    val coffee = Coffee()
    processor.charge(account, coffee.price)
    coffee
  }
  final case class Charge(account: Account, amount: Double)
  def buyCoffee2(account: Account): (Coffee, Charge) = {
    val c = Coffee()
    (c, Charge(account, c.price))
  }

  //
  // EXERCISE 8
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def printLine(line: String): Unit = ()

  //
  // EXERCISE 9
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def readLine: String = ""

  //
  // EXERCISE 10
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def systemExit(code: Int): Unit = ()

  //
  // EXERCISE 11
  //
  // Rewrite the following non-function `printer1` into a pure function, which
  // could be used by pure or impure code.
  //
  def printer1(): Unit = {
    println("Welcome to the help page!")
    println("To list commands, type `commands`.")
    println("For help on a command, type `help <command>`")
    println("To exit the help page, type `exit`.")
  }
  def printer2[A](println: String => A, combine: (A, A) => A): A = {
    val a1 = println("Welcome to the help page!")
    val a2 = println("To list commands, type `commands`.")
    val a3 = println("For help on a command, type `help <command>`")
    val a4 = println("To exit the help page, type `exit`.")

    combine(a1, combine(a2, combine(a3, a4)))
  }

  //
  // EXERCISE 12
  //
  // Create a purely-functional drawing library that is equivalent in
  // expressive power to the following procedural library.
  //
  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }
  def draw1(size: Int): Draw = new Draw {
    val canvas = Array.fill(size, size)(false)
    var x = 0
    var y = 0

    def goLeft(): Unit = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit = y += 1
    def goDown(): Unit = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): List[List[Boolean]] =
      canvas.map(_.toList).toList
  }
  def draw2(size: Int /* ... */): ??? = ???
}

object higher_order {
  //
  // EXERCISE 1
  //
  // Implement the following higher-order function.
  //
  def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = a => (f(a), g(a))

  //
  // EXERCISE 2
  //
  // Implement the following higher-order function.
  //
  def cross[A, B, C, D](f: A => B, g: C => D): (A, C) => (B, D) = (a, c) => (f(a), g(c))

  //
  // EXERCISE 3
  //
  // Implement the following higher-order function.
  //
  def either[A, B, C](f: A => B, g: C => B): Either[A, C] => B = {
    case Left(a) => f(a)
    case Right(c) => g(c)
  }

  //
  // EXERCISE 4
  //
  // Implement the following higher-order function.
  //
  def choice[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] = {
    case Left(a) => Left(f(a))
    case Right(c) => Right(g(c))
  }

  //
  // EXERCISE 5
  //
  // Implement the following higher-order function.
  //
  def compose[A, B, C](f: B => C, g: A => B): A => C = f compose g

  //
  // EXERCISE 6
  //
  // Implement the following higher-order function. After you implement
  // the function, interpret its meaning.
  //
  def alt[E1, E2, A, B](l: Parser[E1, A], r: E1 => Parser[E2, B]): Parser[E2, Either[A, B]] =
    Parser { in =>
      l.run(in) match {
        case Left(e1) =>
          r(e1).run(in) match {
            case Left(e2) => Left(e2)
            case Right((in, b)) => Right((in, Right(b)))
          }
        case Right((in, a)) => Right((in, Left(a)))
      }
    }

  final case class Parser[+E, +A](run: String => Either[E, (String, A)])
  object Parser {
    final def fail[E](e: E): Parser[E, Nothing] = Parser(_ => Left(e))

    final def point[A](a: => A): Parser[Nothing, A] = Parser(input => Right((input, a)))

    final def char[E](e: E): Parser[E, Char] =
      Parser(input =>
        if (input.length == 0) Left(e)
        else Right((input.drop(1), input.charAt(0))))
  }
}

object poly_functions {
  //
  // EXERCISE 1
  //
  // Create a polymorphic function of two type parameters `A` and `B` called
  // `snd` that returns the second element out of any pair of `A` and `B`.
  //
  object snd {
    def apply[A, B](t: (A, B)): B = t._2
  }
  snd((1, "foo")) // "foo"
  snd((true, List(1, 2, 3))) // List(1, 2, 3)

  //
  // EXERCISE 2
  //
  // Create a polymorphic function called `repeat` that can take any
  // function `A => A`, and apply it repeatedly to a starting value
  // `A` the specified number of times.
  //
  object repeat {
    @annotation.tailrec
    def apply[A](n: Int)(a: A, f: A => A): A = if (n == 0) a else repeat(n-1)(f(a), f)
  }
  repeat[   Int](100)( 0, _ +   1) // 100
  repeat[String]( 10)("", _ + "*") // "**********"

  //
  // EXERCISE 3
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample1[A, B](a: A, b: B): Either[A, B] = Left(a)
  val countExample1Answer = 2

  //
  // EXERCISE 4
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample2[A, B](f: A => B, g: A => B, a: A): B = f(a)
  val countExample2Answer = 2

  //
  // EXERCISE 5
  //
  // Implement the function `groupBy1`.
  //
  val Data = "poweroutage;2018-09-20;level=20" :: Nil
  val By: String => String = (data: String) => data.split(";")(1)

  val Reducer: (String, List[String]) => String =
    (date, events) => "On date " + date + ", there were " + events.length + " power outages"

  val Expected = Map("2018-09-20" -> "On date 2018-09-20, there were 1 power outages")

  def groupBy1(l: List[String], by: String => String)(reducer: (String, List[String]) => String): Map[String, String] =
    l.groupBy(by).map { case (k, v) => k -> reducer(k, v) }

  // groupBy1(Data, By)(Reducer) == Expected

  //
  // EXERCISE 6
  //
  // Make the function `groupBy1` as polymorphic as possible and implement
  // the polymorphic function. Compare to the original.
  //
  object groupBy2 {
    def apply[A, B, C](l: List[A], by: A => B)(reducer: (B, List[A]) => C): Map[B, C] =
      l.groupBy(by).map { case (k, v) => k -> reducer(k, v) }
  }
}

object higher_kinded {
  type ?? = Nothing
  type ???[A] = Nothing
  type ????[A, B] = Nothing
  type ?????[F[_]] = Nothing

  trait `* => *`[F[_]]
  trait `[*, *] => *`[F[_, _]]
  trait `(* => *) => *`[T[_[_]]]

  //
  // EXERCISE 1
  //
  // Identify a type constructor that takes one type parameter of kind `*`
  // (i.e. has kind `* => *`), and place your answer inside the square brackets.
  //
  type Answer1 = `* => *`[Option]

  //
  // EXERCISE 2
  //
  // Identify a type constructor that takes two type parameters of kind `*` (i.e.
  // has kind `[*, *] => *`), and place your answer inside the square brackets.
  //
  type Answer2 = `[*, *] => *`[Either]

  //
  // EXERCISE 3
  //
  // Create a new type called `Answer3` that has kind `*`.
  //
  trait Answer3

  //
  // EXERCISE 4
  //
  // Create a trait with kind `[*, *, *] => *`.
  //
  trait Answer4[A, B, C]

  //
  // EXERCISE 5
  //
  // Create a new type that has kind `(* => *) => *`.
  //
  type NewType1[_[_]]
  type Answer5 = `(* => *) => *`[NewType1]

  //
  // EXERCISE 6
  //
  // Create a trait with kind `[* => *, (* => *) => *] => *`.
  //
  trait Answer6[F[_], G[_[_]]]

  //
  // EXERCISE 7
  //
  // Create an implementation of the trait `CollectionLike` for `List`.
  //
  trait CollectionLike[F[_]] {
    def empty[A]: F[A]

    def cons[A](a: A, as: F[A]): F[A]

    def uncons[A](as: F[A]): Option[(A, F[A])]

    final def singleton[A](a: A): F[A] =
      cons(a, empty[A])

    final def append[A](l: F[A], r: F[A]): F[A] =
      uncons(l) match {
        case Some((l, ls)) => append(ls, cons(l, r))
        case None => r
      }

    final def filter[A](fa: F[A])(f: A => Boolean): F[A] =
      bind(fa)(a => if (f(a)) singleton(a) else empty[A])

    final def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      uncons(fa) match {
        case Some((a, as)) => append(f(a), bind(as)(f))
        case None => empty[B]
      }

    final def fmap[A, B](fa: F[A])(f: A => B): F[B] = {
      val single: B => F[B] = singleton[B](_)

      bind(fa)(f andThen single)
    }
  }
  val ListCollectionLike: CollectionLike[List] = new CollectionLike[List] {
    override def empty[A]: List[A] = Nil

    override def cons[A](a: A, as: List[A]): List[A] = a :: as

    override def uncons[A](as: List[A]): Option[(A, List[A])] =
      as match {
        case Nil => None
        case h :: t => Some((h, t))
      }
  }

  //
  // EXERCISE 8
  //
  // Implement `Sized` for `List`.
  //
  trait Sized[F[_]] {
    // This method will return the number of `A`s inside `fa`.
    def size[A](fa: F[A]): Int
  }
  val ListSized: Sized[List] = new Sized[List] {
    override def size[A](fa: List[A]): Int = fa.size
  }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to `String`.
  //
  val MapStringSized: Sized[Map[String, ?]] = MapSized2[String]

  //
  // EXERCISE 10
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to a user-defined type parameter.
  //
  def MapSized2[K]: Sized[Map[K, ?]] = new Sized[Map[K, ?]] {
    override def size[A](fa: Map[K, A]): Int = fa.size
  }

  //
  // EXERCISE 11
  //
  // Implement `Sized` for `Tuple3`.
  //
  def Tuple3Sized[C, B]: Sized[(C, B, ?)] = new Sized[(C, B, ?)] {
    override def size[A](fa: (C, B, A)): Int = 1
  }
}

object tc_motivating {
  /*
  A type class is a tuple of three things:

  1. A set of types and / or type constructors.
  2. A set of operations on values of those types.
  3. A set of laws governing the operations.

  A type class instance is an instance of a type class for a given
  set of types.

  */
  /**
   * All implementations are required to satisfy the transitivityLaw.
   *
   * Transitivity Law:
   * forall a b c.
   *   lt(a, b) && lt(b, c) ==
   *     lt(a, c) || (!lt(a, b) || !lt(b, c))
   */
  trait LessThan[A] {
    def lt(l: A, r: A): Boolean

    final def transitivityLaw(a: A, b: A, c: A): Boolean =
      (lt(a, b) && lt(b, c) == lt(a, c)) ||
      (!lt(a, b) || !lt(b, c))
  }
  implicit class LessThanSyntax[A](l: A) {
    def < (r: A)(implicit A: LessThan[A]): Boolean = A.lt(l, r)
    def >= (r: A)(implicit A: LessThan[A]): Boolean = !A.lt(l, r)
  }
  object LessThan {
    def apply[A](implicit A: LessThan[A]): LessThan[A] = A

    implicit val LessThanInt: LessThan[Int] = new LessThan[Int] {
      def lt(l: Int, r: Int): Boolean = l < r
    }
    implicit def LessThanList[A: LessThan]: LessThan[List[A]] = new LessThan[List[A]] {
      def lt(l: List[A], r: List[A]): Boolean =
        (l, r) match {
          case (Nil, Nil) => false
          case (Nil, _) => true
          case (_, Nil) => false
          case (l :: ls, r :: rs) => l < r && lt(ls, rs)
        }
    }
  }

  def sort[A: LessThan](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort(lessThan) ++ List(x) ++ sort(notLessThan)
  }

  sort(List(1, 2, 3))
  sort(List(List(1, 2, 3), List(9, 2, 1), List(1, 2, 9)))
}

object typeclasses {
  /**
   * {{
   * Reflexivity:   a ==> equals(a, a)
   *
   * Transitivity:  equals(a, b) && equals(b, c) ==>
   *                equals(a, c)
   *
   * Symmetry:      equals(a, b) ==> equals(b, a)
   * }}
   */
  trait Eq[A] {
    def equals(l: A, r: A): Boolean
  }
  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq

    implicit val EqInt: Eq[Int] = new Eq[Int] {
      def equals(l: Int, r: Int): Boolean = l == r
    }
    implicit def EqList[A: Eq]: Eq[List[A]] =
      new Eq[List[A]] {
        def equals(l: List[A], r: List[A]): Boolean =
          (l, r) match {
            case (Nil, Nil) => true
            case (Nil, _) => false
            case (_, Nil) => false
            case (l :: ls, r :: rs) =>
              Eq[A].equals(l, r) && equals(ls, rs)
          }
      }
  }
  implicit class EqSyntax[A](val l: A) extends AnyVal {
    def === (r: A)(implicit eq: Eq[A]): Boolean =
      eq.equals(l, r)
  }

  //
  // Scalaz 7 Encoding
  //
  sealed trait Ordering
  case object EQUAL extends Ordering
  case object LT extends Ordering
  case object GT extends Ordering
  object Ordering {
    implicit val OrderingEq: Eq[Ordering] = new Eq[Ordering] {
      def equals(l: Ordering, r: Ordering): Boolean =
        (l, r) match {
          case (EQUAL, EQUAL) => true
          case (LT, LT) => true
          case (GT, GT) => true
          case _ => false
        }
    }
  }

  trait Ord[A] {
    def compare(l: A, r: A): Ordering

    final def eq(l: A, r: A): Boolean = compare(l, r) == EQUAL
    final def lt(l: A, r: A): Boolean = compare(l, r) == LT
    final def lte(l: A, r: A): Boolean = lt(l, r) || eq(l, r)
    final def gt(l: A, r: A): Boolean = compare(l, r) == GT
    final def gte(l: A, r: A): Boolean = gt(l, r) || eq(l, r)

    final def transitivityLaw1(a: A, b: A, c: A): Boolean =
      (lt(a, b) && lt(b, c) == lt(a, c)) ||
      (!lt(a, b) || !lt(b, c))

    final def transitivityLaw2(a: A, b: A, c: A): Boolean =
      (gt(a, b) && gt(b, c) == gt(a, c)) ||
      (!gt(a, b) || !gt(b, c))

    final def equalityLaw(a: A, b: A): Boolean =
      (lt(a, b) && gt(a, b) == eq(a, b)) ||
      (!lt(a, b) || !gt(a, b))
  }
  object Ord {
    def apply[A](implicit A: Ord[A]): Ord[A] = A

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT else if (l > r) GT else EQUAL
    }
  }
  implicit class OrdSyntax[A](val l: A) extends AnyVal {
    def =?= (r: A)(implicit A: Ord[A]): Ordering =
      A.compare(l, r)

    def < (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), LT)

    def <= (r: A)(implicit A: Ord[A]): Boolean =
      (l < r) || (this === r)

    def > (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), GT)

    def >= (r: A)(implicit A: Ord[A]): Boolean =
      (l > r) || (this === r)

    def === (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), EQUAL)

    def !== (r: A)(implicit A: Ord[A]): Boolean =
      !Eq[Ordering].equals(A.compare(l, r), EQUAL)
  }
  case class Person(age: Int, name: String)
  object Person {
    implicit val OrdPerson: Ord[Person] = new Ord[Person] {
      def compare(l: Person, r: Person): Ordering =
        if (l.age < r.age) LT else if (l.age > r.age) GT
        else if (l.name < r.name) LT else if (l.name > r.name) GT
        else EQUAL
    }
    implicit val EqPerson: Eq[Person] = new Eq[Person] {
      def equals(l: Person, r: Person): Boolean =
        l == r
    }
  }

  //
  // EXERCISE 1
  //
  // Write a version of `sort1` called `sort2` that uses the polymorphic `List`
  // type, and which uses the `Ord` type class, including the compare syntax
  // operator `=?=` to compare elements.
  //
  def sort1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort1(lessThan) ++ List(x) ++ sort1(notLessThan)
  }
  def sort2[A: Ord](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort2(lessThan) ++ List(x) ++ sort2(notLessThan)
  }

  //
  // EXERCISE 2
  //
  // Create a data structure and an instance of this type class for the data
  // structure.
  //
  trait PathLike[A] {
    def child(parent: A, name: String): A

    def parent(node: A): Option[A]

    def root: A
  }
  object PathLike {
    def apply[A](implicit A: PathLike[A]): PathLike[A] = A
  }
  sealed trait MyPath
  case object Root extends MyPath
  final case class ChildOf(path: MyPath, name: String) extends MyPath

  implicit val MyPathPathLike: PathLike[MyPath] =
    new PathLike[MyPath] {
      def child(parent: MyPath, name: String): MyPath = ChildOf(parent, name)

      def parent(node: MyPath): Option[MyPath] =
        node match {
          case Root => None
          case ChildOf(p, _) => Some(p)
        }

      def root: MyPath = Root
    }

  //
  // EXERCISE 3
  //
  // Create an instance of the `PathLike` type class for `java.io.File`.
  //
  implicit val FilePathLike: PathLike[File] = new PathLike[File] {
    override def child(parent: File, name: String): File = new File(parent, name)

    override def parent(node: File): Option[File] = Option(node.getParentFile)

    override def root: File = new File("/")
  }

  //
  // EXERCISE 4
  //
  // Create two laws for the `PathLike` type class.
  //
  trait PathLikeLaws[A] extends PathLike[A] {
    def law1: Boolean = parent(root).isEmpty

    def law2(node: A, name: String, assertEquals: (A, A) => Boolean): Boolean =
      parent(child(node, name)).fold(false)(p => assertEquals(node, p))
  }

  //
  // EXERCISE 5
  //
  // Create a syntax class for path-like values with a `/` method that descends
  // into the given named node.
  //
  implicit class PathLikeSyntax[A](a: A) {
    def / (name: String)(implicit A: PathLike[A]): A = A.child(a, name)

    def parent(implicit A: PathLike[A]): Option[A] = A.parent(a)
  }
  def root[A: PathLike]: A = PathLike[A].root

  root[MyPath] / "foo" / "bar" / "baz" // MyPath
  (root[MyPath] / "foo").parent        // Option[MyPath]

  //
  // EXERCISE 6
  //
  // Create an instance of the `Filterable` type class for `List`.
  //
  trait Filterable[F[_]] {
    def filter[A](fa: F[A], f: A => Boolean): F[A]
  }
  object Filterable {
    def apply[F[_]](implicit F: Filterable[F]): Filterable[F] = F
  }
  implicit val FilterableList: Filterable[List] = new Filterable[List] {
    override def filter[A](fa: List[A], f: A => Boolean): List[A] = fa.filter(f)
  }

  //
  // EXERCISE 7
  //
  // Create a syntax class for `Filterable` that lets you call `.filterWith` on any
  // type for which there exists a `Filterable` instance.
  //
  implicit class FilterableSyntax[F[_], A](fa: F[A]) {
    def filterWith(f: A => Boolean)(implicit F: Filterable[F]): F[A] = F.filter(fa, f)
  }
  List(1, 2, 3).filterWith(_ == 2)

  //
  // EXERCISE 8
  //
  // Create an instance of the `Collection` type class for `List`.
  //
  trait Collection[F[_]] {
    def empty[A]: F[A]
    def cons[A](a: A, as: F[A]): F[A]
    def uncons[A](fa: F[A]): Option[(A, F[A])]
  }
  object Collection {
    def apply[F[_]](implicit F: Collection[F]): Collection[F] = F
  }
  implicit val ListCollection: Collection[List] = new Collection[List] {
    override def empty[A]: List[A] = Nil

    override def cons[A](a: A, as: List[A]): List[A] = a :: as

    override def uncons[A](fa: List[A]): Option[(A, List[A])] =
      fa match {
        case x :: xs => Some((x, xs))
        case Nil => None
      }
  }

  val example = Collection[List].cons(1, Collection[List].empty)

  //
  // EXERCISE 9
  //
  // Create laws for the `Collection` type class.
  //
  trait CollectionLaws[F[_]] extends Collection[F] {
    def duality[A](a: A): Boolean =
      uncons(cons(a, empty[A])).fold(false) { case (h, t) => h == a && t == empty[A] }
  }

  //
  // EXERCISE 10
  //
  // Create syntax for values of any type that has `Collection` instances.
  // Specifically, add an `uncons` method to such types.
  //
  implicit class CollectionSyntax[F[_], A](fa: F[A]) {
    def cons(a: A)(implicit F: Collection[F]): F[A] = F.cons(a, fa)

    def uncons(implicit F: Collection[F]): Option[(A, F[A])] = F.uncons(fa)
  }
  def empty[F[_]: Collection, A]: F[A] = Collection[F].empty[A]

  List(1, 2, 3).uncons // Some((1, List(2, 3)))
}

/**
 * TYPECLASSES
 * 
 * Scala 3 introduces direct support for typeclasses using contextual features of the language.
 * Typeclasses provide a way to abstract over similar data types, without having to change the 
 * inheritance hierarchy of those data types, providing the power of "mixin" interfaces, but 
 * with additional flexibility that plays well with third-party data types.
 */

import typeclass_basics.PrettyPrint
import typeclass_graduation.PrimType
import typeclass_graduation.EncodeData

object typeclass_basics:
  trait PrettyPrint[-A]:
    extension (a: A) def prettyPrint: String

  given PrettyPrint[String] with
    extension (a: String) def prettyPrint: String = a

  "foo".prettyPrint

  final case class Person(name: String, age: Int)

  /**
   * EXERCISE 1
   * 
   * With the help of the `given` keyword, create an instance of the `PrettyPrint` typeclass for the 
   * data type `Person` that renders the person in a pretty way.
   */
  given PrettyPrint[Person] with
    extension (person: Person) def prettyPrint: String =
      s"Person(name: ${person.name}, age: ${person.age})"

  /**
   * EXERCISE 2
   * 
   * With the help of the `given` keyword, create a **named* instance of the `PrettyPrint` typeclass 
   * for the data type `Int` that renders the integer in a pretty way.
   */
  given intPrettyPrint: PrettyPrint[Int] with
    extension (n: Int) def prettyPrint: String = n.toString

  /**
   * EXERCISE 3
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `String`.
   */
  val stringPrettyPrint: PrettyPrint[String] = summon[PrettyPrint[String]]

  /**
   * EXERCISE 4
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `Int`.
   */
  val doIntPrettyPrint: PrettyPrint[Int] = summon[PrettyPrint[Int]]

  /**
   * EXERCISE 5
   * 
   * With the help of the `using` keyword, create a method called `prettyPrintIt` that, for any type 
   * `A` for which a `PrettyPrint` instance exists, can both generate a pretty-print string, and 
   * print it out to the console using `println`.
   */
  def prettyPrintIt[A](a: A)(using PrettyPrint[A]): Unit =
    println(summon[PrettyPrint[A]].prettyPrint(a))

  /**
   * EXERCISE 6
   * 
   * With the help of both `given` and `using`, create an instance of the `PrettyPrint` type class
   * for a generic `List[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given [A](using PrettyPrint[A]): PrettyPrint[List[A]] with
    extension (a: List[A]) def prettyPrint: String =
      a.map(prettyPrintIt(_)).mkString("\n")

  /**
   * EXERCISE 7
   * 
   * With the help of both `given` and `using`, create a **named** instance of the `PrettyPrint` 
   * type class for a generic `Vector[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given vectorPrettyPrint[A](using PrettyPrint[A]): PrettyPrint[Vector[A]] with
    extension (a: Vector[A]) def prettyPrint: String =
      a.map(prettyPrintIt(_)).mkString("\n")

  import scala.CanEqual._ 

object given_scopes:
  trait Hash[-A]:
    extension (a: A) def hash: Int

  object Hash:
    given Hash[Int] = _.hashCode
    given Hash[Long] = _.hashCode
    given Hash[Float] = _.hashCode
    given Hash[Double] = _.hashCode

  object givens {
    /**
     * EXERCISE 1
     * 
     * Import the right given into the scope (but ONLY this given) so the following code will compile.
     */
    import Hash.given Hash[Int]
    12.hash

    /**
     * EXERCISE 2
     * 
     * Import the right given into the scope (but ONLY this given) so the following code will compile.
     */
    import Hash.given Hash[Double]
    12.123.hash   
  }

  object usings:
    /**
     * EXERCISE 3
     * 
     * Adding the right `using` clause to this function so that it compiles.
     */
    def hashingInts(using Hash[Int]) = 12.hash 

    /**
     * EXERCISE 4
     * 
     * Adding the right `using` clause to this function so that it compiles.
     */
    def hashingDoubles(using Hash[Double]) = 12.123.hash

  
object typeclass_derives:
  /**
   * EXERCISE 1
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Color`.
   */
  enum Color derives CanEqual:
    case Red 
    case Green 
    case Blue

  /**
   * EXERCISE 2
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Person`.
   */
  final case class Person(name: String, age: Int) derives CanEqual

/**
 * IMPLICIT CONVERSIONS
 * 
 * Scala 3 introduces a new type class called `Conversion` to perform "implicit 
 * conversions"--the act of automatically converting one type to another.
 */
object conversions:
  import scala.language.implicitConversions

  final case class Rational(n: Int, d: Int)

  /**
   * EXERCISE 1
   * 
   * Create an instance of the type class `Conversion` for the combination of types
   * `Rational` (from) and `Double` (to).
   */
  // given ...
  given Conversion[Rational, Double] with
    def apply(rational: Rational): Double = rational.n / rational.d.toDouble

  /**
   * EXERCISE 2
   * 
   * Multiply a rational number by 2.0 (a double) to verify your automatic
   * conversion works as intended.
   */
  Rational(1, 2) * 2

object typeclass_graduation:
  /**
   * EXERCISE 1
   * 
   * Add cases to this enum for every primitive type in Scala.
   */
  enum PrimType[A]:
    case Byte extends PrimType[Byte]
    case Short extends PrimType[Short]
    case Int extends PrimType[Int]
    case Long extends PrimType[Long]
    case Float extends PrimType[Float]
    case Double extends PrimType[Double]
    case Char extends PrimType[Char]
    case Boolean extends PrimType[Boolean]
    case Unit extends PrimType[Unit]
    case String extends PrimType[String]
  
  /**
   * EXERCISE 2
   * 
   * Add another case to `Data` to model enumerations, like `Either`.
   */
  enum Data:
    case Record(fields: Map[String, Data])
    case Primitive[A](primitive: A, primType: PrimType[A])
    case Collection(elements: Vector[Data])
    case Disjunction(either: Either[Data, Data])

  /**
   * EXERCISE 3
   * 
   * Develop a type class called `EncodeData[A]`, that can encode an `A` into `Data`.
   */
  trait EncodeData[A]:
    extension (a: A) def encode: Data

  /**
   * EXERCISE 4
   * 
   * In the companion object of `Data`, write encoders for different primitive types in Scala,
   * including lists and collections.
   */
  object EncodeData:
    given EncodeData[String] with
      extension (s: String) def encode: Data = 
        Data.Primitive(s, PrimType.String)

    given EncodeData[Int] with
      extension (i: Int) def encode: Data =
        Data.Primitive(i, PrimType.Int)

    given EncodeData[Map[String, Data]] with
      extension (map: Map[String, Data]) def encode: Data =
        Data.Record(map)
  end EncodeData

  /**
   * EXERCISE 5
   * 
   * Create an instance of `EncodeData` for `Person`.
   */
  final case class Person(name: String, age: Int)
  object Person:
    import EncodeData.given

    given Conversion[Person, Map[String, Data]] with
      def apply(p: Person): Map[String, Data] =
        Map("name" -> p.name.encode, "age" -> p.age.encode)

    given EncodeData[Person] = summon[EncodeData[Person]]

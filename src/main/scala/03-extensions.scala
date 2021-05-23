/** EXTENSION METHODS
  *
  * Scala 3 brings first-class support for "extension methods", which allow
  * adding methods to classes after their definition. Previously, this feature
  * was emulated using implicits.
  */
object ext_methods:
  final case class Email(value: String)

  /** EXERCISE 1
    *
    * Add an extension method to `Email` to retrieve the username of the email
    * address (the part of the string before the `@` symbol).
    */
  extension (e: Email) def username: String = e.value.takeWhile(_ != '@')

  val sherlock = Email("sherlock@holmes.com").username

  /** EXERCISE 2
    *
    * Add an extension method to `Email` to retrieve the server of the email
    * address (the part of the string after the `@` symbol).
    */
  extension (e: Email)
    def server: String = e.value.substring(e.value.indexOf("@")) match {
      case s: String => s
      case _         => ""
    }

  /** EXERCISE 3
    *
    * Add an extension method to `Option[A]` that can zip one option with
    * another `Option[B]`, to return an `Option[(A, B)]`.
    */
  extension [A](self: Option[A])
    def zip[B](that: Option[B]): Option[(A, B)] =
      for a <- self ; b <- that yield (a, b)

  /** A rational number is one in the form n/m, where n and m are integers.
    */
  final case class Rational(numerator: BigInt, denominator: BigInt)

  /** EXERCISE 4
    *
    * Add a collection of extension methods to `Rational`, including `+`, to add
    * two rational numbers, `*`, to multiply two rational numbers, and `-`, to
    * subtract one rational number from another rational number.
    */
  extension (self: Rational) def +(that: Rational): Rational =
    Rational(
      (self.numerator * that.denominator) + (self.denominator * that.numerator),
      self.denominator * that.denominator
    )

  /** EXERCISE 5
    *
    * Convert this implicit syntax class to use extension methods.
    */
  extension (self: String) def equalsIgnoreCase(that: String) =
    self.toLowerCase == that.toLowerCase

  /** EXERCISE 6
    *
    * Import the extension method `isSherlock` into the following object so the
    * code will compile.
    */
  object test:
    import string_extensions.isSherlock

    val test: Boolean = "John Watson".isSherlock

  object string_extensions:
    extension (s: String) def isSherlock: Boolean = s.startsWith("Sherlock")

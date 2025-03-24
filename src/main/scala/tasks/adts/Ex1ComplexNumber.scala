package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class ComplexImpl(re:Double, im:Double)

    // Change assignment below: should probably define a case class and use it?
    type Complex = ComplexImpl
    def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = ComplexImpl(complex.re + other.re, complex.im + other.im)
      def subtract(other: Complex): Complex = ComplexImpl(complex.re - other.re, complex.im - other.im)
      def asString(): String = (complex.re, complex.im) match
        case (0,0) => "0.0"
        case (_,0) => complex.re.toString
        case (0,_) => complex.im.toString + "i"
        case _ => complex.re.toString + (complex.im match
          case im if im < 0 =>  " - " + (-complex.im).toString + "i"
          case im if im > 0 => " + " + complex.im.toString + "i"
          )
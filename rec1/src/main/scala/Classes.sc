
class Rational(x : Int, y: Int) {
  def bas6 = x
  def maqam = y

  def add(that: Rational) =
    new Rational(
      bas6 * that.maqam + maqam * that.bas6,
      maqam * that.maqam
    )

  def neg =
    new Rational(
      -1 * bas6,
      maqam
    )

  def sub(that: Rational) =
    add(that.neg)

  def mul(that: Rational) =
    new Rational(
      bas6 * that.bas6,
      maqam * that.maqam
    )


  override def toString =
    bas6 + "/" + maqam
}

val x = new Rational(1, 2)
x.bas6
x.maqam

x.add(new Rational(1,2))

x.neg

val twoThirds = new Rational(2,3)
val oneThird = new Rational(1,3)
twoThirds.sub(oneThird)

val y = new Rational(5,7)
val z = new Rational(3,2)

oneThird.sub(y).sub(z)

def abs(x : Double) : Double = if (x < 0) -x else x

abs(1)
abs(-112)

def sqroot(x : Double) = {
  def isGoodEnough(guess: Double): Boolean =
    abs(guess * guess - x) / x < 0.00001

  def improve(guess: Double): Double =
    (guess + x / guess) / 2

  def squareItr(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else squareItr(improve(guess))

  squareItr(1.0)
}

sqroot(4)
sqroot(1e60)

def factorial(x : Int) : Int = {
  def loop(acc : Int, x : Int) : Int =
    if (x == 0) acc else loop(acc * x, x - 1)

  loop(1, x)
}


factorial(7)
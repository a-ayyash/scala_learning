def sumInts1(lower : Int, upper: Int) : Int =
  if(lower > upper) 0 else lower + sumInts1(lower+1, upper)

sumInts1(0, 5)

def cube(i: Int) : Int = i * i * i

def sumCubes1(lower: Int, upper: Int) : Int =
  if (lower > upper) 0 else cube(lower) + sumCubes1(lower+1, upper)

sumCubes1(0, 2)


//first trial at abstraction
def sum(f: Int => Int, lower : Int, upper: Int) : Int =
  if (lower > upper) 0
  else
    f(lower) + sum(f, lower+1, upper)

sum(a => a, 0, 5)
sum(a => a * a * a, 0, 2)

def fact(x: Int) : Int =
    if (x == 0) 1
    else x * fact(x-1)

fact(3)

sum(fact, 0, 4)
//end of first trial

//first abstraction + tail recursion
def sumTail(f: Int=>Int)(lower: Int, upper: Int) = {

    def loop(lower: Int, acc: Int) : Int = {
        if (lower > upper) acc
        else loop(lower+1, acc + f(lower))
    }

    loop(lower, 0)
}

sumTail(a => a)(0, 5)

//currying
def sumF(f: Int => Int)(lower: Int, upper: Int) : Int =
    if (lower > upper) 0 else f(lower) + sumF(f)(lower+1, upper)

def product(f: Int => Int)(lower: Int, upper: Int) : Int =
    if (lower > upper) 1 else f(lower) * product(f)(lower + 1, upper)

def factorial(n: Int) = product(x=>x)(1, n)

product(x => x*x)(3,4)
factorial(4)

//general form of sum and product
def mapReduce(f: Int=> Int, comb: (Int, Int) => Int, unit: Int)(lower: Int, upper: Int) : Int =
    if (lower > upper) unit
    else comb(f(lower), mapReduce(f, comb, unit)(lower + 1, upper))

def productMapReduce(f: Int => Int)(lower: Int, upper: Int) : Int =
    mapReduce(f, (x,y) => x*y, 1)(lower, upper)

productMapReduce(x=>x*x)(3,4)

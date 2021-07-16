import scala.annotation.tailrec
// package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    rng.nextInt match {
      case (n, rng) if n < 0 => (-n, rng)
      case (n, rng) => (n, rng)
    }
  

  def double(rng: RNG): (Double, RNG) =
    nonNegativeInt(rng) match {
      case (i, rng2) => (i.toDouble / Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) =  {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    intDouble(rng) match {
      case ((i, d), rng2) => ((d, i), rng2)
    }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, rng: RNG, l: List[Int]): (List[Int], RNG) =
      count match {
        case 0 => (l, rng)
        case _ => {
          val (i, rng2) = rng.nextInt
          loop(count - 1, rng2, i :: l)
        }
      }

    loop(count, rng, Nil)
  }

  def _double: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def loop(rng: RNG, lra: List[Rand[A]], la: List[A]): (List[A], RNG) =
      lra match {
        case Nil => (la, rng)
        case ra :: lraTail => {
          val (a, rng2) = ra(rng)
          loop(rng2, lraTail, a :: la)
        }
      }

    rng => loop(rng, fs, Nil) match {
      case (la, rng2) => (la.reverse, rng2)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]) = {
    (rng: RNG) => f(rng) match {
      case (a, rng2) => g(a)(rng2)
    }
  }
  type _Rand[+A] = RNG => (A, RNG)

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => (rng => (f(a), rng)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){a =>
      flatMap(rb){b =>
        (rng => (f(a, b), rng))
      }
    }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State(s => (f(a), s)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.flatMap(b => State(s => (f(a, b), s))))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => run(s) match {
      case (a, s2) => f(a).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    State(s => ls.foldLeft((List.empty[A], s)) {
      case ((la, s), ra) => ra.run(s) match {
        case (a, s2) => (a :: la, s2)
      }
    } match {
      case (as, s) => (as.reverse, s)
    })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object Example {
  val incr: State[Int, Double] = State(s => ((s + 1).toDouble, s + 1))
  val decr: State[Int, Double] = State(s => ((s - 1).toDouble, s - 1))
  val times5: State[Int, Double] = State(s => ((s * 5).toDouble, s * 5))
  val pow2: State[Int, Double] = State(s => ((s * s).toDouble, s * s))

  def run(x: Int): ((Double, Double, Double, Double), Int) = {
    val state = for {
      a <- incr
      b <- pow2
      c <- times5
      d <- decr
    } yield (a, b, c, d)
    
    state.run(x)
  }

  def sequence(x: Int) =
    State.sequence(List(incr, decr, times5, pow2)).run(x)
}
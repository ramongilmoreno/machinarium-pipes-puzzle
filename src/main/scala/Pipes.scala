object Pipes extends App {
  val valves = 2 to 12 toList

  case class Connection (a: Int, b: Int) {
    def contains (x: Int) = x == a || x == b
  }

  class Pipes (c: List[Connection] = Nil) {
    var connections: List[Connection] = c
    def remove (x: Int) = {
      val r = new Pipes(connections.filter(c => !(c.a == x || c.b == x)))
      // println(s"From $connections created ${r.connections}")
      r
    }

    implicit class NodeWrapper (node: Int) {
      def -> (b: Int): Unit = {
        connections = Connection(node, b) :: connections
        // println(s"${Pipes.this} $connections")
      }
    }
  }

  // Pipes definitions
  val pipes = new Pipes {
    1 -> 2
    1 -> 19
    19 -> 3
    19 -> 22
    22 -> 23

    22 -> 6
    4 -> 24
    4 -> 25
    25 -> 26
    25 -> 10

    26 -> 27
    26 -> 28



    26 -> 9
    9 -> 36
    36 -> 45
    36 -> 37
    37 -> 38

    37 -> 41
    38 -> 40
    38 -> 39
    39 -> 10
    2 -> 13

    13 -> 14
    14 -> 46
    14 -> 12
    12 -> 15



    15 -> 27
    15 -> 16
    23 -> 20
    20 -> 21
    20 -> 11

    11 -> 18
    18 -> 24
    18 -> 17
    17 -> 3
    17 -> 29

    29 -> 30
    30 -> 31
    30 -> 45
    31 -> 34


    34 -> 35
    35 -> 5
    5 -> 28
    6 -> 32
    32 -> 7

    7 -> 43
    43 -> 44
    43 -> 46
    8 -> 42
    8 -> 34
  }
  case class Case (pipes: Pipes, water: List[Int]) {
    def flow: Option[Case] = {
      pipes.connections.partition(c => water.contains(c.a) || water.contains(c.b)) match {
        case (Nil, _) => None
        case (flows, newPipes) => Some(Case(new Pipes(newPipes), (water ::: flows.flatMap(c => List(c.a, c.b))).distinct.sorted))
      }
    }
  }

  def rflow (cas: Case): List[Int] = {
    cas.flow match {
      case None => cas.water
      case Some(newCase) => rflow(newCase)
    }
  }

  def test (pipes: Pipes, v: List[Int], closed: List[Int], maxClosed: Int, checkSolutionWater: List[Int] => Boolean): Unit = {
      v match {
        case Nil => {
          val water = rflow(Case(pipes, List(1)))
          if (closed.size <= maxClosed) {
            // println(s"Closed [$closed] gives [$water]")
            if (checkSolutionWater(water)) {
              println(s"SOLUTION $closed")
            }
          }
        }
        case a :: rest =>
          // Test open and closed
          test(pipes, rest, closed, maxClosed, checkSolutionWater)
          test(pipes.remove(a), rest, (a :: closed).sorted, maxClosed, checkSolutionWater)
      }
  }

  if (false) {
    val sample = new Pipes {
      1 -> 2
      2 -> 3
      1 -> 4
    }
    test(sample, List(2), Nil, 1, !_.contains(3))
  }

  test(pipes, valves, Nil, 3, !_.contains(40))

}


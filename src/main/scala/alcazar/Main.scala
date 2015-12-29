package alcazar

import alcazar.StateRenderer
import goal.{NamedRule, CompoundRule}

/**
  * Created by matt on 29/12/2015.
  */
object Main extends App {

  val nodes = List(
    EdgeNode(3, 0),
    EmptyNode(1, 1),
    EmptyNode(2, 1),
    EmptyNode(3, 1),
    EmptyNode(1, 2),
    EmptyNode(2, 2),
    EmptyNode(3, 2),
    EmptyNode(1, 3),
    EmptyNode(2, 3),
    EmptyNode(3, 3),
    EdgeNode(1, 4)
  )
  val edges = List(
    0 -> 3,
    1 -> 2,
    1 -> 4,
    2 -> 3,
    3 -> 6,
    4 -> 5,
    4 -> 7,
    5 -> 6,
    5 -> 8,
    6 -> 9,
    7 -> 8,
    7 -> 10,
    8 -> 9
  )
  val state = AlcazarState(nodes, edges)

  println(StateRenderer.test)
  /*val alcazarRules = CompoundRule(
    NamedRule("has-corners", {
      case s: AlcazarState if
    })
  )*/

}

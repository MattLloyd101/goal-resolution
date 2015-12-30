package alcazar

import goal.State

import scala.annotation.tailrec

/**
  * Created by matt on 29/12/2015.
  */
abstract class AlcazarState extends State {

  type Coord = (Int, Int)
  type Grid = Map[Coord, AlcazarNode]

  val nodes: List[AlcazarNode]
  val edges: List[AlcazarEdge]
  lazy val edgeMap: Map[AlcazarNode, List[AlcazarNode]] = edges.foldLeft(Map[AlcazarNode, List[AlcazarNode]]()) {
    // TODO: Handle user defined walls
    case (out, AlcazarEdge(a, b)) => out + (a -> (b :: out.getOrElse(a, Nil))) + (b -> (a :: out.getOrElse(b, Nil)))
  }
  lazy val coordMap: Map[Coord, List[Coord]] = edgeMap.map { case (node, seq) => node.coords -> seq.map(_.coords) }
  val path: List[AlcazarNode] = Nil


  lazy val grid: Map[Coord, AlcazarNode] = {
    @tailrec
    def generateGrid(nodeList: List[AlcazarNode], out: Grid): Grid  = nodeList match {
      case node :: rest => generateGrid(rest, out + (node.coords -> node))
      case Nil => out
    }

    generateGrid(nodes, Map())
  }

  lazy val bounds: (Int, Int) = nodes.filter(!_.isInstanceOf[EdgeNode]).foldLeft((0, 0)) { case ((w, h), n) => (Math.max(w, n.x), Math.max(h, n.y)) }
  lazy val width = bounds._1
  lazy val height = bounds._2

  def inBounds(nodeLocation: Coord) = nodeLocation match {
    case (x, y) if x > 0 && x < width && y > 0 && y < height => true
    case _ => false
  }

  def isAdjacent(a: AlcazarNode, b: AlcazarNode): Boolean = edgeMap.get(a).exists { list => list.contains(b) }
  def isAdjacentCoords(a: Coord, b: Coord): Boolean = coordMap.get(a).exists { list => list.contains(b) }
}

object AlcazarState {

  def apply(_nodes: List[AlcazarNode], edgeIndexes: List[(Int, Int)]): AlcazarState = new AlcazarState {
    override val nodes: List[AlcazarNode] = _nodes
    override val edges: List[AlcazarEdge] = edgeIndexes.map {
      case (a, b) => AlcazarEdge(nodes(a), nodes(b))
    }
  }

}

/**
  * Doublely linked list A->B ∴ B->A
  */
case class AlcazarEdge(a: AlcazarNode, b: AlcazarNode)

sealed trait AlcazarNode {
  val x: Int
  val y: Int

  lazy val coords: (Int, Int) = (x, y)
}

case class EmptyNode(x: Int, y: Int) extends AlcazarNode
case class EdgeNode(x: Int, y: Int) extends AlcazarNode
case class OptionalNode(x: Int, y: Int) extends AlcazarNode
case class TunnelNode(x: Int, y: Int) extends AlcazarNode

sealed trait TileType
case object OOB extends TileType
case object Wall extends TileType
case object Exit extends TileType
case object GridH extends TileType
case object GridV extends TileType
case object Empty extends TileType
case object Tunnel extends TileType
case object Optional extends TileType
case object PlayerPath extends TileType
case object PlayerWall extends TileType
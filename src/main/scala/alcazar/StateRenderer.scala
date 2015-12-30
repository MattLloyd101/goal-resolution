package alcazar

import scala.collection.immutable.{SortedMap, ListMap}

/**
  * Created by matt on 29/12/2015.
  */
object StateRenderer {

  import LineStyles.{Wall => W, Path => P}

  type Coord = (Int, Int)

  def render(state: AlcazarState): String = {

    val w = state.width*2
    val h = state.height*2

    def nodeToTileType(node: AlcazarNode): TileType = node match {
      case n: EmptyNode => Empty
      case n: EdgeNode => Exit
      case n: OptionalNode => Optional
      case n: TunnelNode => Tunnel
    }

    def nodeLocationToTileType(fromNode: AlcazarNode, nodeLocation: Coord): TileType = state.grid.get(nodeLocation) match {
      case None if state.inBounds(nodeLocation) => Wall
      case None => OOB
      case Some(node) if state.isAdjacent(fromNode, node) => nodeToTileType(node)
      case Some(node) => Wall
    }

    def renderLocation(grid: Map[Coord, TileType])(location: (Int, Int)): String = {
      val (x, y) = location

      val upl = (x, y - 1)
      val rightl = (x + 1, y)
      val downl = (x, y + 1)
      val leftl = (x - 1, y)

      val self = grid.getOrElse(location, OOB)
      val up = grid.getOrElse(upl, OOB)
      val right = grid.getOrElse(rightl, OOB)
      val down = grid.getOrElse(downl, OOB)
      val left = grid.getOrElse(leftl, OOB)

      (self, up, right, down, left) match {
        case (Wall, Wall, Wall, Wall, Wall) => W.TJ
        case (Wall, Wall, Wall, Wall, _) => W.LM
        case (Wall, Wall, Wall, _, Wall) => W.BM
        case (Wall, Wall, _, Wall, Wall) => W.RM
        case (Wall, _, Wall, Wall, Wall) => W.TM

        case (Wall, _, Wall, Wall, _) => W.TL
        case (Wall, _, _, Wall, Wall) => W.TR
        case (Wall, _, Wall, _, Wall) => W.MH
        case (Wall, Wall, _, Wall, _) => W.MV
        case (Wall, Wall, Wall, _, _) => W.BL
        case (Wall, Wall, _, _, Wall) => W.BR
        case (Wall, _, Exit, _, Wall) => W.MH
        case (Wall, _, Wall, _, Exit) => W.MH
        case (Wall, _, _, Wall, Exit) => W.TR
        case (Wall, Wall, Exit, _, _) => W.BL

        case (Wall, GridH | GridV | GridX, Wall, GridH | GridV | GridX, GridH | GridV | GridX) => W.MH
        case (Wall, GridH | GridV | GridX, GridH | GridV | GridX, GridH | GridV | GridX, Wall) => W.MH
        case (Wall, Wall, GridH | GridV | GridX, GridH | GridV | GridX, GridH | GridV | GridX) => W.MV
        case (Wall, GridH | GridV | GridX, GridH | GridV | GridX, Wall, GridH | GridV | GridX) => W.MV

        case (GridX, _, _, _, _) => W.DV
        case (GridH, _, _, _, _) => W.DH
        case (GridV, _, _, _, _) => W.DV
        case (Empty, _, _, _, _) => W.SP
        case (Exit, _, _, _, _) => W.SP

        case wat =>
          println(wat)
          "?"
      }
    }

    @inline
    def toScaled(x: Int, y: Int): Coord = (1+(x-1)*2, 1+(y-1)*2)

    @inline
    def fromScaled(x: Int, y: Int): Coord = (((x-1)/2)+1, ((y-1)/2)+1)

    val coords = for (x <- 0 until (w+1); y <- 0 until (h+1)) yield (x, y)

    val scaledGrid = state.grid map {
      // Edge nodes are slightly different as they aren't scaled the same way.
      case (coord@(x, y), v) if v.isInstanceOf[EdgeNode] =>
        val scaled = toScaled(x,y)
        coord match {
          case _ if x == 0 => (0, scaled._2) -> v
          case _ if x > state.width => (w, scaled._2) -> v
          case _ if y == 0 => (scaled._1, 0) -> v
          case _ if y > state.height => (scaled._1, h) -> v
        }
      case ((x, y), v) => toScaled(x, y) -> v
    }

    val grid = coords.map { case coord =>
      coord -> scaledGrid.get(coord).map(nodeToTileType).getOrElse {
        coord match {
          case (x, y) if x == 0 || x == w || y == 0 || y == h  => Wall

          // The case where we are on a grid cross.
          case (x, y) if y != 0 && y != h && y % 2 == 0 &&
                         x != 0 && x != w && x % 2 == 0 => GridX

          case (x, y) if y != 0 && y != h && y % 2 == 0 =>
            val aboveNode = state.grid.get(fromScaled(x, y-1))
            lazy val belowNode = state.grid.get(fromScaled(x, y+1))
            aboveNode
              .flatMap { n1 => belowNode.map { n2 => (n1, n2) } }
              .filterNot { case (n1, n2) => state.isAdjacent(n1, n2) }
              .map { _ => Wall }
              .getOrElse(GridH)

          // walls and grid lines only appears between spaces i.e. %2==0
          case (x, y) if x != 0 && x != w && x % 2 == 0 =>
            val leftNode = state.grid.get(fromScaled(x-1, y))
            // lazy as if left doesn't exist right isn't required.
            lazy val rightNode = state.grid.get(fromScaled(x+1, y))
            // lookup the left and right nodes.
            leftNode
              .flatMap { n1 => rightNode.map { n2 => (n1, n2) } }
              // filter to only those things that aren't adjacent
              .filterNot { case (n1, n2) => state.isAdjacent(n1, n2) }
              // these must be walls as they aren't adjacent
              .map { _ => Wall }
              // everything else is a free spot so draw the grid lines.
              .getOrElse(GridV)

          case _ => Empty
        }
      }
    }.toMap

    val grid2 = grid.map {
      case (k@(x, y), GridX) =>
        val list = List( (x, y-1), (x, y+1), (x-1, y), (x+1, y) )
        // if any of the adjacent locations are walls
        if (list.flatMap(grid.get).contains(Wall)) {
          // then this is a wall too.
          k -> Wall
        } else {
          k -> GridX
        }

      case (k, v) => k -> v
    }

    val fn = renderLocation(grid2)_

    val map: Map[Int, String] = (SortedMap.empty(Ordering[Int]) ++ coords.view.groupBy(_._2)).mapValues { row =>
      row.map(fn).mkString("")
    }

    map.values.mkString("\n")
  }

  val test =
      W.TL + W.MH + W.MH + P.MV + W.TR + "\n" +
      P.MV + P.TL + P.MH + P.BR + W.MV  + "\n" +
      W.MV + P.MV + W.MH + P.MH + W.MV  + "\n" +
      W.MV + P.BL + P.MH + P.TR + W.MV  + "\n" +
      W.MV + P.TL + P.MH + P.BR + W.MV  + "\n" +
      W.BL + P.MV + W.MH + W.MH + W.BR + "\n"

  val test2 =
    W.TL + W.MH + W.MH + W.MH + W.MH + P.MV + W.TR + "\n" +
    W.MV + P.TL + W.DV + P.MH + W.DV + P.BR + W.MV + "\n" +
    W.MV + W.DH + W.MH + W.MH + W.MH + W.DH + W.MV + "\n" +
    W.MV + P.BL + W.DV + P.MH + W.DV + P.TR + W.MV + "\n" +
    W.MV + P.MH + P.MH + P.MH + W.DH + W.DH + W.MV + "\n" +
    W.MV + P.TL + W.DV + P.MH + W.DV + P.BR + W.MV + "\n" +
    W.BL + P.MV + W.MH + W.MH + W.MH + W.MH + W.BR + "\n"

}

object LineStyles {
  object Wall {
    val DH = "┄"
    val DV = "┆"

    val TL = "╔"
    val TR = "╗"
    val TM = "╦"

    val BL = "╚"
    val BR = "╝"
    val BM = "╩"

    val MH = "═"

    val LM = "╠"

    val RM = "╣"

    val MV = "║"

    val TJ = "╬"
    val XX = "X"
    val SP = " "
  }
  object Path {
    val TH = "─"
    val TL = "┌"
    val TR = "┐"
    val TM = "┬"

    val BH = "─"
    val BL = "└"
    val BR = "┘"
    val BM = "┴"

    val MH = "─"

    val LV = "│"
    val LM = "├"

    val RV = "│"
    val RM = "┤"

    val MV = "│"

    val TJ = "┼"
    val XX = "X"
    val SP = " "
  }
}

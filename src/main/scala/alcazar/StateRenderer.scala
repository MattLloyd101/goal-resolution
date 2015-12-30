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

      val self = grid.getOrElse(location, OOB)
      val up = grid.getOrElse((x, y - 1), OOB)
      val right = grid.getOrElse((x + 1, y), OOB)
      val down = grid.getOrElse((x, y + 1), OOB)
      val left = grid.getOrElse((x - 1, y), OOB)


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

        case (Empty, _, _, _, _) if y != 0 && y != h && y % 2 == 0 => W.DH
        case (Empty, _, _, _, _) if x != 0 && x != w && x % 2 == 0 => W.DV
        case (Empty, _, _, _, _) => W.SP
        case (Exit, _, _, _, _) => W.SP

        case wat =>
          println(wat)
          "?"
      }
    }

    val coords = for (x <- 0 until (w+1); y <- 0 until (h+1)) yield (x, y)

    val scaledGrid = state.grid map {
      case (coord, v) if v.isInstanceOf[EdgeNode] => coord match {
        case (x, y) if x == 0 => (0, 1+(y-1)*2) -> v
        case (x, y) if x > state.width => (w, 1+(y-1)*2) -> v
        case (x, y) if y == 0 => (1+(x-1)*2, y) -> v
        case (x, y) if y > state.height => (1+(x-1)*2, h) -> v
      }
      case ((x, y), v) => (1+(x-1)*2, 1+(y-1)*2) -> v
    }
    // generate a pure walled grid.
    val grid = coords.map { case coord =>
      coord -> scaledGrid.get(coord).map(nodeToTileType).getOrElse {
        coord match {
          case (x, _) if x == 0 || x == w => Wall
          case (_, y) if y == 0 || y == h => Wall
          case _ => Empty
        }
      }
    }.toMap



    val fn = renderLocation(grid)_

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

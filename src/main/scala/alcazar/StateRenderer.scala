package alcazar

/**
  * Created by matt on 29/12/2015.
  */
object StateRenderer {

  case class RenderNode(self: TileType, up: TileType, down: TileType, left: TileType, right: TileType)

  def render(state: AlcazarState): String = {

    def nodeToTileType(node: AlcazarNode): TileType = node match {
      case n: EmptyNode => Empty
      case n: EdgeNode => Wall
      case n: OptionalNode => Optional
      case n: TunnelNode => Tunnel
    }

    def nodeLocationToTileType(nodeLocation: (Int, Int)): TileType = state.grid.get(nodeLocation) match {
      case None if state.inBounds(nodeLocation) => Wall
      case None => OOB
      case Some(node) => nodeToTileType(node)
    }


    def mapNode(node: AlcazarNode): RenderNode = {
      RenderNode(
        nodeToTileType(node),
        nodeLocationToTileType((node.x, node.y + 1)),
        nodeLocationToTileType((node.x, node.y - 1)),
        nodeLocationToTileType((node.x - 1, node.y)),
        nodeLocationToTileType((node.x + 1, node.y)))
    }

    val newGrid = state.grid.mapValues(mapNode)

    ""
  }

  import LineStyles.{Wall => W, Path => P}

  val test =
      W.TL + W.TH + W.TH + P.MV + W.TR + "\n" +
      P.MV + P.TL + P.MH + P.BR + W.RV  + "\n" +
      W.LV + P.MV + W.MH + P.MH + W.RV  + "\n" +
      W.LV + P.BL + P.MH + P.TR + W.RV  + "\n" +
      W.LV + P.TL + P.MH + P.BR + W.RV  + "\n" +
      W.BL + P.MV + W.BH + W.BH + W.BR + "\n"

}

object LineStyles {
  object Wall {
    val TH = "═"
    val TL = "╔"
    val TR = "╗"
    val TM = "╦"

    val BH = "═"
    val BL = "╚"
    val BR = "╝"
    val BM = "╩"

    val MH = "═"

    val LV = "║"
    val LM = "╠"

    val RV = "║"
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

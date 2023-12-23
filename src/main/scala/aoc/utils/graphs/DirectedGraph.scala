package aoc.utils.graphs

import scala.collection.mutable

class DirectedGraph(nodesArg: List[DirectedGraphNode]) {

  var nodes: mutable.ListBuffer[DirectedGraphNode] = mutable.ListBuffer.from(nodesArg)

  def getTotalWeight: Int = {
    nodes.map(node => node.nodesTo.map { case (_, weight) => weight }.sum).sum
  }

  def getLongestPath(from: DirectedGraphNode, to: DirectedGraphNode): Int = {

    from.nodesTo.map { case (nodeTo, weight) => if (nodeTo == to) weight else weight + getLongestPath(nodeTo, to) }
      .max
  }

  def replaceNodesWithOneInOneOut(): Unit = {

    var nodeRemoved = true
    while (nodeRemoved) {
      val replacedNodes = nodes.flatMap { node =>
        if ((node.nodesFrom.length == 1) && (node.nodesTo.length == 1)) {
          (node.nodesFrom.head, node.nodesTo.head) match {
            case ((nodeFrom, weightFrom), (nodeTo, weightTo)) =>
              nodeFrom.connectTo(nodeTo, weightFrom + weightTo)
              nodeFrom.removeConnectionTo(node)
              node.removeConnectionFrom(nodeFrom)
              node.removeConnectionTo(nodeTo)
              nodeTo.removeConnectionFrom(node)
              None
          }
        }
        else {
          Some(node)
        }
      }
      nodeRemoved = nodes.length != replacedNodes.length
      nodes = replacedNodes
    }
  }
}

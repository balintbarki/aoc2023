package aoc.utils.graphs

import scala.annotation.tailrec
import scala.collection.mutable

case class DirectedGraphNode(id: String = "") {

  // Nodes to which there is a transition from this node, with a weight
  val nodesTo: mutable.ListBuffer[(DirectedGraphNode, Int)] = mutable.ListBuffer()

  // Nodes from which there is a transition to this node, with a weight
  val nodesFrom: mutable.ListBuffer[(DirectedGraphNode, Int)] = mutable.ListBuffer()

  def isConnectedTo(other: DirectedGraphNode): Boolean = nodesListContains(nodesTo, other)

  def isConnectedFrom(other: DirectedGraphNode): Boolean = nodesListContains(nodesFrom, other)

  def connectTo(other: DirectedGraphNode, weight: Int = 1): Unit = {
    if (!isConnectedTo(other)) {
      nodesTo.addOne((other, weight))
    } else
      throw new IllegalArgumentException(s"Connection already exists between nodes $this and $other")


    if (!other.isConnectedFrom(this))
      other.nodesFrom.addOne((this, weight))
    else
      throw new IllegalArgumentException(s"Connection already exists between nodes $this and $other")
  }

  def removeConnectionTo(other: DirectedGraphNode): Unit = {
    nodesTo.zipWithIndex.filter { case ((node, _), _) => node == other }
      .foreach { case (_, index) => nodesTo.remove(index) }
  }

  def removeConnectionFrom(other: DirectedGraphNode): Unit = {
    nodesFrom.zipWithIndex.filter { case ((node, _), _) => node == other }
      .foreach { case (_, index) => nodesFrom.remove(index) }
  }

  def getAllNodesTo: Set[DirectedGraphNode] = {

    @tailrec
    def collectAllNodesTo(
      visitedNodes: Set[DirectedGraphNode],
      newNodes: Set[DirectedGraphNode]): Set[DirectedGraphNode] = {
      val newNodesTo = newNodes.flatMap { node => node.nodesTo.map { case (node, _) => node }.toSet }
      val newVisitedNodes = visitedNodes union newNodes
      val newNewNodes = newNodesTo diff newVisitedNodes
      if (newNewNodes.nonEmpty)
        collectAllNodesTo(newVisitedNodes, newNewNodes)
      else
        newVisitedNodes
    }

    collectAllNodesTo(Set.empty, nodesTo.map { case (node, _) => node }.toSet)
  }

  private def nodesListContains(
    nodes: mutable.ListBuffer[(DirectedGraphNode, Int)], node: DirectedGraphNode): Boolean = {
    nodes.map { case (node, _) => node }.contains(node)
  }
}

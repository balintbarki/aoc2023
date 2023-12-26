package aoc.utils.graphs

import scala.collection.mutable

case class UndirectedGraphNode(id: String = "") {

  val connectedNodes: mutable.ListBuffer[(UndirectedGraphNode, Int)] = mutable.ListBuffer()

  def connectedNodesWithoutWeight: Set[UndirectedGraphNode] = connectedNodes.map { case (node, _) => node }.toSet

  def isConnectedTo(other: UndirectedGraphNode): Boolean = connectedNodes.map { case (node, _) => node }.contains(other)

  def connectTo(other: UndirectedGraphNode, weight: Int = 1): Unit = {
    if (!isConnectedTo(other)) {
      connectedNodes.addOne((other, weight))
      other.connectedNodes.addOne((this, weight))
    }
  }

  def removeConnectionTo(other: UndirectedGraphNode): Unit = {
    connectedNodes.zipWithIndex.filter { case ((node, _), _) => node == other }
      .foreach { case (_, index) => connectedNodes.remove(index) }
  }
}

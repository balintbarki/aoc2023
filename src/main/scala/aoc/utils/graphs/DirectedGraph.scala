package aoc.utils.graphs

import scala.collection.mutable

class DirectedGraph(nodesArg: List[DirectedGraphNode]) {

  val cacheIsPathBetween: mutable.Map[(DirectedGraphNode, DirectedGraphNode), Boolean] = mutable.Map.empty
  val cacheShortestPath: mutable.Map[(DirectedGraphNode, DirectedGraphNode), Seq[DirectedGraphNode]] = mutable.Map.empty

  var nodes: mutable.ListBuffer[DirectedGraphNode] = mutable.ListBuffer.from(nodesArg)

  def getTotalWeight: Int = {
    nodes.map(node => node.nodesTo.map { case (_, weight) => weight }.sum).sum
  }

  def isPathBetween(first: DirectedGraphNode, second: DirectedGraphNode): Boolean = {
    cacheIsPathBetween.getOrElseUpdate((first, second), first.getAllNodesTo.contains(second))
  }

  def getLongestPath(from: DirectedGraphNode, to: DirectedGraphNode): Int = {

    val visitedNodes: mutable.Set[DirectedGraphNode] = mutable.Set()

    def doGetLongestPath(from: DirectedGraphNode, to: DirectedGraphNode): Option[Int] = {
      val newPathLengths = from.nodesTo.flatMap { case (nodeTo, weight) =>
        if (nodeTo == to)
          Some(weight)
        else if (!visitedNodes.contains(nodeTo)) {
          visitedNodes.add(from)
          val result = doGetLongestPath(nodeTo, to).map(_ + weight)
          visitedNodes.remove(from)
          result
        } else
          None
      }

      if (newPathLengths.nonEmpty)
        Some(newPathLengths.max)
      else
        None
    }

    visitedNodes.add(from)
    doGetLongestPath(from, to)
      .getOrElse(throw new IllegalArgumentException(s"Top level path finding returned None"))
  }

  def getShortestPath(from: DirectedGraphNode, to: DirectedGraphNode): Seq[DirectedGraphNode] = {

    def calculateShortestPath(from: DirectedGraphNode, to: DirectedGraphNode): Seq[DirectedGraphNode] = {
      if (from == to) {
        Seq(from)
      } else {

        var currentShortestPath: Option[Seq[DirectedGraphNode]] = None

        def updateShortestPath(path: Seq[DirectedGraphNode]): Unit = {
          currentShortestPath match {
            case Some(shortestPath) if path.size < shortestPath.size => currentShortestPath = Some(path)
            case None                                                => currentShortestPath = Some(path)
            case _                                                   =>
          }
        }

        def findPath(walkedPath: Seq[DirectedGraphNode], to: DirectedGraphNode): Unit = {
          val lastInPath = walkedPath.last
          if (lastInPath.isConnectedTo(to)) {
            updateShortestPath(walkedPath ++ Seq(to))
          } else if (lastInPath.nodesTo.nonEmpty) {
            lastInPath.nodesTo.foreach { case (node, _) => findPath(walkedPath ++ Seq(node), to) }
          }
        }

        findPath(Seq(from), to)
        currentShortestPath.getOrElse(Seq.empty)
      }
    }

    cacheShortestPath.getOrElseUpdate((from, to), calculateShortestPath(from, to))
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

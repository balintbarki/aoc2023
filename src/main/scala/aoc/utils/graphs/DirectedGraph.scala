package aoc.utils.graphs

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

  def keepOnlyNodes(nodesToKeep: Seq[DirectedGraphNode]): DirectedGraph = {
    val nodesKept = nodesToKeep.map { nodeToKeep =>
      val nodesToToKeep = nodeToKeep.nodesTo.filter { case (node, _) => nodesToKeep.contains(node) }
      val nodesFromToKeep = nodeToKeep.nodesFrom.filter { case (node, _) => nodesToKeep.contains(node) }
      nodeToKeep.copy(nodesTo = nodesToToKeep, nodesFrom = nodesFromToKeep)
    }

    // Redirect connections to the new instances
    // First collect the new connections, clear the current ones, then connect to the new instances
    val nodesWithNodesToAndNodesFrom = nodesKept.map { nodeKept =>
      val newNodesTo = nodeKept.nodesTo
        .flatMap { case (nodeTo, weight) => nodesKept.find(_.id == nodeTo.id).map((_, weight)) }
      nodeKept.nodesTo.clear()
      nodeKept.nodesFrom.clear()

      (nodeKept, newNodesTo)
    }

    nodesWithNodesToAndNodesFrom.foreach { case (node, nodesTo) =>
      nodesTo.foreach { case (nodeTo, weight) =>
        node.connectTo(nodeTo, weight)
      }
    }

    new DirectedGraph(nodesKept.toList)
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

  def getNodesInOrder: Seq[DirectedGraphNode] = {

    @tailrec
    def createOrderedNodeList(
      startList: ListBuffer[DirectedGraphNode],
      endList: ListBuffer[DirectedGraphNode],
      remainingNodes: List[DirectedGraphNode]): List[DirectedGraphNode] = {

      if (remainingNodes.length <= 1) {
        startList.addAll(remainingNodes).addAll(endList).toList
      } else {
        val remainingGraph = new DirectedGraph(remainingNodes.toList)
        val initialNodes = remainingGraph.nodes.filter(_.nodesFrom.isEmpty)
        val lastNodes = remainingGraph.nodes.filter(_.nodesTo.isEmpty)

        startList.addAll(initialNodes)
        endList.insertAll(0, lastNodes)

        val keptNodes = remainingNodes.filter(!initialNodes.contains(_)).filter(!lastNodes.contains(_))
        val reducedGraph = keepOnlyNodes(keptNodes)

        createOrderedNodeList(startList, endList, reducedGraph.nodes.toList)
      }
    }

    createOrderedNodeList(ListBuffer[DirectedGraphNode](), ListBuffer[DirectedGraphNode](), nodes.toList)
  }
}

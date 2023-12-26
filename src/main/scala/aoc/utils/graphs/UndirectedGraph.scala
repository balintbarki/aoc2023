package aoc.utils.graphs

import scala.collection.mutable

case class UndirectedGraph(nodesArg: Set[UndirectedGraphNode]) {

  val nodes: mutable.Set[UndirectedGraphNode] = mutable.Set.from(nodesArg)

  def getAllEdges: Set[(UndirectedGraphNode, UndirectedGraphNode)] = {

    val edges: mutable.ListBuffer[(UndirectedGraphNode, UndirectedGraphNode)] = mutable.ListBuffer()

    nodes.foreach(node => node.connectedNodesWithoutWeight.foreach(connectedNode => {
      if (!edges.contains((node, connectedNode)) && !edges.contains((connectedNode, node)))
        edges.addOne((node, connectedNode))
    }))

    edges.toSet
  }

  def isNodeInGraph(node: UndirectedGraphNode): Boolean = nodes.contains(node)

  // This finds the minimum cut in the graph using Karger's algorithm, according to
  // https://web.stanford.edu/class/archive/cs/cs161/cs161.1176/Lectures/CS161Lecture16.pdf
  def findMinimumCut(): (Int, UndirectedGraph, UndirectedGraph) = {

    case class SuperNode(nodesArg: Set[UndirectedGraphNode]) {
      val nodes: mutable.Set[UndirectedGraphNode] = mutable.Set.from(nodesArg)

    }

    case class UnorderedPair[T](first: T, second: T) {

      override def hashCode(): Int = first.hashCode() ^ second.hashCode()

      override def equals(obj: Any): Boolean = if (obj.isInstanceOf[T]) {
        val otherPair = obj.asInstanceOf[UnorderedPair[_]]
        ((first == otherPair.first) && (second == otherPair.second)) || ((first == otherPair
          .second) && (second == otherPair.first))
      }
      else
        false
    }

    // Initially create a supernode from each input node
    val initialSuperNodeMap: Map[UndirectedGraphNode, SuperNode] = nodes.map(node => node -> SuperNode(Set(node)))
      .toMap

    val superEdgeMap: mutable.Map[UnorderedPair[SuperNode], Set[UnorderedPair[UndirectedGraphNode]]] = mutable.Map()

    val superNodes: mutable.Set[SuperNode] = mutable.Set.from(initialSuperNodeMap.values)

    getAllEdges
      .foreach { case (from, to) => superEdgeMap
        .update(UnorderedPair[SuperNode](initialSuperNodeMap(from), initialSuperNodeMap(to)),
          Set(UnorderedPair[UndirectedGraphNode](from, to)))
      }

    def mergeSuperNodes(a: SuperNode, b: SuperNode): Unit = {
      val x = SuperNode((a.nodes ++ b.nodes).toSet)
      require(superNodes.contains(a))
      superNodes.remove(a)
      require(superNodes.contains(b))
      superNodes.remove(b)
      require(superEdgeMap.contains(UnorderedPair[SuperNode](a, b)))
      superEdgeMap.remove(UnorderedPair[SuperNode](a, b))
      superNodes.foreach(d => {
        val Eda = superEdgeMap.getOrElse(UnorderedPair[SuperNode](d, a), Set())
        val Edb = superEdgeMap.getOrElse(UnorderedPair[SuperNode](d, b), Set())
        val Edx = Eda ++ Edb
        if (Edx.nonEmpty) {
          superEdgeMap.update(UnorderedPair[SuperNode](d, x), Eda ++ Edb)
          superEdgeMap.remove(UnorderedPair[SuperNode](d, a))
          superEdgeMap.remove(UnorderedPair[SuperNode](d, b))
        }
      })
      superNodes.addOne(x)
    }

    while (superNodes.size > 2) {
      val allSuperNodePairs = superEdgeMap.keySet.toSeq
      val uv = allSuperNodePairs((math.random() * allSuperNodePairs.size).floor.toInt)

      mergeSuperNodes(uv.first, uv.second)
    }

    (superEdgeMap.values.toSet.flatten.size, UndirectedGraph(
      superNodes.head.nodes.toSet), UndirectedGraph(
      superNodes.last.nodes.toSet))
  }

  /*
  def findCutsWithSize(size: Int): Seq[(UndirectedGraph, UndirectedGraph)] = {

    val cutSizeMemo: mutable.Map[Int, Int] = mutable.Map()
    val findCutSizeMemo: mutable.Map[Int, Unit] = mutable.Map()
    val result: mutable.ListBuffer[(UndirectedGraph, UndirectedGraph)] = mutable.ListBuffer()

    val jobQueue: mutable.Queue[() => Unit] = mutable.Queue()

    def getCutSize(first: UndirectedGraph, second: UndirectedGraph): Int = {
      val firstStr = first.nodes.map(_.id).sorted.mkString
      val secondStr = second.nodes.map(_.id).sorted.mkString
      val key = Seq(firstStr, secondStr).sorted.mkString("#").hashCode
      cutSizeMemo.getOrElseUpdate(key, {
        first.nodes
          .map(node => node.connectedNodes.count { case (connectedNode, _) => second.isNodeInGraph(connectedNode) }).sum
      })
    }

    def doFindCutsWithSize(visitedNodes: List[UndirectedGraphNode], size: Int): Unit = {

      val remainingNodes = nodes.diff(visitedNodes).toList
      val visitedGraph = UndirectedGraph(visitedNodes)
      val remainingGraph = UndirectedGraph(remainingNodes)

      if (getCutSize(visitedGraph, remainingGraph) == size)
        result.addOne((visitedGraph, remainingGraph))

      if (remainingGraph.nodes.nonEmpty) {
        visitedNodes.flatMap(_.connectedNodesWithoutWeight).distinct
          .filter(connectedNode => remainingNodes.contains(connectedNode))
          .foreach { connectedNode => {
            val key = (visitedNodes :+ connectedNode).map(_.id).sorted.mkString.hashCode
            findCutSizeMemo
              .getOrElseUpdate(key, jobQueue.enqueue(() => doFindCutsWithSize(visitedNodes :+ connectedNode, size)))
          }
          }
      }
    }

    jobQueue.enqueue(() => doFindCutsWithSize(List(nodes.head), size))

    while (jobQueue.nonEmpty)
      jobQueue.dequeue()()

    result.toSeq
  }

   */

}

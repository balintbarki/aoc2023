package aoctest.utils.graphs

import aoc.utils.graphs.{DirectedGraph, DirectedGraphNode}
import org.junit.{Assert, Test}

class TestDirectedGraph {

  @Test
  def testArea(): Unit = {

    val nodeSource = DirectedGraphNode("S")
    val node1 = DirectedGraphNode("N1") // This is to be reduced
    val node2 = DirectedGraphNode("N2")
    val node3 = DirectedGraphNode("N3")
    val node4 = DirectedGraphNode("N4")
    val node5 = DirectedGraphNode("N5")
    val nodeTarget = DirectedGraphNode("T")
    nodeSource.connectTo(node1)
    node1.connectTo(node2)
    node2.connectTo(node3)
    node3.connectTo(node4)
    node3.connectTo(node5)
    node4.connectTo(nodeTarget)
    node5.connectTo(nodeTarget)

    val graph = new DirectedGraph(List(nodeSource, node1, node2, node3, nodeTarget))
    val orignalWeight = graph.getTotalWeight
    graph.replaceNodesWithOneInOneOut
    val replacedWeight = graph.getTotalWeight

    Assert.assertEquals(3, graph.nodes.length)
    Assert.assertEquals(orignalWeight, replacedWeight)

  }
}

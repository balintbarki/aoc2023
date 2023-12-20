package aoc.aoc2023.day20

import aoc.aoc2023.DailyPuzzle2023

import scala.collection.mutable
import scala.util.matching.Regex

case object Day20Puzzle extends DailyPuzzle2023(20, "Pulse Propagation") {
  private val jobQueue: mutable.Queue[() => Unit] = mutable.Queue()

  override def calculatePart1(lines: Seq[String]): String = {

    parseInput(lines)

    (1 to 1000).foreach(_ => {
      jobQueue.enqueue(() => Broadcaster.pulseFrom(Button, Pulse.Low))
      processJobQueue()
    }
    )
    (Pulse.Low.triggerCnt * Pulse.High.triggerCnt).toString
  }

  override def calculatePart2(lines: Seq[String]): String = ???

  private def parseInput(lines: Seq[String]): Unit = {

    val modules: mutable.Map[String, Module] = mutable.Map()

    val regex = new Regex("""([\&\%]?\w+) -> (.*)""")
    val modulesWithTargets = lines.map {
      case regex(id, targets) =>
        val module: Module = if (id.startsWith("&")) {
          Conjunction(id.drop(1))
        } else if (id.startsWith("%")) {
          FlipFlop(id.drop(1))
        } else if (id == Broadcaster.id) {
          Broadcaster
        } else {
          throw new IllegalArgumentException(s"Unexpected module type: $id")
        }
        modules.update(module.id, module)
        (module, targets.split(",").map(_.trim))
    }


    modulesWithTargets.foreach { case (module, targets) =>
      targets.foreach(targetStr => {

        // If the target is not in the module map, it is considered as an output
        if (!modules.contains(targetStr)) {
          modules.update(targetStr, Output(targetStr))
        }

        val target = modules
          .getOrElse(targetStr, throw new IllegalArgumentException(s"Target $targetStr not found in configuration"))
        module.addTarget(target)
        target.addSource(module)
      }
      )
    }
  }

  private def processJobQueue(): Unit = {
    while (jobQueue.nonEmpty) {
      jobQueue.dequeue()()
    }
  }

  private abstract class Module(val id: String) {
    var sources: List[Module] = List()
    var targets: List[Module] = List()

    def addSource(source: Module): Unit = sources = sources :+ source

    def addTarget(target: Module): Unit = targets = targets :+ target

    final def pulseFrom(from: Module, pulse: Pulse): Unit = {
      pulse.triggered()
      processPulseFrom(from, pulse)
    }

    protected def processPulseFrom(from: Module, pulse: Pulse): Unit

    final protected def sendToAll(pulse: Pulse): Unit = {
      targets.foreach(target => jobQueue.enqueue(() => target.pulseFrom(this, pulse)))
    }
  }

  private abstract class Pulse {

    var triggerCnt: Long = 0

    def reset(): Unit = triggerCnt = 0

    def triggered(): Unit = triggerCnt = triggerCnt + 1
  }

  private case class FlipFlop(override val id: String) extends Module(id) {

    private var state: Boolean = false

    override protected def processPulseFrom(from: Module, pulse: Pulse): Unit = {
      if (pulse == Pulse.Low) {
        if (!state) {
          state = true
          sendToAll(Pulse.High)
        }
        else {
          state = false
          sendToAll(Pulse.Low)
        }
      }
    }
  }

  private case class Conjunction(override val id: String) extends Module(id) {
    private val lastInputs: mutable.Map[String, Pulse] = mutable.Map()

    override def addSource(source: Module): Unit = {
      super.addSource(source)
      lastInputs.update(source.id, Pulse.Low)
    }

    override protected def processPulseFrom(from: Module, pulse: Pulse): Unit = {
      lastInputs.update(from.id, pulse)
      if (lastInputs.values.forall(_ == Pulse.High)) {
        sendToAll(Pulse.Low)
      }
      else {
        sendToAll(Pulse.High)
      }
    }
  }

  private case class Output(override val id: String) extends Module("output") {
    override protected def processPulseFrom(from: Module, pulse: Pulse): Unit = {}
  }

  private case object Broadcaster extends Module("broadcaster") {

    addSource(Button)

    override protected def processPulseFrom(from: Module, pulse: Pulse): Unit = {
      sendToAll(pulse)
    }
  }

  private case object Button extends Module("button") {

    addTarget(Broadcaster)

    override protected def processPulseFrom(from: Module, pulse: Pulse): Unit = {}
  }

  private object Pulse {

    case object High extends Pulse

    case object Low extends Pulse

  }


}

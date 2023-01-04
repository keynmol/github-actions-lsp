//> using lib "tech.neander::langoustine-app::0.0.19"
//> using lib "co.fs2::fs2-io::3.4.0"
//> using lib "org.yaml:snakeyaml:1.33"
//> using scala "3.2.1"

import org.yaml.snakeyaml.*
import org.yaml.snakeyaml.nodes.*
import scala.jdk.CollectionConverters.*
import java.io.{StringReader, File}

@main def hello(file: String) =
  processWorkflowFile(new File(file))
end hello

def processWorkflowFile(f: File) =
  processWorkflowText(scala.io.Source.fromFile(f).getLines.mkString("\n"))

def processWorkflowText(s: String) =
  val yaml = Yaml()
  val loaded = yaml.compose(
    new StringReader(s)
  )
  loaded match
    case mp: nodes.MappingNode =>
      processTopLevel(scalify(mp))
    case _ => err("This document is not a workflow file,\n it's dogshit")

case class Derived[T](from: Node, value: T)

def scalify(mp: MappingNode): Derived[Map[String, Node]] =
  Derived(
    mp,
    mp
      .getValue()
      .asScala
      .map(nd => nd.getKeyNode() -> nd.getValueNode())
      .collect { case (sn: nodes.ScalarNode, value) =>
        sn.getValue() -> value
      }
      .toMap
  )

def processTopLevel(mp: Derived[Map[String, nodes.Node]]) =
  if !mp.value.contains("jobs") then
    err("Where are the jobs: you fucking idiot?", mp.from)
  val jobs = mp.value("jobs")
  jobs match
    case mp: MappingNode => processJobs(scalify(mp))
    case other =>
      err("jobs: needs to be a key-value thing, what the fuck is that?", other)

def processJobs(mp: Derived[Map[String, Node]]) =
  mp.value.collect { case (name, m: MappingNode) =>
    processSingleJob(name, scalify(m))
  }

def processSingleJob(name: String, node: Derived[Map[String, Node]]) =
  if !node.value.contains("runs-on") then
    err("what it gonna run on you dumbass? add `runs-on:`", node.from)

  println(node)

case class Err(msg: String, what: Option[Node]) extends Throwable(msg)

def err(msg: String, n: Node) = throw Err(msg, Some(n))
def err(msg: String) = throw Err(msg, None)

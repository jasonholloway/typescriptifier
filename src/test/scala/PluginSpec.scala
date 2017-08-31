import java.nio.file.{Files, Paths}

import org.scalactic.source.Position
import org.scalatest.{BeforeAndAfter, BeforeAndAfterEach, FlatSpec, Matchers}
import woodpigeon.typescriptifier.TsSchemaPlugin

import scala.io.Source
import scala.tools.nsc._
import scala.tools.nsc.io._
import scala.tools.nsc.reporters.ConsoleReporter

class PluginSpec extends FlatSpec with Matchers with BeforeAndAfter {

  def compile(sources: List[String], outputDir: String): Unit = {
    val settings = new Settings()
    settings.embeddedDefaults[TsSchemaSpec]
    settings.plugin.value = List("C:\\dev\\bb\\store\\ts-schema\\target\\scala-2.12\\ts-schema_2.12-0.0.1.jar")
    settings.require.value = List("ts-schema")

    settings.pluginOptions.value = List(s"ts-schema:output-dir:$outputDir")

    val global = new Global(settings, new ConsoleReporter(settings))

    val run = new global.Run()

    val files = sources.map(toTempFile)

    run.compile(files.map(f => f.getAbsolutePath).toList)

  }

  private def toTempFile(src: String) = {
    val file = java.io.File.createTempFile("typescriptifier", ".scala")
    file.deleteOnExit()

    val writer = new java.io.PrintWriter(file)
    try writer.write(src.stripMargin)
    finally writer.close()

    file
  }

  private def createTempDir(tmpName: String): String = {
    val tmpDir = Paths.get(System.getProperty("java.io.tmpdir"))
    val name = tmpDir.getFileSystem.getPath(tmpName)
    if (name.getParent != null) throw new IllegalArgumentException("Invalid prefix or suffix")
    tmpDir.resolve(name).toString
  }


  "Plugin" should "run without blowing up" in {
    val sources = List(s"""
     |import woodpigeon.typescriptifier._
     |
     |@model
     |class Duck(bill: Boolean)
     |
     |@model
     |case class Mallard(bill: Boolean, blahblah: Integer) extends Duck(bill)
     |
     """)

    compile(sources, Files.createTempDirectory("typescriptifier").toString)
  }


  "Each schematized class" should "be saved to named ts file" in {
    val sources = List(s"""
     |import woodpigeon.typescriptifier._
     |
     |@model
     |class Duck(bill: Boolean)
     |
     |@model
     |case class Mallard(bill: Boolean, blahblah: Integer) extends Duck(bill)
     |
     """)

    val outputDir = Files.createTempDirectory("typescriptifier").toString

    compile(sources, outputDir)

    val savedFileNames = Directory(outputDir).files.map(f => f.name).toSet
    savedFileNames should (contain("Duck.ts") and contain("Mallard.ts"))
  }


  "TS Schema" should "contain class header" in {
    val source =
      s"""
         |import woodpigeon.typescriptifier._
         |
         |@model
         |case class Weasel(name: String, hasFur: Boolean)
         |
       """

    val outputDir = Files.createTempDirectory("typescriptifier").toString
    compile(List(source), outputDir)

    val output = Source.fromFile(outputDir + "/Weasel.ts").mkString
    output should include regex """(?s)class Weasel \{.*\}"""
  }


  "TS Schema" should "contain simple class members" in {
    val source =
      s"""
         |import woodpigeon.typescriptifier._
         |
         |@model
         |case class Wolverine(name: String, hasFur: Boolean)
         |
       """

    val outputDir = Files.createTempDirectory("typescriptifier").toString
    compile(List(source), outputDir)

    val output = Source.fromFile(outputDir + "/Wolverine.ts").mkString
    output should include regex """name: string;"""
    output should include regex """hasFur: boolean;"""
  }


}


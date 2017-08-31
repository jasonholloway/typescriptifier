package woodpigeon.typescriptifier

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.io.File

class TypescriptifierPlugin(val global: Global) extends Plugin {

  override val name: String = "typescriptifier"
  override val description: String = "Generates TypeScript class defs from Scala case classes"

  override val components: List[PluginComponent] = List(Component)

  private val extractOption = "([a-zA-Z-_]+):(.*)".r
  private var optionMap = Map[String, String]()

  override def processOptions(options: List[String], error: (String) => Unit): Unit = {
    optionMap = options.flatMap {
      case extractOption(key, value) => Some(key, value)
      case _ => None
    }.toMap
  }

  private def outputDir = optionMap.getOrElse("output-dir", "/")


  private object Component extends PluginComponent {
    val global: Global = TypescriptifierPlugin.this.global
    import global._

    override val phaseName: String = "ts-schema"
    override val runsAfter: List[String] = List("typer")
    override val runsBefore: List[String] = List("patmat")

    override def newPhase(prev: Phase) = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = generateSchema(unit)
    }

    def generateSchema(unit: CompilationUnit) : Unit = {
      findTargetClasses(unit.body).foreach { classDef =>
        val className = classDef.symbol.tpe.toString
        val path = s"$outputDir\\${className}.ts"

        val writer = File(path).createFile().printWriter()

        writer.write(s"export class $className {\n")

        val innards = findAccessors(classDef) map {
          case (name, typeTree) =>
            s"    ${name}: ${formatType(typeTree).get};\n"
        }
        innards.foreach { writer.write(_) }

        writer.write("}\n")

        writer.close
      }

//      global.treeBrowsers.create().browse("ts-schema", List(unit))
    }


    def allTrees(tree: Tree) : Iterator[Tree] =
      Iterator(tree) ++ tree.children.flatMap(allTrees(_))


    def findTargetClasses(tree: Tree) = {
      allTrees(tree) flatMap {
        case n @ ClassDef(_, _, _, _)
          if n.symbol.annotations.exists(isTargetAnnotation) => Some(n)
        case _ => None
      }
    }

    def findAccessors(tree: Tree) = {
      allTrees(tree) flatMap {
        case n @ DefDef(mods, name, _, _, typeTree, _)
          if mods.hasFlag(0x01000000) => Some(name, typeTree)
        case _ => None
      }
    }

    def formatType(tree: Tree): Option[String] = {
      tree.toString match {
        case "Boolean" => Some("boolean")
        case "String" => Some("string")
        case _ => None
      }
    }


    val targetAnnotName = typeOf[model].toString

    def isTargetAnnotation(annot: AnnotationInfo) = annot.tpe.toString == targetAnnotName


  }

}
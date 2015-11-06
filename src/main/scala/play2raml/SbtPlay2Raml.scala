package play2raml

import java.io.File

import org.apache.commons.io.FileUtils
import play.api.PlayException
import play.core.PlayVersion
import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import play.routes.compiler.{RoutesFileParser, RoutesCompilationError, RoutesGenerator}
import play.sbt.Play
import play.sbt.routes._
import sbt._
import Keys._

import scala.io.Codec


object SbtPlay2Raml extends AutoPlugin {

  object autoImport {
    val keys = RoutesKeys
    val ramlRoutesCompilerTasks = TaskKey[Seq[RoutesCompilerTask]]("playRamlRoutesTasks", "The raml routes files to compile")
  }

  import autoImport.keys._

  override def requires = Play

  override def trigger = allRequirements

  override lazy val projectSettings =
    RoutesCompiler.defaultSettings ++
      inConfig(Compile)(routesSettings) ++
      inConfig(Test)(routesSettings)

  def routesSettings = Seq(
    autoImport.ramlRoutesCompilerTasks <<= Def.taskDyn {
      // Aggregate all the routes file tasks that we want to compile the reverse routers for.
      aggregateReverseRoutes.value.map { agg =>
        autoImport.ramlRoutesCompilerTasks in(agg.project, configuration.value)
      }.join.map { aggTasks: Seq[Seq[RoutesCompilerTask]] =>

        // Aggregated tasks need to have forwards router compilation disabled and reverse router compilation enabled.
        val reverseRouterTasks = aggTasks.flatten.map { task =>
          task.copy(forwardsRouter = false, reverseRouter = true)
        }

        def routesSources = {
          val dirs = (unmanagedResourceDirectories in Compile).value
          (dirs * "raml").get ++ (dirs * "*.raml").get
        }
        val thisProjectTasks = routesSources.map { file =>
          RoutesCompilerTask(file, routesImport.value, forwardsRouter = true,
            reverseRouter = generateReverseRouter.value, namespaceReverseRouter = namespaceReverseRouter.value)
        }

        thisProjectTasks ++ reverseRouterTasks
      }
    },
    sourceGenerators <+= compileRamlRoutesFiles
  )

  private val compileRamlRoutesFiles = Def.task[Seq[File]] {
    compileRoutes(autoImport.ramlRoutesCompilerTasks.value, routesGenerator.value, (target in routes).value, streams.value.cacheDirectory,
      state.value.log)
  }

  def compileRoutes(tasks: Seq[RoutesCompilerTask], generator: RoutesGenerator, generatedDir: File,
                    cacheDirectory: File, log: Logger): Seq[File] = {
    val ops = tasks.map(task => RoutesCompilerOp(task, generator.id, PlayVersion.current))

    val (products, errors) = {
      val results = tasks.map { task =>
        compile(task, generator, generatedDir)
      }
      val (lefts, rights) = results.partition(_.isLeft)
      val errors = lefts.flatMap { case Left(e) => e }
      val files = rights.flatMap { case Right(r) => r }
      (files, errors)
    }

    if (errors.nonEmpty) {
      val exceptions = errors.map {
        case RoutesCompilationError(source, message, line, column) =>
          reportCompilationError(log, RoutesCompilationException(source, message, line, column.map(_ - 1)))
      }
      throw exceptions.head
    }

    products.to[Seq]
  }

  def compile(task: RoutesCompilerTask, generator: RoutesGenerator, generatedDir: File): Either[Seq[RoutesCompilationError], Seq[File]] = {

    val namespace = Option(task.file.getName).filter(_.endsWith(".routes")).map(_.dropRight(".routes".size))
    val routeFile = task.file.getAbsoluteFile
    val parsed = RoutesFileParser.parse(routeFile)
    parsed.right.map { rules =>
      val generated = generator.generate(task, namespace, rules)
      generated.map {
        case (filename, content) =>
          val file = new File(generatedDir, filename)
          FileUtils.writeStringToFile(file, content, implicitly[Codec].name)
          println(s"Writing file: $filename")
          file
      }
    }
  }

  private def reportCompilationError(log: Logger, error: PlayException.ExceptionSource) = {
    // log the source file and line number with the error message
    log.error(Option(error.sourceName).getOrElse("") + Option(error.line).map(":" + _).getOrElse("") + ": " + error.getMessage)
    Option(error.interestingLines(0)).map(_.focus).flatMap(_.headOption) foreach { line =>
      // log the line
      log.error(line)
      Option(error.position).foreach { pos =>
        // print a carat under the offending character
        val spaces = (line: Seq[Char]).take(pos).map {
          case '\t' => '\t'
          case x => ' '
        }
        log.error(spaces.mkString + "^")
      }
    }
    error
  }

}

private case class RoutesCompilerOp(task: RoutesCompilerTask, generatorId: String, playVersion: String)

case class RoutesCompilationException(source: File, message: String, atLine: Option[Int], column: Option[Int]) extends PlayException.ExceptionSource(
  "Compilation error", message) with FeedbackProvidedException {
  def line = atLine.map(_.asInstanceOf[java.lang.Integer]).orNull

  def position = column.map(_.asInstanceOf[java.lang.Integer]).orNull

  def input = IO.read(source)

  def sourceName = source.getAbsolutePath
}
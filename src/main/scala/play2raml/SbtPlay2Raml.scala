package play2raml

import com.typesafe.sbt.web.incremental._
import play.api.PlayException
import play.core.PlayVersion
import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import play.routes.compiler.{RoutesCompilationError, RoutesGenerator}
import play.sbt.Play
import play.sbt.routes._
import sbt._
import Keys._


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
  // probably needs to be added
  /*  watchSources in Defaults.ConfigGlobal <++= sources in routes,

    target in routes := crossTarget.value / "routes" / Defaults.nameForSrc(configuration.value.name),

    routes <<= compileRamlRoutesFiles,

    sourceGenerators <+= routes,
    managedSourceDirectories <+= target in routes,*/

    sourceGenerators in Compile <+= compileRamlRoutesFiles
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
        play.routes.compiler.RoutesCompiler.compile(task, generator, generatedDir)
      }
      val t = results.groupBy(_.isRight)

      val files = results.flatMap {
        case Right(f) => f
      }
      val errors = results.flatMap {
        case Left(e) => e
      }
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


  private def reportCompilationError(log: Logger, error: PlayException.ExceptionSource) = {
    // log the source file and line number with the error message
    log.error(Option(error.sourceName).getOrElse("") + Option(error.line).map(":" + _).getOrElse("") + ": " + error.getMessage)
    Option(error.interestingLines(0)).map(_.focus).flatMap(_.headOption) map { line =>
      // log the line
      log.error(line)
      Option(error.position).map { pos =>
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
package toolc

import utils._
import lexer._
import java.io.File

object Main {

  def processOptions(args: Array[String]): Context = {
    val (opts, files) = args.toSeq.partition(_.startsWith("--"))
    val reporter = new Reporter()

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, files = new File(files.head) :: Nil)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)
    val pipeline = Lexer andThen
                   DisplayTokens
    pipeline.run(ctx)(ctx.files.head)
    ctx.reporter.terminateIfErrors()
  }

}

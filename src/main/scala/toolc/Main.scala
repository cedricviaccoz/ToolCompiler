package toolc

import utils._
import java.io.File

import lexer._
import ast._
import eval._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {
    val reporter = new Reporter()
    var files: List[File] = Nil
    var outDir: Option[File] = None
    var isJBytecode = false

    def rec(args: List[String]): Unit = args match {

      case "-bytecode" :: xs =>
        isJBytecode = true
        rec(xs)

      case "-d" :: dir :: xs =>
        outDir = Some(new File(dir))
        rec(xs)

      case f :: xs =>
        files  ::= new File(f)
        rec(xs)

      case _ =>
    }

    rec(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, files = files, outDir = outDir, isBytecode = isJBytecode)
  }

  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val ASTPipeline = Lexer andThen
                   Parser andThen
                   NameAnalysis andThen
                   TypeChecking

    val pipeline =  ASTPipeline andThen (if(ctx.isBytecode) CodeGeneration else COutputGeneration)

    pipeline.run(ctx)(ctx.files.head)

    ctx.reporter.terminateIfErrors
  }
}

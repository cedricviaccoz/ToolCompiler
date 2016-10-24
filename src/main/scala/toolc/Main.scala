package toolc

import utils._
import lexer._
import ast._
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
    val pipeline = Lexer andThen Parser
    val ast = pipeline.run(ctx)(ctx.files.head)
    ctx.reporter.terminateIfErrors()
    println(Printer(ast))
    
    /*val result = grammarcomp.grammar.GrammarUtils.nullableFirstFollow(Parser.ll1Grammar)
    println("Nullables :")
    for( n <- result._1){
      println("   "+n)
    }
    println("Firsts :")
    for( n <- result._2){
      println("   "+n._1+ " ==> "+n._2)
    }
    println("Follows :")
    for( n <- result._3){
      
      println("   "+n._1 + " ==> "+n._2)
    }*/
  }

}

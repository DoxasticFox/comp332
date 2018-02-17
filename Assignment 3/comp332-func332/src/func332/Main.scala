/**
 * Func332 implementation main program.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

import Func332Tree.Expression
import org.kiama.output.PrettyPrinter
import org.kiama.util.ParsingREPL

/**
 * Conduct syntax analysis on the Func332 program in the file given as the
 * first command-line argument. If the file is syntactically valid, go on
 * to perform semantic checks, and if they pass, translate the code into
 * SEC code and run it.
 */
object Main extends SyntaxAnalysis with PrettyPrinter with ParsingREPL[Expression] {

    import Func332Tree.{Program, SourceNode}
    import org.kiama.attribution.Attribution.initTree
    import SemanticAnalysis.check
    import Translator.translate
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging.{messagecount, report, resetmessages}
    import java.io.FileReader
    import java.io.FileNotFoundException

    override def main (args : Array[String]) {

        args.size match {

            // If there are no command-line arguments, we want to enter a
            // read-eval-print-loop (REPL) to read expressions and run them
            // one-by-one.
            case 0 =>
                super.main (args)

            // If there is exactly one command-line argument, we want to
            // compile and run that file.
            case 1 =>
                try {
                    // Create a reader for the argument file name
                    val reader = new FileReader (args (0))

                    // Parse the file
                    parse (parser, reader) match {

                        // If it worked, we get a source tree
                        case Success (sourcetree, x_) =>
                            // Pretty print the source tree
                            // println (pretty_any (sourcetree))

                            // Initialise node properties
                            initTree (sourcetree)

                            // Compile and run the program source tree
                            compileAndRun (sourcetree)

                        // Parsing failed, so report it
                        case f =>
                            println (f)
                    }
                } catch {
                    case e : FileNotFoundException =>
                        println (e.getMessage)
                }

            // Complain otherwise
            case _ =>
                println ("usage: run [file.func332]")

        }

    }

    /**
     * The prompt to use in REPL mode.
     */
    override val prompt = "func332> "

    /**
     * The parser to use in REPL mode. We parse each line entered by the user
     * as an expression (not a program).
     */
    val start = exp

    /**
     * Process a parsed input line in REPL mode. Just compile and run it as
     * a program containing a single expression.
     */
    def process (exp : Expression) {
        compileAndRun (Program (List (exp)))
    }

    /**
     * Compile and run a program source tree. First analyse it to check for
     * semantic errors.  If any messages are produced, print them and abort.
     * Otherwise, translate the program into SEC instructions and run them.
     */
    def compileAndRun (tree : Program) {
        // Analysis
        resetmessages
        check (tree)
        if (messagecount > 0)
            report (new Emitter)
        else {
            // Translation
            val instrs = translate (tree)
            // println (instrs)

            // A machine to perform the run
            val machine = new SECMachine

            // Execution
            machine.run (instrs) match {
                case machine.FatalError (message) =>
                    println ("execution error: " + message)
                case _ =>
                    // All ok, do nothing
            }
        }
    }

}

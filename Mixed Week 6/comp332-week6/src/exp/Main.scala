/**
 * Expression language implementation main program.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.kiama.output.PrettyPrinter

/**
 * Syntax analyse the expression language program in the file given as the
 * first command-line argument and process the source tree to perform naming
 * semantic analysis checks.
 */
object Main extends SyntaxAnalysis with PrettyPrinter {

    import ExpTree._
    import org.kiama.attribution.Attribution.initTree
    import SemanticAnalysis._
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging.{messagecount, report, resetmessages}
    import java.io.FileReader
    import java.io.FileNotFoundException

    def main (args : Array[String]) {

        args.size match {

            // If there is exactly one command-line argument
            case 1 =>
                try {
                    // Create a reader for the argument file name
                    val reader = new FileReader (args (0))

                    // Parse the file
                    parse (parser, reader) match {

                        // If it worked, we get a source tree
                        case Success (sourcetree, _) =>
                            // Pretty print the source tree
                            // println (pretty (any (sourcetree)))

                            // Initialise node properties
                            initTree (sourcetree)

                            // Process the source tree
                            process (sourcetree)

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
                println ("usage: run file.exp")

        }

    }

    /**
     * Process the source tree by analysing it to check for semantic
     * errors.  If any messages are produced, print them.
     */
    def process (tree : ExpProgram) {
        resetmessages
        check (tree)
        if (messagecount > 0)
            report (new Emitter)
    }

}

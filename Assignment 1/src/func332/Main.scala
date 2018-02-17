/**
 * Func332 implementation main program.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

import org.kiama.output.PrettyPrinter

/**
 * Conduct syntax analysis on the Func332 program in the file given as the
 * first command-line argument. If the file is syntactically valid, go on
 * to perform semantic checks, and if they pass, translate the code into
 * SECD code and run it.
 */
object Main extends SyntaxAnalysis with PrettyPrinter {

    import java.io.FileReader
    import java.io.FileNotFoundException
    import Func332Tree._
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging.{messagecount, report, resetmessages}

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
                            println (pretty (any (sourcetree)))

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
                println ("usage: run file.f")
        }

    }

}

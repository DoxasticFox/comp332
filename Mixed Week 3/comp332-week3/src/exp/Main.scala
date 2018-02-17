/**
 * Expression language implementation main program.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.kiama.output.PrettyPrinter

/**
 * Syntax analyse the expression language program in the file given as the
 * first command-line argument and print the source tree.
 */
object Main extends SyntaxAnalysis with PrettyPrinter {

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
                println ("usage: run file.exp")

        }

    }

}

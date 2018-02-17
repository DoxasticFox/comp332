/**
 * Expression language semantics analysis test support.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.scalatest.FunSuite

/**
 * Support code for semantic test suites.
 */
class SemanticTests extends SyntaxAnalysis with FunSuite {

    import ExpTree._
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.Messaging.{messagecount, messages, resetmessages}
    import SemanticAnalysis.check

    /**
     * Parse some test input and, if the parse succeeds with no input left,
     * return the program tree. If the parse fails, fail the test.
     */
    def parseProgram (str : String) : ExpProgram =
        parseAll (parser, str) match {
            case Success (r, in) =>
                if (!in.atEnd) fail ("input remaining at " + in.pos)
                r
            case f : Error =>
                fail ("parse error: " + f)
            case f : Failure =>
                fail ("parse failure: " + f)
        }

    /**
     * Parse some test input and run the name analyser over the resulting
     * tree (if the parse succeeds).
     */
    def semanticTest (str : String) {
        val prog = parseProgram (str)
        initTree (prog)
        resetmessages
        check (prog)
        // println (messages)
    }

    /**
     * Assert that a message was produced at a given position.
     */
    def assertMessage (index : Int, line : Int, column : Int, msg : String) {
        val m = messages (index)
        expectResult (msg, "wrong text in message " + index) (m.message)
        expectResult (line, "wrong line number in message " + index) (m.pos.line)
        expectResult (column, "wrong column number in message " + index) (m.pos.column)
    }

}

/**
 * Expression language name analysis tests.
 *
 * Copyright 2012-2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the name analysis works correctly.  I.e., we run
 * the checking process over some input and make sure that the expected
 * errors were produced (and only them).
 */
@RunWith(classOf[JUnitRunner])
class NameTests extends SyntaxAnalysis with FunSuite {

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

    test ("can't have two variables with same name") {
        semanticTest ("""
            |decl v
            |decl v
            """.stripMargin)
        assert (messagecount == 1)
        assertMessage (0, 3, 6, "v is declared more than once")
    }

    test ("can't have a constant and a variable with same name") {
        semanticTest ("""
            |const c = 10
            |decl c
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 3, 6, "c is declared more than once")
    }

    test ("can't set a constant") {
        semanticTest ("""
            |const c = 10
            |set c = 11
            """.stripMargin)
        assert (messagecount == 1)
        assertMessage (0, 3, 5, "c is not variable, so cannot be set")
    }

    // FIXME: more tests needed here...
	test ("can't initialise a constant with a non-constant expression") {
		semanticTest ("""
			|decl a
			|set a = 5
			|const b = a""".stripMargin)
		assert (messagecount == 1)
		assertMessage (0, 4, 11, "initialising expression is not constant")
	}
	
	test ("can't set undeclared variable") {
		semanticTest ("set a = 4")
		assert (messagecount == 1)
		assertMessage (0, 1, 5, "a is not declared")
	}
	
	test ("variable can be set") {
		semanticTest ("""
			|decl v
			|set v = 5""".stripMargin)
		assert (messagecount == 0)
	}
	
	test ("variable value can be changed") {
		semanticTest ("""
			|decl v
			|set v = 5
			|set v = 42""".stripMargin)
		assert (messagecount == 0)
	}
	
	test ("constant can be initialised with trivial expression") {
		semanticTest ("const c = 104729")
		assert (messagecount == 0)
	}
	
	test ("constant can be initialised with novel expression") {
		semanticTest ("""
			|const a = 5
			|const b = a*3""".stripMargin)
		assert (messagecount == 0)
	}
}

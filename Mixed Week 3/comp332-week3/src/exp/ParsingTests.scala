/**
 * Expression language tests.
 *
 * Copyright 2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class ParsingTests extends SyntaxAnalysis with FunSuite {

    import ExpTree._

    test ("parsing digits as a factor gives the correct tree") {
        assertParseOk ("1203", factor, IntExp (1203))
    }

    test ("parsing a non-integer as an integer gives an error") {
        assertParseError ("hello", integer, "string matching regex `[0-9]+' expected but `h' found")
    }

    test ("parsing a simple expression produces the correct tree") {
        assertParseOk ("2 + 4", expression,
            PlusExp (IntExp (2), IntExp (4)))
    }

    test ("parsing an expression with associative operators produces the correct tree") {
        assertParseOk ("2 + 4 * 6", expression,
            PlusExp (IntExp (2), StarExp (IntExp (4), IntExp (6))))
    }

	test ("parsing a negative expression gives the correct tree") {
		assertParseOk ("-1203", factor, NegExp ( IntExp (1203)))
	}
	
	test ("parsing an expression with the subtraction operator produces the correct tree") {
		assertParseOk ("42 - 1203", expression,
			MinusExp (IntExp (42), IntExp (1203)))
	}
	
	test ("parsing a complex 'set' expression gives the correct tree") {
		assertParseOk ("set variable = 44 * 3 - 2", statement,
			SetStmt (IdnExp ("variable"),
				MinusExp (StarExp (IntExp (44), IntExp (3)), IntExp (2))))
	}
	
	test ("parsing multiple statements gives a program") {
		assertParseOk ("set a = 4\nset b = -1", parser,
			ExpProgram (
				List (
					SetStmt (IdnExp ("a"), IntExp (4) ),
					SetStmt (IdnExp ("b"), NegExp (IntExp (1))))))
	}
	
	// Testing if statement
	test ("parsing if statement with simple condition and simple body gives correct tree") {
		assertParseOk ("if (0) { set a = 4 }", statement,
			IfStmt (
				IntExp (0),
				List (
					SetStmt (
						IdnExp ("a"),
						IntExp (4)))))
	}

	test ("parsing if statement with compound condition gives correct tree") {
		assertParseOk ("if (4*2/1) { set a = 4 }", statement,
			IfStmt (
				SlashExp (
					StarExp (
						IntExp (4),
						IntExp (2)),
					IntExp (1)),
				List (
					SetStmt (
						IdnExp ("a"),
						IntExp (4)))))
	}
	
	test ("parsing if statement with multiple statements in body gives correct tree") {
		assertParseOk ("if (1) { set a = 4\nset b = 0 }", statement,
			IfStmt (
				IntExp (1),
				List (
					SetStmt (
						IdnExp ("a"),
						IntExp (4)),
					SetStmt (
						IdnExp ("b"),
						IntExp (0)))))
	}
	
	test ("parsing if statement whose condition lacks parentheses produces error") {
		assertParseError ("if 1 {set a = 4}", statement, "string matching regex `\\z' expected but `1' found")
	}
	
	test ("parsing if statement without condition produces error (1)") {
		assertParseError ("if {set a = 4}", statement, "string matching regex `\\z' expected but `{' found")
	}

	test ("parsing if statement without condition produces error (2)") {
		assertParseError ("if ( ) {set a = 4}", statement, "string matching regex `[a-zA-Z]+' expected but `)' found")
	}
	
	test ("parsing if statement whose body lacks curly brackets produces error") {
		assertParseError ("if (1) set a = 4", statement, "`{' expected but `s' found")
	}
	
	test ("parsing if statement without body produces error (1)") {
		assertParseError ("if (1)", statement, "`{' expected but end of source found")
	}
	
	test ("parsing if statement without body produces error (2)") {
		assertParseError ("if (1) { }", statement, "string matching regex `[a-zA-Z]+' expected but `}' found")
	}
	
    /**
     * Try to parse str as a T, which is expected to work and produce the
     * given result.  Assert a failure if it doesn't or if it doesn't
     * consume all of the input.
     */
    def assertParseOk[T] (str : String, p : Parser[T], result : T) {
        parseAll (p, str) match {
            case Success (r, in) =>
                if (r != result) fail ("found '" + r + "' not '" + result + "'")
                if (!in.atEnd) fail ("input remaining at " + in.pos)
            case f =>
                fail ("parse failure: " + f)
        }
    }

    /**
     * Try to parse str as a T, which is expected to fail with a given
     * message.  Fail the test if it doesn't.
     */
    def assertParseError[T] (str : String, p : Parser[T], msg : String) {
        parseAll (p, str) match {
            case Success (_, _) =>
                fail ("expected to find parse error in " + str)
            case e : NoSuccess =>
                expectResult (msg, "wrong message in error") (e.msg)
        }
    }

}

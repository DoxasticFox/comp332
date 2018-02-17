/**
 * MiniJava parser implementation tests.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the parser works correctly.  I.e., it accepts correct
 * input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class ParsingTests extends SyntaxAnalysis with FunSuite {

    import Func332Tree._
	
	test ("parsing a simple program gives the correct tree") {
		assertParseOk ("1234;\n 16 + 32;", program, Program (List (IntExp (1234), PlusExp (IntExp (16), IntExp (32)))))
	}
	
	test ("parsing a multiplication expression produces the correct tree") {
		assertParseOk ("42 * a", exp, StarExp (IntExp (42), IdnExp (IdnUse ("a"))))
	}

	test ("parsing a division expression produces the correct tree") {
		assertParseOk ("a / 0", exp, SlashExp (IdnExp (IdnUse ("a")), IntExp (0)))
	}

	test ("* is left associative") {
		assertParseOk ("a * b * c", exp,
			StarExp (StarExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
	}

	test ("/ is left associative") {
		assertParseOk ("a / b / c", exp,
			SlashExp (SlashExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
	}

	test ("parsing a function application expression produces the correct tree") {
		assertParseOk ("func 42", exp, AppExp (IdnExp (IdnUse ("func")), IntExp (42)))
	}

	test ("parsing a let expression produces the correct tree") {
		assertParseOk ("let a = 1 in a", exp,
			LetExp (
				IdnDef("a"),
				IntExp (1),
				IdnExp (IdnUse ("a"))))
	}

	test ("parsing a less than expression produces the correct tree") {
		assertParseOk ("0 < 1", exp, LessExp (IntExp (0), IntExp (1)))
	}

	test("parsing a conditional expression produces the correct tree"){
		assertParseOk ("if a = b then x else y", exp, 
			IfExp (
				EqualExp (
					IdnExp (IdnUse ("a")),
					IdnExp (IdnUse ("b"))),
				IdnExp (IdnUse ("x")),
				IdnExp (IdnUse ("y"))))
	}

	test ("parsing an anonymous function expression produces the correct tree") {
		assertParseOk ("(a : bool) => true ", exp,
			FunExp (IdnDef("a"),
				BoolType (),
				BoolExp (true)))
	}

	test ("parsing a chain of anonymous function expressions produces the correct tree") {
		assertParseOk ("(a : int) => (b : int) => x + y", exp, 
			FunExp (
				IdnDef ("a"),
				IntType (),
				FunExp (
					IdnDef ("b"),
					IntType (),
					PlusExp (
						IdnExp (IdnUse ("x")),
						IdnExp (IdnUse ("y"))))))
	}
	
    test ("+ has lower precedence than / (to right)") {
        assertParseOk ("a / b + c", exp,
            PlusExp (SlashExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("- has lower precedence than / (to right)") {
        assertParseOk ("a / b - c", exp,
            MinusExp (SlashExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }
	
    test ("+ has lower precedence than / (to left)") {
        assertParseOk ("a + b / c", exp,
            PlusExp (IdnExp (IdnUse ("a")), SlashExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("- has lower precedence than / (to left)") {
        assertParseOk ("a - b / c", exp,
            MinusExp (IdnExp (IdnUse ("a")), SlashExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    // FIXME: you need to define tests that give you confidence that your parser
    // is correctly implementing the assignment specification. The tests below
    // should not need to be changed. They test some simple specifications and
    // also give you suggestions for the types of tests you might want to have.

    // Tests of parsing terminals

    test ("parsing an identifier of one letter produces the correct tree") {
        assertParseOk ("x", identifier, "x")
    }

    test ("parsing an identifier as an identifier produces the correct tree") {
        assertParseOk ("count", identifier, "count")
    }

    test ("parsing an identifier containing digits and underscores produces the correct tree") {
        assertParseOk ("x1_2_3", identifier, "x1_2_3")
    }

    test ("parsing an integer as an identifier gives an error") {
        assertParseError ("42", identifier, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9_]*' expected but `4' found")
    }

    test ("parsing a non-identifier as an identifier gives an error (digit)") {
        assertParseError ("4foo", identifier, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9_]*' expected but `4' found")
    }

    test ("parsing a non-identifier as an identifier gives an error (underscore)") {
        assertParseError ("_f3", identifier, 1, 1,
            "string matching regex `[a-zA-Z][a-zA-Z0-9_]*' expected but `_' found")
    }

    test ("parsing a keyword as an identifier gives an error") {
        assertParseError ("let ", identifier, 1, 1, "Expected failure")
    }

    test ("parsing a keyword prefix as an identifier produces the correct tree") {
        assertParseOk ("letter", identifier, "letter")
    }

    test ("parsing an integer of one digit as an integer produces the correct tree") {
        assertParseOk ("8", integer, "8")
    }

    test ("parsing an integer as an integer produces the correct tree") {
        assertParseOk ("99", integer, "99")
    }

    test ("parsing a non-integer as an integer gives an error") {
        assertParseError ("total", integer, 1, 1,
            "string matching regex `[0-9]+' expected but `t' found")
    }

    // Tests of parsing basic expressions

    test ("parsing an addition expression produces the correct tree") {
        assertParseOk ("a + 1", exp, PlusExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a subtraction expression produces the correct tree") {
        assertParseOk ("a - 1", exp, MinusExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing an integer expression produces the correct tree") {
        assertParseOk ("823", exp, IntExp (823))
    }

    test ("parsing a true expression produces the correct tree") {
        assertParseOk ("true", exp, BoolExp (true))
    }

    test ("parsing a false expression produces the correct tree") {
        assertParseOk ("false", exp, BoolExp (false))
    }

    test ("parsing an identifier expression produces the correct tree") {
        assertParseOk ("v123", exp, IdnExp (IdnUse ("v123")))
    }

    test ("parsing a parenthesized expression produces the correct tree") {
        assertParseOk ("(a + 5)", exp,
            PlusExp (IdnExp (IdnUse ("a")), IntExp (5)))
    }

    // Tests of parsing types

    test ("parsing the int type produces the correct tree") {
        assertParseOk ("int", tipe, IntType ())
    }

    // Tests of binary operator associativity

    test ("+ is left associative") {
        assertParseOk ("a + b + c", exp,
            PlusExp (PlusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("- is left associative") {
        assertParseOk ("a - b - c", exp,
            MinusExp (MinusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("parentheses override associativity in expressions") {
        assertParseOk ("a + (b + c)", exp,
            PlusExp (IdnExp (IdnUse ("a")), PlusExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    // Tests of type operator associativity

    test ("-> is right associative") {
        assertParseOk ("int -> bool -> int", tipe,
            FunType (IntType (), FunType (BoolType (), IntType ())))
    }

    test ("parentheses override associativity in types") {
        assertParseOk ("(int -> bool) -> int", tipe,
            FunType (FunType (IntType (), BoolType ()), IntType ()))
    }

    // Tests of binary operator relative precedence

    test ("application has a lower precedence than + (to left)") {
        assertParseOk ("f x + 3", exp,
            AppExp (IdnExp (IdnUse ("f")), PlusExp (IdnExp (IdnUse ("x")), IntExp (3))))
    }

    test ("application has a lower precedence than - (to left)") {
        assertParseOk ("f x - 3", exp,
            AppExp (IdnExp (IdnUse ("f")), MinusExp (IdnExp (IdnUse ("x")), IntExp (3))))
    }

    test ("+ has lower precedence than * (to left)") {
        assertParseOk ("a + b * c", exp,
            PlusExp (IdnExp (IdnUse ("a")), StarExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("- has lower precedence than * (to left)") {
        assertParseOk ("a - b * c", exp,
            MinusExp (IdnExp (IdnUse ("a")), StarExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("application has lower precedence than + (to right)") {
        assertParseOk ("a + f x", exp,
            AppExp (PlusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("f"))), IdnExp (IdnUse ("x"))))
    }

    test ("application has lower precedence than - (to right)") {
        assertParseOk ("a - f x", exp,
            AppExp (MinusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("f"))), IdnExp (IdnUse ("x"))))
    }

    test ("+ has lower precedence than * (to right)") {
        assertParseOk ("a * b + c", exp,
            PlusExp (StarExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("- has lower precedence than * (to right)") {
        assertParseOk ("a * b - c", exp,
            MinusExp (StarExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("parentheses override precedence (to left)") {
        assertParseOk ("(a + b) * c", exp,
            StarExp (PlusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("parentheses override precedence (to right)") {
        assertParseOk ("a * (b + c)", exp,
            StarExp (IdnExp (IdnUse ("a")), PlusExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    // Tests of parsing programs

    test ("a program with a few expressions produces the correct tree") {
        assertParseOk ("1 + 2;  if true then 3 else 4;  let x = 1 in x + 1;", program,
            Program (List (PlusExp (IntExp (1), IntExp (2)),
                           IfExp (BoolExp (true), IntExp (3), IntExp (4)),
                           LetExp (IdnDef ("x"), IntExp (1),
                                   PlusExp (IdnExp (IdnUse ("x")), IntExp (1))))))
    }

    test ("programs with comments in them work") {
        assertParseOk ("""
            |42; // On a line with code
            |// On a line by itself
            |
            |1 + // In an expression
            |99;
            |
            |     // Over multiple
            | // lines with some code in it:
            | // 4 + 3;
            |""".stripMargin, program,
            Program (List (IntExp (42), PlusExp (IntExp (1), IntExp (99)))))
    }

    test ("factorial program produces the correct tree (file test/factorial.func332)") {
        assertParseOk ("""
            |let fact = (n : int) => if n = 0 then 1 else n * fact (n - 1) in
            |  fact 5;
            |""".stripMargin, program,
            Program (
                List (
                    LetExp (
                        IdnDef ("fact"),
                        FunExp (
                            IdnDef ("n"),
                            IntType (),
                            IfExp (
                                EqualExp (IdnExp (IdnUse ("n")), IntExp (0)),
                                IntExp (1),
                                AppExp (
                                    StarExp (
                                        IdnExp (IdnUse ("n")),
                                        IdnExp (IdnUse ("fact"))),
                                    MinusExp (IdnExp (IdnUse ("n")), IntExp (1))))),
                        AppExp (IdnExp (IdnUse ("fact")), IntExp (5))))))
    }

    /**
     * Assert that a parsing operation should be performed correctly.
     * Try to parse `str` as a `T` using the parser `p`, which is expected
     * to succeed and to produce the given result.  Fail if `p` doesn't
     * produce the given result or if `p` doesn't consume all of the input.
     */
    def assertParseOk[T] (str : String, p : Parser[T], result : T) {
        parseAll (p, str) match {
            case Success (r, in) =>
                if (r != result) fail ("found '" + r + "' not '" + result + "'")
                if (!in.atEnd) fail ("input remaining at " + in.pos)
            case f : Error =>
                fail ("parse error: " + f)
            case f : Failure =>
                fail ("parse failure: " + f)
        }
    }

    /**
     * Assert that a parsing operation should not result in success.
     * Try to parse `str` as a `T` using the parser `p`, which is expected
     * to not succeed, giving either a fatal error or failure (as specified
     * by the `iserr` parameter, which defaults to failure). Fail the test
     * if the parsing operation succeeds. Furthermore, fail the test if it
     * fails, but the error or failure is not indicated at the given `line`
     * and `column` location or doesn't contain the given message `msg`.
     */
    def assertParseError[T] (str : String, p : Parser[T], line : Int, column : Int,
                             msg : String, iserr : Boolean = false) {
        parseAll (p, str) match {
            case Success (r, _) =>
                fail ("expected to find parse error in " + str + " but it succeeded with " + r)
            case e : NoSuccess =>
                if (iserr && e.isInstanceOf[Failure])
                    fail ("got parse failure when expecting parse error")
                else if (!iserr & e.isInstanceOf[Error])
                    fail ("got parse error when expecting parse failure")
                expectResult (msg, "wrong message in error") (e.msg)
                expectResult (line, "wrong line number in error") (e.next.pos.line)
                expectResult (column, "wrong column number in error") (e.next.pos.column)
        }
    }

}

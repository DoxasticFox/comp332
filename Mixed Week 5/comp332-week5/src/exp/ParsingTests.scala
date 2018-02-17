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

    test ("parsing digits as an factor gives the correct tree") {
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

    // Some possible extra tests to make it clearer that things are working.
    // There is no way to exhaustively test, since the number of possible
    // input programs is infinite.  All we can do is try to cover all of
    // the constructs at least once, in some common combinations.

    test ("a complex expression is parsed correctly") {
        assertParseOk ("1 + total / 3 * sum - 5", expression,
            MinusExp (PlusExp (IntExp (1),
                               StarExp (SlashExp (IdnExp("total"), IntExp (3)),
                                        IdnExp ("sum"))),
                      IntExp (5)))
    }

    test ("parsing an identifier factor gives the correct tree") {
        assertParseOk ("blah", factor, IdnExp ("blah"))
    }

    test ("parsing a non-identifier as an identifier gives an error") {
        assertParseError ("1x5", ident,
            "string matching regex `[a-zA-Z]+' expected but `1' found")
    }

    test ("parsing a set statement gives the correct tree") {
        assertParseOk ("set bob = 11", statement,
            SetStmt (IdnExp ("bob"), IntExp (11)))
    }

    test ("parsing an erroneous set statement fails") {
        assertParseError ("set bob 11", statement,
            "`=' expected but `1' found")
    }

    test ("parsing an expression statement gives the correct tree") {
        assertParseOk ("11 - number", statement,
            ExpStmt (MinusExp (IntExp (11), IdnExp ("number"))))
    }

    test ("parsing a program gives the correct tree") {
        assertParseOk ("set a = 3 set b = 4 + a a * a + b", program,
            ExpProgram (List (SetStmt (IdnExp ("a"), IntExp (3)),
                  SetStmt (IdnExp ("b"), PlusExp (IntExp (4), IdnExp ("a"))),
                  ExpStmt (PlusExp (StarExp (IdnExp ("a"), IdnExp ("a")), IdnExp ("b"))))))
    }

    test ("simple expression in conditional and simpl expression statement in branch") {
        assertParseOk ("if (a) { a }", statement,
            IfStmt (IdnExp ("a"), List (ExpStmt (IdnExp ("a")))))
    }

    test ("more complex conditional, multiple statements in branch") {
        assertParseOk ("if (a + b) { set b = 0 b * a }", statement,
            IfStmt (PlusExp (IdnExp ("a"), IdnExp ("b")),
                List (SetStmt (IdnExp ("b"), IntExp (0)),
                      ExpStmt (StarExp (IdnExp ("b"), IdnExp ("a"))))))
    }

    test ("nested conditionals are parsed correctly") {
        assertParseOk ("if (a + 1) { if (b * 2) { set a = b - 3 }}", statement,
            IfStmt (PlusExp (IdnExp ("a"), IntExp (1)),
                List (IfStmt (StarExp (IdnExp ("b"), IntExp (2)),
                          List (SetStmt (IdnExp ("a"), MinusExp (IdnExp ("b"), IntExp (3))))))))
    }

    test ("missing condition is an error") {
        assertParseError ("if () { a }", statement,
            "string matching regex `[a-zA-Z]+' expected but `)' found")
    }

    test ("empty branch is an error") {
        assertParseError ("if (a) { }", statement,
            "string matching regex `[a-zA-Z]+' expected but `}' found")
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

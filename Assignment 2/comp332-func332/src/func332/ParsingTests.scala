/**
 * Func332 parser implementation tests.
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

    test ("parsing an equal expression produces the correct tree") {
        assertParseOk ("a = 1", exp, EqualExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a less than expression produces the correct tree") {
        assertParseOk ("a < 1", exp, LessExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing an addition expression produces the correct tree") {
        assertParseOk ("a + 1", exp, PlusExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a subtraction expression produces the correct tree") {
        assertParseOk ("a - 1", exp, MinusExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a multiplication expression produces the correct tree") {
        assertParseOk ("a * 1", exp, StarExp (IdnExp (IdnUse ("a")), IntExp (1)))
    }

    test ("parsing a division expression produces the correct tree") {
        assertParseOk ("a / 1", exp, SlashExp (IdnExp (IdnUse ("a")), IntExp (1)))
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

    test ("parsing an application expression produces the correct tree") {
        assertParseOk ("a b", exp,
            AppExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))))
    }

    test ("parsing a let expression produces the correct tree") {
        assertParseOk ("let x = 1 in x + 1", exp,
            LetExp (IdnDef ("x"), IntExp (1), PlusExp (IdnExp (IdnUse ("x")), IntExp (1))))
    }

    test ("parsing an if expression produces the correct tree") {
        assertParseOk ("if true then 3 else 4", exp,
            IfExp (BoolExp (true), IntExp (3), IntExp (4)))
    }

    test ("parsing a fun expression produces the correct tree") {
        assertParseOk ("(x : int) => x * 2", exp,
            FunExp (IdnDef ("x"), IntType (), StarExp (IdnExp (IdnUse ("x")), IntExp (2))))
    }

    // Tests of parsing types

    test ("parsing the int type produces the correct tree") {
        assertParseOk ("int", tipe, IntType ())
    }

    test ("parsing the bool type produces the correct tree") {
        assertParseOk ("bool", tipe, BoolType ())
    }

    test ("parsing a function type produces the correct tree") {
        assertParseOk ("bool -> int", tipe, FunType (BoolType (), IntType ()))
    }

    // Tests of binary operator associativity

    test ("let expressions nest properly") {
        assertParseOk ("let x = 1 in let y = 2 in x + y", exp,
            LetExp (IdnDef ("x"), IntExp (1),
                LetExp (IdnDef ("y"), IntExp (2),
                    PlusExp (IdnExp (IdnUse ("x")), IdnExp (IdnUse ("y"))))))

    }

    test ("if expressions nest properly") {
        assertParseOk ("if true then if false then 1 else 2 else if false then 3 else 4", exp,
            IfExp (BoolExp (true),
                IfExp (BoolExp (false), IntExp (1), IntExp (2)),
                IfExp (BoolExp (false), IntExp (3), IntExp (4))))
    }

    test ("function expressions nest properly") {
        assertParseOk ("(x : int) => (y : int) => x + y", exp,
            FunExp (IdnDef ("x"), IntType (),
                FunExp (IdnDef ("y"), IntType (),
                    PlusExp (IdnExp (IdnUse ("x")), IdnExp (IdnUse ("y"))))))
    }

    test ("application is right associative") {
        assertParseOk ("a b c", exp,
            AppExp (IdnExp (IdnUse ("a")), AppExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("parentheses override associativity for applications") {
        assertParseOk ("(a b) c", exp,
            AppExp (AppExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("< is not associative") {
        assertParseError ("a < b < c", exp, 1, 7,
            """string matching regex `\z' expected but `<' found""")
    }

    test ("= is not associative") {
        assertParseError ("a = b = c", exp, 1, 7,
            """string matching regex `\z' expected but `=' found""")
    }

    test ("+ is left associative") {
        assertParseOk ("a + b + c", exp,
            PlusExp (PlusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("- is left associative") {
        assertParseOk ("a - b - c", exp,
            MinusExp (MinusExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("* is left associative") {
        assertParseOk ("a * b * c", exp,
            StarExp (StarExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("/ is left associative") {
        assertParseOk ("a / b / c", exp,
            SlashExp (SlashExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
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

    test ("let has a lower precedence than = (to left)") {
        assertParseOk ("let x = 1 in x = 3", exp,
            LetExp (IdnDef ("x"), IntExp (1),
                EqualExp (IdnExp (IdnUse ("x")), IntExp (3))))
    }

    test ("let has a lower precedence than < (to left)") {
        assertParseOk ("let x = 1 in x < 3", exp,
            LetExp (IdnDef ("x"), IntExp (1),
                LessExp (IdnExp (IdnUse ("x")), IntExp (3))))
    }

    test ("if has lower precedence than = (to left") {
        assertParseOk ("if true then 1 else 2 = 3", exp,
            IfExp (BoolExp (true), IntExp (1), EqualExp (IntExp (2), IntExp (3))))
    }

    test ("if has lower precedence than < (to left") {
        assertParseOk ("if true then 1 else 2 < 3", exp,
            IfExp (BoolExp (true), IntExp (1), LessExp (IntExp (2), IntExp (3))))
    }

    test ("fun has lower precedence than = (to left") {
        assertParseOk ("(x : bool) => 1 = 2", exp,
            FunExp (IdnDef ("x"), BoolType (), EqualExp (IntExp (1), IntExp (2))))
    }

    test ("fun has lower precedence than < (to left") {
        assertParseOk ("(x : bool) => 1 < 2", exp,
            FunExp (IdnDef ("x"), BoolType (), LessExp (IntExp (1), IntExp (2))))
    }

    test ("= has lower precedence than application (to left)") {
        assertParseOk ("a = b c", exp,
            EqualExp (IdnExp (IdnUse ("a")), AppExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("< has lower precedence than application (to left)") {
        assertParseOk ("a < b c", exp,
            LessExp (IdnExp (IdnUse ("a")), AppExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

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

    test ("= has lower precedence than application (to right)") {
        assertParseOk ("a = b c", exp,
            EqualExp (IdnExp (IdnUse ("a")), AppExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
    }

    test ("< has lower precedence than application (to right)") {
        assertParseOk ("a < b c", exp,
            LessExp (IdnExp (IdnUse ("a")), AppExp (IdnExp (IdnUse ("b")), IdnExp (IdnUse ("c")))))
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

    test ("+ has lower precedence than / (to right)") {
        assertParseOk ("a / b + c", exp,
            PlusExp (SlashExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
    }

    test ("- has lower precedence than / (to right)") {
        assertParseOk ("a / b - c", exp,
            MinusExp (SlashExp (IdnExp (IdnUse ("a")), IdnExp (IdnUse ("b"))), IdnExp (IdnUse ("c"))))
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

    test ("a program must have at least one expression") {
        assertParseError ("", program, 1, 1,
            """`(' expected but end of source found""")
    }

    test ("a program with one expression produces the correct tree") {
        assertParseOk ("42;", program, Program (List (IntExp (42))))
    }

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

    test ("a program with many expressions produces the correct tree (file test/tests.func332)") {
        assertParseOk ("""
            |42;
            |
            |3 + 4;
            |
            |2 * (12 / 4);
            |
            |if x < 0 then 1 else 2;
            |
            |if x = y then if true then 3 else 4 else 5;
            |
            |1 + (if x < 10 then 10 else x);
            |
            |((x : int) => x * 2) 4;
            |
            |(let x = 5 in x + x) * (let y = 10 in y / 4);
            |
            |let double = (x : int) => x * 2 in
            |  double 4;
            |
            |let f = (x : int) => x - 1 in
            |  f f 4;
            |
            |(f f) 4;
            |
            |let f = (x : int) => x + 1 in
            |  let g = (h : int -> int) => h 5 in
            |    g f;
            |
            |let f = (p : int -> bool) => p 0 in
            |  let g = (x : int) => x = 0 in
            |    if f g then 9 else 10;
            |
            |let f = (x : int) => (y : int) => x + y in
            |  (f 5) 6;
            """.stripMargin, program,
            Program (
                List (
                    IntExp (42),
                    PlusExp (IntExp (3), IntExp (4)),
                    StarExp (IntExp (2), SlashExp (IntExp (12), IntExp (4))),
                    IfExp (
                        LessExp (IdnExp (IdnUse ("x")), IntExp (0)),
                        IntExp (1),
                        IntExp (2)),
                    IfExp (
                        EqualExp (IdnExp (IdnUse ("x")), IdnExp (IdnUse ("y"))),
                        IfExp (BoolExp (true), IntExp (3), IntExp (4)),
                        IntExp (5)),
                    PlusExp (
                        IntExp (1),
                        IfExp (
                            LessExp (IdnExp (IdnUse ("x")), IntExp (10)),
                            IntExp (10),
                            IdnExp (IdnUse ("x")))),
                    AppExp (
                        FunExp (
                            IdnDef ("x"),
                            IntType (),
                            StarExp (IdnExp (IdnUse ("x")), IntExp (2))),
                        IntExp (4)),
                    StarExp (
                        LetExp (
                            IdnDef ("x"),
                            IntExp (5),
                            PlusExp (IdnExp (IdnUse ("x")), IdnExp (IdnUse ("x")))),
                        LetExp (
                            IdnDef ("y"),
                            IntExp (10),
                            SlashExp (IdnExp (IdnUse ("y")), IntExp (4)))),
                    LetExp (
                        IdnDef ("double"),
                        FunExp (
                            IdnDef ("x"),
                            IntType (),
                            StarExp (IdnExp (IdnUse ("x")), IntExp (2))),
                        AppExp (IdnExp (IdnUse ("double")), IntExp (4))),
                    LetExp (
                        IdnDef ("f"),
                        FunExp (
                            IdnDef ("x"),
                            IntType (),
                            MinusExp (IdnExp (IdnUse ("x")), IntExp (1))),
                        AppExp (
                            IdnExp (IdnUse ("f")),
                            AppExp (IdnExp (IdnUse ("f")), IntExp (4)))),
                    AppExp (
                        AppExp (IdnExp (IdnUse ("f")), IdnExp (IdnUse ("f"))),
                        IntExp (4)),
                    LetExp (
                        IdnDef ("f"),
                        FunExp (
                            IdnDef ("x"),
                            IntType (),
                            PlusExp (IdnExp (IdnUse ("x")), IntExp (1))),
                        LetExp (
                            IdnDef ("g"),
                            FunExp (
                                IdnDef ("h"),
                                FunType (IntType (), IntType ()),
                                AppExp (IdnExp (IdnUse ("h")), IntExp (5))),
                            AppExp (IdnExp (IdnUse ("g")), IdnExp (IdnUse ("f"))))),
                    LetExp (
                        IdnDef ("f"),
                        FunExp (
                            IdnDef ("p"),
                            FunType (IntType (), BoolType ()),
                            AppExp (IdnExp (IdnUse ("p")), IntExp (0))),
                        LetExp (
                            IdnDef ("g"),
                            FunExp (
                                IdnDef ("x"),
                                IntType (),
                                EqualExp (IdnExp (IdnUse ("x")), IntExp (0))),
                            IfExp (
                                AppExp (
                                    IdnExp (IdnUse ("f")),
                                    IdnExp (IdnUse ("g"))),
                                IntExp (9),
                                IntExp (10)))),
                    LetExp (
                        IdnDef ("f"),
                        FunExp (
                            IdnDef ("x"),
                            IntType (),
                            FunExp (
                                IdnDef ("y"),
                                IntType (),
                                PlusExp (
                                    IdnExp (IdnUse ("x")),
                                    IdnExp (IdnUse ("y"))))),
                        AppExp (
                            AppExp (IdnExp (IdnUse ("f")), IntExp (5)),
                            IntExp (6))))))
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

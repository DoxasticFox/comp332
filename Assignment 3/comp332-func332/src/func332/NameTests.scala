/**
 * Func332 language name analysis tests.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the name analysis works correctly.  I.e., we run
 * the checking process over some input and make sure that the expected
 * errors were produced (and only them).
 */
@RunWith(classOf[JUnitRunner])
class NameTests extends SemanticTests {

    import org.kiama.util.Messaging.messagecount

    // Basic binding tests

    test ("a top-level variable is unbound") {
        semanticTest ("""
            |x;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 1, "x is not declared")
    }

    test ("a binding doesn't escape to the next top-level expression") {
        semanticTest ("""
            |let x = 1 in x;
            |x;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 3, 1, "x is not declared")
    }

    test ("a let body variable that is let bound is not an error") {
        semanticTest ("""
            |let x = 1 in x + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("a let body variable that is not bound is an error") {
        semanticTest ("""
            |let x = 1 in y + 1;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 14, "y is not declared")
    }

    test ("a fun body variable that is fun bound is not an error") {
        semanticTest ("""
            |(x : int) => x + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("a fun body variable that is not bound is an error") {
        semanticTest ("""
            |(x : int) => y + 1;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 14, "y is not declared")
    }

    // Nested binding tests (separate fun and let)

    test ("let in let: let body variable that is inner let bound is not an error (diff name)") {
        semanticTest ("""
            |let x = 1 in let y = 2 in y + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("let in let: let body variable that is inner let bound is not an error (same name)") {
        semanticTest ("""
            |let x = true in let x = 2 in x + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("let in let: let body variable that is outer let bound is not an error") {
        semanticTest ("""
            |let x = 1 in let y = 2 in x + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("let in let: unbound variable is an error") {
        semanticTest ("""
            |let x = 1 in let y = 2 in z + 1;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 27, "z is not declared")
    }

    test ("fun in fun: fun body variable that is inner fun bound is not an error (diff name)") {
        semanticTest ("""
            |(x : int) => (y : int) => y + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("fun in fun: fun body variable that is inner fun bound is not an error (same name)") {
        semanticTest ("""
            |(x : bool) => (x : int) => x + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("fun in fun: fun body variable that is outer fun bound is not an error") {
        semanticTest ("""
            |(x : int) => (y : bool) => x + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("fun in fun: unbound variable is an error") {
        semanticTest ("""
            |(x : int) => (y : int) => z + 1;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 27, "z is not declared")
    }

    // Nested binding tests (mixed fun and let)

    test ("fun in let: fun body variable that is let bound is not an error") {
        semanticTest ("""
            |let x = 1 in (y : int) => x + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("fun in let: fun body variable that is fun bound is not an error") {
        semanticTest ("""
            |let x = 1 in (y : int) => y + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("fun in let: fun body variable that is not bound is an error") {
        semanticTest ("""
            |let x = 1 in (y : int) => z + 1;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 27, "z is not declared")
    }

    test ("fun in let: unbound variable is an error") {
        semanticTest ("""
            |let x = 1 in (y : int) => z + 1;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 27, "z is not declared")
    }

    test ("let in fun: let body variable that is let bound is not an error") {
        semanticTest ("""
            |(y : int) => let x = 1 in x + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("let in fun: let body variable that is fun bound is not an error") {
        semanticTest ("""
            |(y : int) => let x = 1 in y + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("let in fun: let body variable that is not bound is an error") {
        semanticTest ("""
            |(y : int) => let x = 1 in z + 1;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 27, "z is not declared")
    }

    test ("let in fun: unbound variable is an error") {
        semanticTest ("""
            |(y : int) => let y = 1 in z + 1;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 27, "z is not declared")
    }

}

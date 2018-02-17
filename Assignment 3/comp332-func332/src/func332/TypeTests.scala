/**
 * Func332 language type analysis tests.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the type analysis works correctly.  I.e., we run
 * the checking process over some input and make sure that the expected
 * errors were produced (and only them).
 */
@RunWith(classOf[JUnitRunner])
class TypeTests extends SemanticTests {

    import org.kiama.util.Messaging.messagecount

    // Basic type tests

    test ("addition of two integers is legal") {
        semanticTest ("""
            |1 + 2;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("addition of a Boolean and a Boolean is not legal") {
        semanticTest ("""
            |true + true;
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 1, "type error: expected int got bool")
        assertMessage (1, 2, 8, "type error: expected int got bool")
    }

    test ("addition of a function and a function is not legal") {
        semanticTest ("""
            |((x : bool) => x) + ((x : int) => x);
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 2, "type error: expected int got bool -> bool")
        assertMessage (1, 2, 22, "type error: expected int got int -> int")
    }

    test ("subtraction of two integers is legal") {
        semanticTest ("""
            |1 - 2;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("subtraction of a Boolean and a Boolean is not legal") {
        semanticTest ("""
            |true - true;
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 1, "type error: expected int got bool")
        assertMessage (1, 2, 8, "type error: expected int got bool")
    }

    test ("subtraction of a function and a function is not legal") {
        semanticTest ("""
            |((x : bool) => x) - ((x : int) => x);
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 2, "type error: expected int got bool -> bool")
        assertMessage (1, 2, 22, "type error: expected int got int -> int")
    }

    test ("multiplication of two integers is legal") {
        semanticTest ("""
            |1 * 2;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("multiplication of a Boolean and a Boolean is not legal") {
        semanticTest ("""
            |true * true;
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 1, "type error: expected int got bool")
        assertMessage (1, 2, 8, "type error: expected int got bool")
    }

    test ("multiplication of a function and a function is not legal") {
        semanticTest ("""
            |((x : bool) => x) * ((x : int) => x);
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 2, "type error: expected int got bool -> bool")
        assertMessage (1, 2, 22, "type error: expected int got int -> int")
    }

    test ("division of two integers is legal") {
        semanticTest ("""
            |1 / 2;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("division of a Boolean and a Boolean is not legal") {
        semanticTest ("""
            |true / true;
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 1, "type error: expected int got bool")
        assertMessage (1, 2, 8, "type error: expected int got bool")
    }

    test ("division of a function and a function is not legal") {
        semanticTest ("""
            |((x : bool) => x) / ((x : int) => x);
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 2, "type error: expected int got bool -> bool")
        assertMessage (1, 2, 22, "type error: expected int got int -> int")
    }

    test ("less-than of two integers is legal") {
        semanticTest ("""
            |1 < 2;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("less-than of a Boolean and a Boolean is not legal") {
        semanticTest ("""
            |true < true;
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 1, "type error: expected int got bool")
        assertMessage (1, 2, 8, "type error: expected int got bool")
    }

    test ("less-than of a function and a function is not legal") {
        semanticTest ("""
            |((x : bool) => x) < ((x : int) => x);
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 2, "type error: expected int got bool -> bool")
        assertMessage (1, 2, 22, "type error: expected int got int -> int")
    }

    test ("equality of two integers is legal") {
        semanticTest ("""
            |1 = 2;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("equality of two Booleans is legal") {
        semanticTest ("""
            |true = false;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("equality of functions is not legal") {
        semanticTest ("""
            |((x : bool) => x) = ((x : bool) => x);
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 1, "type error: expected int or bool got bool -> bool")
    }

    test ("a function can be applied") {
        semanticTest ("""
            |((x : int) => x + 1) 6;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("an integer can't be applied as a function") {
        semanticTest ("""
            |5 6;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 1, "type error: expected UnknownType() -> UnknownType() got int")
    }

    test ("a Boolean can't be applied as a function") {
        semanticTest ("""
            |true 6;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 1, "type error: expected UnknownType() -> UnknownType() got bool")
    }

    test ("an if with Boolean condition and integer branches is legal") {
        semanticTest ("""
            |if 5 = 6 then 42 else 99;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("an if with Boolean condition and Boolean branches is legal") {
        semanticTest ("""
            |if 5 = 6 then true else false;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("an if with Boolean condition and function branches is legal") {
        semanticTest ("""
            |if 5 = 6 then (x : int) => x else (y : int) => y + 1;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("an if can't have an integer condition") {
        semanticTest ("""
            |if 5 then 1 else 2;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 4, "type error: expected bool got int")
    }

    test ("an if can't have a function condition") {
        semanticTest ("""
            |if (x : int) => x then 1 else 2;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 4, "type error: expected bool got int -> int")
    }

    test ("an if can't have branches with different types") {
        semanticTest ("""
            |if 5 = 6 then 1 else true;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 22, "type error: expected int got bool")
    }

    test ("a let bound integer variable can be used as an integer") {
        semanticTest ("""
            |let x = 1 in x + x;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("a let bound Boolean variable can't be used as an integer") {
        semanticTest ("""
            |let x = true in x + x;
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 17, "type error: expected int got bool")
        assertMessage (1, 2, 21, "type error: expected int got bool")
    }

    test ("an integer argument can be used as an integer") {
        semanticTest ("""
            |(x : int) => x + x;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("a Boolean argument can't be used as an integer") {
        semanticTest ("""
            |(x : bool) => x + x;
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 2, 15, "type error: expected int got bool")
        assertMessage (1, 2, 19, "type error: expected int got bool")
    }

}

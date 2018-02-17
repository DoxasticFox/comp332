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

    // FIXME: more tests needed here...
    test ("a let binding is visible inside a function inside that let statement") {
        semanticTest ("""
            |let x = (a : int) => x (a + 1) in
			|	x 4;
            """.stripMargin)
        assert (messagecount === 0)
    }
	
    test ("a variable in a function definition hides outside variables by the same name") {
        semanticTest ("""
            |let x = (x : bool) => (if x then 4 else 2) in
			|	x true;
            |let x = (x : bool) => (if x then 4 else 2) in
			|	x 4;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 5, 4, "type error: expected bool got int")
    }

	test ("a let-bound variable can only be used within its defining expression") {
        semanticTest ("""
            |let x = 1 in
			|	x + 1;
			|x + 2;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 4, 1, "x is not declared")
	}

	test ("nested let expressions hide outer variables of the same name") {
        semanticTest ("""
            |let x = true in
			|	let x = 4 in
			|		x * 2;
			|
            |let x = 4 in
			|	let x = true in
			|		x * 2;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 8, 3, "type error: expected int got bool")
	}
}

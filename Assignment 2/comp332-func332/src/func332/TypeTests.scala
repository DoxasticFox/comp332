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

    test ("an addition of two integers is legal") {
        semanticTest ("""
            |1 + 2;
            """.stripMargin)
        assert (messagecount === 0)
    }

    test ("addition of an integer and a Boolean is not legal") {
        semanticTest ("""
            |1 + true;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 5, "type error: expected int got bool")
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

    // FIXME: more tests needed here...
	test ("applying the greater than operator to integers is legal") {
        semanticTest ("3 < 1;")
        assert (messagecount === 0)
    }
	
	test ("applying the greater than operator to Booleans is illegal") {
        semanticTest ("true < 1; 3 < true; false < true;")
        assert (messagecount === 4)
		assertMessage (0, 1, 1, "type error: expected int got bool")
		assertMessage (1, 1, 15, "type error: expected int got bool")
		assertMessage (2, 1, 21, "type error: expected int got bool")
		assertMessage (3, 1, 29, "type error: expected int got bool")
    }

    test ("multiplication of an integer and a Boolean is illegal") {
        semanticTest ("1*true;")
        assert (messagecount === 1)
        assertMessage (0, 1, 3, "type error: expected int got bool")
    }

    test ("subtraction of an integer and a Boolean is illegal") {
        semanticTest ("1 - true;")
        assert (messagecount === 1)
        assertMessage (0, 1, 5, "type error: expected int got bool")
    }

    test ("division of an integer and a Boolean is illegal") {
        semanticTest ("1 / true;")
        assert (messagecount === 1)
        assertMessage (0, 1, 5, "type error: expected int got bool")
    }
	
	test ("an if statement's conditional expression have a Boolean type") {
        semanticTest ("""
            |if true then true else true;
			|if 1    then true else true;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 3, 4, "type error: expected bool got int")
    }
	
	test ("an if statement's if-body and else-body must have the same type") {
        semanticTest ("""
            |if true then true else true;
			|if true then true else 1;
			|if true then 1    else true;
			|if true then 1    else 1;
            """.stripMargin)
        assert (messagecount === 2)
        assertMessage (0, 3, 24, "type error: expected bool got int")
        assertMessage (1, 4, 24, "type error: expected int got bool")
    }
	
	test ("a function mapping from an integer to a Boolean is legal") {
        semanticTest ("((x : int) => true) 4;")
        assert (messagecount === 0)
    }
	
	test ("a function mapping from an integer can only be applied to an integer") {
        semanticTest ("""
            |((x : int) => x * 2) 4;
			|((x : int) => x * 2) true;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 3, 22, "type error: expected int got bool")
    }
	
	test ("a function mapping from a Boolean can only be applied to an Boolean") {
        semanticTest ("""
            |((x : bool) => x) 4;
			|((x : bool) => x) true;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 2, 19, "type error: expected bool got int")
    }
	
	test ("let expressions have their body's type") {
        semanticTest ("""
            |(let x = true in 4) * 2;
			|(let x = true in x) * 2;
            """.stripMargin)
        assert (messagecount === 1)
        assertMessage (0, 3, 2, "type error: expected int got bool")
    }
}

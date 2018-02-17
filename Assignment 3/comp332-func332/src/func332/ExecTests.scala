/**
 * Func332 language execution tests.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the translation works correctly.
 */
@RunWith(classOf[JUnitRunner])
class ExecTests extends SemanticTests {

    import org.kiama.util.Messaging.messagecount
    import org.kiama.util.StringEmitter

    /**
     * Parse some test input, perform semantic analysis checks, expect no
     * semantic errors. Then translate into SEC machine code and run the code.
     * The value `expected` should be the output that we expect from this
     * run.
     */
    def execTest (str : String, expected : String) {
        val tree = semanticTest (str)
        assert (messagecount === 0)
        val instrs = Translator.translate (tree)
        val emitter = new StringEmitter ()
        val machine = new SECMachine (emitter)
        machine.run (instrs)
        expectResult (expected, "wrong execution output") (emitter.result ())
    }
    
    test ("an integer is printed") {
        execTest ("""
            |42;
            """.stripMargin,
            "42\n")
    }
    
    test ("a boolean is printed") {
        execTest ("""
            |true;
            """.stripMargin,
            "true\n")
    }
    
    test ("an addition expression evaluates to the correct result") {
        execTest ("""
            |3 + 4;
            """.stripMargin,
            "7\n")
    }
    
    test ("a subtraction expression evaluates to the correct result") {
        execTest ("""
            |3 - 4;
            """.stripMargin,
            "-1\n")
    }
    
    test ("a multiplication expression evaluates to the correct result") {
        execTest ("""
            |3 * 4;
            """.stripMargin,
            "12\n")
    }
    
    test ("a division expression evaluates to the correct result") {
        execTest ("""
            |3 / 3;
            """.stripMargin,
            "1\n")
    }
    
    test ("a number is equal to itself") {
        execTest ("""
            |3 = 3;
            """.stripMargin,
            "true\n")
    }
    
    test ("a number is not equal to a different number") {
        execTest ("""
            |2 + 2 = 5;
            """.stripMargin,
            "false\n")
    }
    
    test ("four is less than five") {
        execTest ("""
            |4 < 5;
            """.stripMargin,
            "true\n")
    }
    
    test ("five is not less than four") {
        execTest ("""
            |5 < 4;
            """.stripMargin,
            "false\n")
    }

    test ("a single-level let evaluates to the correct result") {
        execTest ("""
            |let x = 5 in x + 4;
            """.stripMargin,
            "9\n")
    }
    
    test ("nested let expressions evaluate to the correct result") {
        execTest ("""
            |let x = 5 in
            |	let y = 10 in (y / 2) * (x + x);
            """.stripMargin,
            "50\n")
    }
    
    test ("a single-level function evaluates to the correct result") {
        execTest ("""
            |((x : int) => x * 2) 4;
            """.stripMargin,
            "8\n")
    }
    
    test ("chained functions evaluate to the correct result") {
        execTest ("""
            |((x : int) => x * 2) ((x : int) => x*3) 5;
            """.stripMargin,
            "30\n")
    }
    
    test ("a top-level if statment evaluates to the correct result") {
        execTest ("""
            |if 1 < 0 then 0 - 42 else 42;
            """.stripMargin,
            "42\n")
    }
    
    test ("nested if statments evaluate to the correct result") {
        execTest ("""
            |if 1 < 0 then 
            |	if 2 < 3 then 7243 else 442
            |else
            |	if 4 < 7 then 7709 else 724;
            """.stripMargin,
            "7709\n")
    }
    
    test ("a let expression that binds a function expression evaluates to the correct result") {
        execTest ("""
            |let fact = (n : int) => if n = 0 then 1 else n * (fact (n - 1)) in
  			|	fact 5;
            """.stripMargin,
            "120\n")
    }
    
    test ("functions having a fun type evaluate correctly") {
        execTest ("""
            |let f = (x : int) => x + 1 in
  			|	let g = (h : int -> int) => h 5 in
    		|		g f;
            """.stripMargin,
            "6\n")
    }
    
    test ("functions having a bool type evaluate correctly") {
        execTest ("""
            |((x : bool) => if x then 4 else 2) true;
            """.stripMargin,
            "4\n")
    }
}

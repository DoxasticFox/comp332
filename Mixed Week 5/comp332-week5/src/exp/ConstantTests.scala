/**
 * Expression language constant tests.
 *
 * Copyright 2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the constant semantic analysis works correctly.  I.e.,
 * it correctly detects constant and non-constant expressions.  For constant
 * ones, it correctly calculates the value.
 */
@RunWith(classOf[JUnitRunner])
class ConstantTests extends FunSuite {

    import ExpTree._
    import SemanticAnalysis._

    test ("a single integer expression is constant with the correct value") {
        val e = IntExp (10)
        expectResult (true) (e->isconst)
        expectResult (10) (e->expvalue)
    }

    test ("a single identifier is not constant") {
        val e = IdnExp ("total")
        expectResult (false) (e->isconst)
    }

    test ("an expression involving only integers is constant with the correct value") {
        val e = PlusExp (StarExp (IntExp (3), IntExp (4)), IntExp (5))
        expectResult (true) (e->isconst)
        expectResult (17) (e->expvalue)
    }

    // FIXME: more tests needed here...
	test ("an expression involving division by zero is not constant") {
		val e = SlashExp (IntExp (1), IntExp(0))
		expectResult (false) (e->isconst)
	}
	
	test ("an expression involving integers and identifiers is not constant") {
		val e = StarExp (PlusExp (SlashExp (IdnExp ("b"), IntExp(1203)), IdnExp ("a")), IntExp (1))
		expectResult (false) (e->isconst)
	}
	
    test ("a single negative integer expression is constant with the correct value") {
        val e = NegExp (IntExp (1024))
        expectResult (true) (e->isconst)
        expectResult (-1024) (e->expvalue)
    }
}

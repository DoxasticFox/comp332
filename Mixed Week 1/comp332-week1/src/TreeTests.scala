/**
 * Some tests of the binary tree functions.
 *
 * Copyright 2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Some tests of the binary tree functions.  The RunWith etc stuff is there
 * to make it possible for tools such as Eclipse to find these tests.
 */
@RunWith(classOf[JUnitRunner])
class TreeTests extends FunSuite {

    import BinaryTrees._

    /**
     * These tests use the ScalaTest library.  Each test has a title
     * description and a body.  The body uses the expectResult function to
     * run an expression and check that the result is as expected.
     * The general form is expectResult (x) (e) where the expression e is
     * expected to produce the value x.
     */

    test ("a single leaf tree has just one leaf") {
        expectResult (1) (numleaves (Leaf (42)))
    }

    test ("a one-level tree has two leaves") {
        expectResult (2) (numleaves (Fork (Leaf (42), Leaf (99))))
    }

    test ("a more complex tree has the correct number of leaves") {
        val t = Fork (Leaf (1), Fork (Fork (Leaf (2), Leaf (3)), Leaf (4)))
        expectResult (4) (numleaves (t))
    }

    test ("a single leaf tree has just that leaf") {
        expectResult (List (Leaf (42))) (leaves (Leaf (42)))
    }

    test ("a one-level tree has its two leaves") {
        expectResult (List (Leaf (42), Leaf (88))) (
            leaves (Fork (Leaf (42), Leaf (88)))
        )
    }

    test ("a more complex tree has the correct leaves") {
        val t = Fork (Leaf (1), Fork (Fork (Leaf (2), Leaf (3)), Leaf (4)))
        expectResult (List (Leaf (1), Leaf (2), Leaf (3), Leaf (4))) (
            leaves (t)
        )
    }

    test ("the sum of a single leaf tree is the value of the leaf") {
        expectResult (42) (sumleaves (Leaf (42)))
    }

    test ("the sum of a a one-level tree is the sum of the two leaves") {
	    val t = Fork (Leaf (42), Leaf (-26))
        expectResult (16) (sumleaves (t))
    }

    test ("the sum of a mutli-level tree is the sum of its leaves") {
        val t = Fork (Leaf (1), Fork (Fork (Leaf (2), Leaf (3)), Leaf (4)))
        expectResult (10) (sumleaves (t))
    }

    test ("the incandswap of a leaf tree is just the leaf tree incremented") {
        expectResult (Leaf (43)) (incandswap (Leaf (42)))
    }

    test ("the incandswap of a one-level tree is inc and swap of those leaves") {
        val t = Fork (Leaf (42), Leaf (-26))
		expectResult (Fork (Leaf (-25), Leaf (43))) (incandswap (t))
    }

    test ("the incandswap of a mutli-level tree is as expectResulted") {
        val t = Fork (Leaf (1), Fork (Fork (Leaf (2), Leaf (3)), Leaf (4)))
        expectResult (Fork (Fork (Leaf (5), Fork (Leaf (4), Leaf (3))), Leaf(2))) (
            incandswap (t)
        )
    }

}

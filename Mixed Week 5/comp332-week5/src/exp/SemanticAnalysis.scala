/**
 * Semantic analysis for the Expression language.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

object SemanticAnalysis {

    import ExpTree._
    import org.kiama.attribution.Attribution.attr

    /**
     * Is an expression constant?
     */
    val isconst : Expression => Boolean =
        attr {
			case NegExp   (e)    => isconst (e)
			case MinusExp (l, r) => isconst (l) && isconst (r)
			case PlusExp  (l, r) => isconst (l) && isconst (r)
			case StarExp  (l, r) => isconst (l) && isconst (r)
			case SlashExp (l, r)
					if expvalue (r) != 0
					=> isconst (l) && isconst (r)
			case IntExp   (_)    => true
			case _               => false
        }

    /**
     * What is the value of an expression?  Only needs to be valid if the
     * expression is constant (see isconst above).
     */
    val expvalue : Expression => Int =
        attr {
			case NegExp   (e)    => - expvalue (e)
			case MinusExp (l, r) =>   expvalue (l) - expvalue (r)
			case PlusExp  (l, r) =>   expvalue (l) + expvalue (r)
			case StarExp  (l, r) =>   expvalue (l) * expvalue (r)
			case SlashExp (l, r)
					if expvalue (r) != 0
					=> expvalue (l) / expvalue (r)
			case IntExp   (n)    =>   n
			case _               =>   0
        }

}

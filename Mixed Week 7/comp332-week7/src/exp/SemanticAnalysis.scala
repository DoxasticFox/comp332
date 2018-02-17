/**
 * Semantic analysis for the Expression language.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

object SemanticAnalysis {

    import ExpTree._
    import SymbolTable._
    import org.kiama.attribution.Attribution.attr
    import org.kiama.util.Messaging.message

    /**
     * Check the sub-tree rooted at the given node to see if it contains
     * any semantic errors. If yes, as a side-effect this method will
     * record those errors using the Messaging module so that they can be
     * reported to the user later.
     */
    def check (n : ExpNode) {
        // Check this node
        n match {
            case d @ IdnDef (i) if (d->entity == MultipleEntity ()) =>
                message (d, i + " is declared more than once")

            case u @ IdnUse (i) if (u->entity == UnknownEntity ()) =>
                message (u, i + " is not declared")

            case ConstDecl (i, _, e) if (!(e->isconst)) =>
                message (e, "initialising expression is not constant")

            case SetStmt (IdnExp (u @ IdnUse (i)), _) =>
                (u->entity) match {
                    case _ : Constant =>
                        message (u, i + " is not variable, so cannot be set")
                    case _ =>
                        // Ok
                }

            case _ =>
                // Do nothing by default
        }

        // Check the children of this node
        for (child <- n.children)
            check (child.asInstanceOf[ExpNode])
    }

    /**
     * The environment containing all bindings visible at a particular
     * node in the tree, not including any that are defined at that node.
     */
    val envin : ExpNode => Environment =
        attr {

            // If we are at the program node (root of tree) then the
            // environment in is an empty root environment.
            case p : ExpProgram =>
                rootenv ()

            // If we are at a statement that is not the first in a
            // statement sequence then the environment in is the same
            // as the environment out of the previous statement.
            case s : Statement if !s.isFirst =>
                (s.prev[ExpNode])->env

            // Otherwise, the environment in is our parent's environment in.
            case n =>
                (n.parent[ExpNode])->envin

        }

    /**
     * The environment containing all bindings visible "after" a
     * particular node in the tree.  I.e., it's the environment at the
     * node plus any new bindings introduced by the node.
     */
    val env : ExpNode => Environment =
        attr {
            case VarDecl (n @ IdnDef (i), _) =>
                define (n->envin, i, n->entity)
            case ConstDecl (n @ IdnDef (i), _, _) =>
                define (n->envin, i, n->entity)
            case n =>
                n->envin
        }

    /**
     * The program entity referred to by an identifier definition or use.  In
     * the case of a definition it's the thing being defined, so define it to
     * be a reference to a new entity that represents that thing.  If it's
     * already defined, return an entity that indicates a multiple definition.
     * In the case of a use, it's the thing defined elsewhere that is being
     * referred to here, so look it up in the environment, using an unknown
     * entity if the environment does not already contain a binding.
     */
    val entity : IdnNode => Entity =
        attr {
            case n @ IdnDef (i) =>
                if (isDefinedInEnv (n->envin, i))
                    MultipleEntity ()
                else
                    n.parent match {
                        case VarDecl (_, t)      => Variable (t)
                        case ConstDecl (_, t, e) => Constant (t, e)
                        case _                   => UnknownEntity ()
                    }
            case n @ IdnUse (i) =>
                lookup (n->env, i, UnknownEntity ())
        }

    /**
     * Is an expression constant?
     */
    val isconst : Expression => Boolean =
        attr {
            case IdnExp (n) =>
                (n->entity) match {
                    case _ : Variable => false
                    case _            => true
                }
            case IntExp (n)      => true
            case StrExp (_)      => true
            case NegExp (e)      => e->isconst
            case MinusExp (l, r) => (l->isconst) && (r->isconst)
            case PlusExp (l, r)  => (l->isconst) && (r->isconst)
            case StarExp (l, r)  => (l->isconst) && (r->isconst)
            case SlashExp (l, r) => (l->isconst) && (r->isconst)
        }

    /**
     * What is the value of an expression?  Only needs to be valid if the
     * expression is constant (see isconst above).
     */
    val expvalue : Expression => Int =
        attr {
            case IdnExp (n) =>
                (n->entity) match {
                    case Constant (_, e) => e->expvalue
                    case _               => 0 // Hack for variables
                }
            case IntExp (n)      => n
            case StrExp (_)      => 0 // Dummy value
            case NegExp (e)      => -1 * (e->expvalue)
            case MinusExp (l, r) => (l->expvalue) - (r->expvalue)
            case PlusExp (l, r)  => (l->expvalue) + (r->expvalue)
            case StarExp (l, r)  => (l->expvalue) * (r->expvalue)
            case SlashExp (l, r) => (l->expvalue) / (r->expvalue)
        }

    /**
     * What is the type of an expression?
     */
    val tipe : Expression => Type =
        attr {
            case n => UnknownType () // FIXME
        }

    /**
     * What is the expected type of an expression?  I.e., what type does
     * the context impose on it?  Returns UnknownType is any type will do.
     */
    val exptipe : Expression => Type =
        attr {
            case e =>
                (e.parent) match {
                    case p => UnknownType () // FIXME
                }
        }

}

/**
 * Semantic analysis for the Func332 language.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

object SemanticAnalysis {

    import Func332Tree._
    import SymbolTable._
    import org.kiama.attribution.Attribution.attr
    import org.kiama.util.Messaging.message

    /**
     * Check the sub-tree rooted at the given node to see if it contains
     * any semantic errors. If yes, as a side-effect this method will
     * record those errors using the Messaging module so that they can be
     * reported to the user later.
     */
    def check (n : SourceNode) {
        // Check this node
        n match {

            case u @ IdnUse (i) if (u->entity == UnknownEntity ()) =>
                message (u, i + " is not declared")

            case e : Expression =>

                if (!iscompatible (e->tipe, e->exptipe))
                    message (e, "type error: expected " + (e->exptipe) + " got " + (e->tipe))

                // Special case checks
                e match {

                    // Make sure that the LHS of an equality is an int or bool. We are not
                    // able to check this using the above compatibility check since exptipe
                    // can only return one type. Note that the RHS is checked as per usual
                    // since it has to be the same as the LHS's type.
                    case EqualExp (l, _) =>
                        if ((l->tipe != IntType ()) && (l->tipe != BoolType ()))
                            message (e, "type error: expected int or bool got " + (l->tipe))

                    // Check for recursively-defined non-function bindings in lets
                    case LetExp (_, _ : FunExp, _) =>
                        // Do nothing

                    case LetExp (IdnDef (n), exp, body) =>
                        if ((exp->freevars) contains n)
                            message (e, "illegal recursive non-function binding to " + n)

                    case _ =>
                        // No other special case checks

                }


            case _ =>
                // Do nothing by default
        }

        // Check the children of this node
        for (child <- n.children)
            check (child.asInstanceOf[SourceNode])
    }

    /**
     * Are two types compatible? If they are both function types, then
     * the input and output types of the functions must be compatible for
     * the function types to be compatible. Otherwise, if either of them
     * are unknown then we assume an error has already been raised
     * elsewhere so we say they are compatible with anything. Otherwise
     * the two types have to be exactly the same.
     */
    def iscompatible (t1 : Type, t2 : Type) : Boolean =
        t1 match {
            case FunType (in1, out1) =>
                t2 match {
                    case FunType (in2, out2) =>
                        iscompatible (in1, in2) && iscompatible (out1, out2)
                    case _ =>
                        t2 == UnknownType ()
                }
            case _ =>
                (t1 == UnknownType ()) || (t2 == UnknownType ()) || (t1 == t2)
        }

    /**
     * The environment containing all bindings visible at a particular
     * node in the tree, not including any that are defined at that node.
     */
    val envin : SourceNode => Environment =
        attr {

            // If we are at the program node (root of tree) then the
            // environment in is an empty root environment.
            case _ : Program =>
                rootenv ()

            // Special case: identifier defs use the environment in of their
            // parents since we are adding them to their parent's env. We
            // need this to avoid a cycle.
            case n : IdnDef =>
                (n.parent[SourceNode])->envin

            // Otherwise environments are properly nested, so we just get the
            // env of our parent
            case n =>
                (n.parent[SourceNode])->env

        }

    /**
     * The environment containing all bindings visible "after" a
     * particular node in the tree.  I.e., it's the environment at the
     * node plus any new bindings introduced by the node.
     *
     * In each case here we create a new nested environment since each
     * let or fun expression represents a new scope.
     */
    val env : SourceNode => Environment =
        attr {
            case LetExp (n @ IdnDef (i), _, _) =>
                define (enter (n->envin), i, n->entity)
            case FunExp (n @ IdnDef (i), _, _) =>
                define (enter (n->envin), i, n->entity)
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
     * entity if the environment does not already contain a binding. Note:
     * in func332 it's not actually possible to have a multiple definition
     * since each new scope can only introduce one name binding. However,
     * since it's needed for many languages we leave that check in for
     * consistency.
     */
    val entity : IdnNode => Entity =
        attr {
            case n @ IdnDef (i) =>
                n.parent match {
                    case LetExp (_, e, _) => Variable (e)
                    case FunExp (_, t, _) => Argument (t)
                    case _                => UnknownEntity ()
                }
            case n @ IdnUse (i) =>
                lookup (n->envin, i, UnknownEntity ())
        }

    /**
     * What is the type of an expression?
     */
    val tipe : Expression => Type =
        attr {

            // Application, function, if, let expressions

            case AppExp (l, r) =>
                (l->tipe) match {
                    case FunType (in, out) => out
                    case _                 => UnknownType ()
                }

            case FunExp (_, t, body) =>
                FunType (t, body->tipe)

            case IfExp (_, l, r) =>
                if (l->tipe == r->tipe)
                    l->tipe
                else
                    UnknownType ()

            case LetExp (_, _, body) =>
                body->tipe

            // Relational expressions

            case EqualExp (l, r) =>
                BoolType ()

            case LessExp (l, r) =>
                BoolType ()

            // Arithmetic expressions

            case MinusExp (l, r) =>
                IntType ()

            case PlusExp (l, r) =>
                IntType ()

            case SlashExp (l, r) =>
                IntType ()

            case StarExp (l, r) =>
                IntType ()

            // Simple expressions

            case BoolExp (_) =>
                BoolType ()

            case IdnExp (i) =>
                (i->entity) match {
                    case Variable (e) => e->tipe
                    case Argument (t) => t
                    case _            => UnknownType ()
                }

            case IntExp (_) =>
                IntType ()

        }

    /**
     * What is the expected type of an expression?  I.e., what type does
     * the context impose on it?  Returns UnknownType if any type will do.
     */
    val exptipe : Expression => Type =
        attr {
            case e =>
                (e.parent) match {

                    // Application, conditional, function expressions

                    // Applied expression must be a function. We use unknowns for
                    // the input and output type of the function because we don't
                    // care what they actually are.
                    case AppExp (l, r) if e eq l =>
                        FunType (UnknownType (), UnknownType ())

                    // If the applied expression is a function, then the argument
                    // must be of the input type of the function.
                    case AppExp (l, r) if e eq r =>
                        (l->tipe) match {
                            case FunType (in, out) => in
                            case _                 => UnknownType ()
                        }

                    case IfExp (c, _, _) if e eq c =>
                        BoolType ()

                    // Right branch of if has to have same type as left branch
                    case IfExp (_, l, r) if e eq r =>
                        l->tipe

                    // Relational expressions

                    // We say here that we don't care what type the LHS of an
                    // equality is, even though it has to be int or bool. The
                    // reason is that we can only return one type here, but we
                    // want to return two. The actual check is made specially
                    // in the check function above.
                    case EqualExp (l, _) if e eq l =>
                        UnknownType ()

                    // RHS of an equality must have same type as the LHS
                    case EqualExp (l, r) if e eq r =>
                        l->tipe

                    case LessExp (_, _) =>
                        IntType ()

                    // Arithmetic expressions

                    case MinusExp (_, _) =>
                        IntType ()

                    case PlusExp (_, _) =>
                        IntType ()

                    case SlashExp (_, _) =>
                        IntType ()

                    case StarExp (_, _) =>
                        IntType ()

                    // Other: e.g., top-level expression, left branch of if,
                    // body of FunExp, all parts of let. We don't care...

                    case _  =>
                        UnknownType ()

                }
        }

    /**
     * The free variables that occur in an expression. A free variable is
     * one whose meaning is defined outside the expression. E.g., in the
     * expression `(x : int) => x + y`, `x` is not free since it is bound
     * by the function argument, but `y` is free.
     */
    val freevars : Expression => Set[Identifier] =
        attr {
            case AppExp (l, r) =>
                (l->freevars) ++ (r->freevars)

            case FunExp (IdnDef (i), _, body) =>
                (body->freevars) - i

            case IfExp (c, l, r) =>
                (c->freevars) ++ (l->freevars) ++ (r->freevars)

            case LetExp (IdnDef (i), e, body) =>
                (e->freevars) ++ (body->freevars) - i

            // Relational expressions

            case EqualExp (l, r) =>
                (l->freevars) ++ (r->freevars)

            case LessExp (l, r) =>
                (l->freevars) ++ (r->freevars)

            // Arithmetic expressions

            case MinusExp (l, r) =>
                (l->freevars) ++ (r->freevars)

            case PlusExp (l, r) =>
                (l->freevars) ++ (r->freevars)

            case SlashExp (l, r) =>
                (l->freevars) ++ (r->freevars)

            case StarExp (l, r) =>
                (l->freevars) ++ (r->freevars)

            // Simple expressions

            case BoolExp (_) =>
                Set ()

            case IdnExp (IdnUse (i)) =>
                Set (i)

            case IntExp (_) =>
                Set ()
        }

}


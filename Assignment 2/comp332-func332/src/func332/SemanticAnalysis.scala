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

            // FIXME: check things here
			case n : Expression if (!iscompatible (n->exptipe, n->tipe)) =>
				message (n, "type error: expected " + (n->exptipe).toString() + " got " + (n->tipe).toString() + "")
            case u @ IdnUse (i) if (u->entity == UnknownEntity ()) =>
                message (u, i + " is not declared")
			// case u : Expression =>
				// message (u, u + "")
			    // message (u, (u->tipe).toString() + "")
            case _ =>
                // Do nothing by default
        }

        // Check the children of this node
        for (child <- n.children)
            check (child.asInstanceOf[SourceNode])
    }
	
	def iscompatible (t1 : Type, t2 : Type) : Boolean =
        (t1 == UnknownType ()) ||
		(t2 == UnknownType ()) ||
		(t1 == t2)

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

            // FIXME: might need to adjust environments here...
            case n : LetExp =>
                enter ((n.parent[SourceNode])->env)
				
            case n : FunExp =>
                enter ((n.parent[SourceNode])->env)
			
            case n =>
                (n.parent[SourceNode])->env

        }

    /**
     * The environment containing all bindings visible "after" a
     * particular node in the tree.  I.e., it's the environment at the
     * node plus any new bindings introduced by the node.
     */
    val env : SourceNode => Environment =
        attr {

            // FIXME: handling defining occurrences here
			case e @ LetExp (n @ IdnDef (i), _, _) =>
				define (e->envin, i, n->entity)
			case e @ FunExp (n @ IdnDef (i), _, _) =>
				define (e->envin, i, n->entity)
            case n =>
                n->envin
        }

    /**
     * The program entity referred to by an identifier definition or use.
     */
    val entity : IdnNode => Entity =
        attr {
            case n @ IdnDef (i) =>
                // FIXME
				n.parent match {
					case LetExp (_, e, _) => Variable (e)
					case FunExp (_, t, _) => Argument (t)
				}
            case n @ IdnUse (i) =>
                // FIXME
                lookup (n->env, i, UnknownEntity ())

        }

    /**
     * What is the type of an expression?
     */
    val tipe : Expression => Type =
        attr {
            // FIXME
			case BoolExp (_) =>
				BoolType ()
			case IntExp (_) =>
				IntType ()
			case IdnExp (i) =>
				i->entity match {
					case Variable (e) => e->tipe
					case Argument (t) => t
					case _ => UnknownType ()
				}
			case PlusExp (_, _) =>
				IntType ()
			case MinusExp (_, _) =>
				IntType ()
			case StarExp (_, _) =>
				IntType ()
			case SlashExp (_, _) =>
				IntType ()
			case EqualExp (_, _) =>
				BoolType ()
			case LessExp (_, _) =>
				BoolType ()
			case LetExp (_, _, b) =>
				b->tipe
			case IfExp (_, l, r) if (l->tipe == r->tipe) =>
				l->tipe
			case FunExp (_, t, _) =>
				t
			case AppExp (f, _) =>
				f->tipe
            case _ =>
                UnknownType ()
        }
	
    /**
     * What is the expected type of an expression?  I.e., what type does
     * the context impose on it?  Returns UnknownType if any type will do.
     */
    val exptipe : Expression => Type =
        attr {
            case e =>
                (e.parent) match {
                    // FIXME
					case PlusExp  (_, _) =>
						IntType  ()
					case MinusExp (_, _) =>
						IntType  ()
					case StarExp  (_, _) =>
						IntType  ()
					case SlashExp (_, _) =>
						IntType  ()
					case EqualExp (l, _) =>
						l->tipe
					case LessExp  (_, _) =>
						IntType  ()
					case IfExp    (c, l, _) if (c ne e) =>
						l->tipe
					case IfExp    (e, _, _) =>
						BoolType ()
					
					// case AppExp   (e, e1) if (e1 ne e) =>
						// FunType  (UnknownType (), UnknownType ())
					case AppExp   (f, e) =>
						f->tipe
                    case _  =>
                        UnknownType ()

                }
        }

}


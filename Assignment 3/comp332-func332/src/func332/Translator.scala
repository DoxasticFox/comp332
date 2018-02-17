/**
 * Func332 to SEC translator.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

/**
 * Translator from Func332 source programs to SEC target programs.
 */
object Translator {

    import SECTree._
    import Func332Tree._
    import scala.collection.mutable.ListBuffer

    /**
     * Return a frame that represents the SEC instructions for a Func332 program.
     */
    def translate (program : Program) : Frame = {
        var frame = List[Instr] ()
        program match {
        	case Program (es) =>
        		for (e <- es) {
        			frame = frame ++ translate (e) ++ print
        		}
        }
        frame
    }
    
    def translate (exp : Expression) : Frame = {
    	exp match {
    		case LetExp (IdnDef (a), b @ FunExp (IdnDef (c), _, e), f) =>
    			List (IClosure (None, a, translate (f) ++ pop)) ++
    			List (IClosure (Some(a), c, translate (e) ++ pop)) ++
    			call
    		case LetExp (IdnDef (a), b, f) =>
    			List (IClosure (None, a, translate (f) ++ pop)) ++
    			translate (b) ++
    			call
    		case FunExp (IdnDef (c), _, e) =>
    			List (IClosure (None, c, translate (e) ++ pop))
    		case IdnExp (IdnUse (n))	=>
    			List (IVar (n))
    		case AppExp (l, r)  		=>
    			translate (l) ++ translate (r) ++ call
    		case PlusExp (l, r) 		=>
    			translate (l) ++ translate (r) ++ add
    		case MinusExp (l, r) 		=>
    			translate (l) ++ translate (r) ++ sub
    		case StarExp (l, r) 		=>
    			translate (l) ++ translate (r) ++ mul
    		case SlashExp (l, r)		=>
    			translate (l) ++ translate (r) ++ div
    		case IfExp (c, l, r)		=>
    			translate (c) ++ List (IBranch (translate (l), translate (r)))
    		case EqualExp (l, r)		=>
    			translate (l) ++ translate (r) ++ equal
    		case LessExp (l, r)		=>
    			translate (l) ++ translate (r) ++ less
    		case IntExp (i)				=>
    			List (IInt (i))
    		case BoolExp (b)			=>
    			List (IBool (b))
    		case _ 						=>
    			List ()
    	}
    }
    
    
    /**
     * Muh syntactic sugar
     */
    def call : List[Instr] =
    	List (ICall ())
    def print : List[Instr] =
    	List (IPrint ())
    def add : List[Instr] =
    	List (IAdd ())
    def sub : List[Instr] =
    	List (ISub ())
    def mul : List[Instr] =
    	List (IMul ())
    def div : List[Instr] =
    	List (IDiv ())
    def equal : List[Instr] =
    	List (IEqual ())
    def less : List[Instr] =
    	List (ILess ())
    def pop : List[Instr] =
    	List (IPopEnv ())

}

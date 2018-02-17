/**
 * Func332 source program tree definition.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

/**
 * Module containing tree structures for representing Func332 programs.
 */
object Func332Tree {

    import org.kiama.attribution.Attributable
    import org.kiama.util.Positioned

    /**
     * The common supertype of all source tree nodes.
     */
    sealed abstract class SourceNode extends Attributable with Positioned

    /**
     * A Func332 program is a list of expressions. The parser ensures that
     * this list is not empty.
     */
    case class Program (exps : List[Expression]) extends SourceNode

    /**
     * Common superclass of expressions.
     */
    sealed abstract class Expression extends SourceNode

    /**
     * A let expression that binds a name to an expression and a body expression in
     * which that bound name is visible.
     */
    case class LetExp (name : IdnDef, exp : Expression, body : Expression) extends Expression

    /**
     * Function application applies the left expression to the right expression.
     */
    case class AppExp (left : Expression, right : Expression) extends Expression

    /**
     * Equality expression compares the left and right expressions for equality.
     */
    case class EqualExp (left : Expression, right : Expression) extends Expression

    /**
     * Less than expression compares the left and right numeric expressions for less-than order.
     */
    case class LessExp (left : Expression, right : Expression) extends Expression

    /**
     * Addition expression.
     */
    case class PlusExp (left : Expression, right : Expression) extends Expression

    /**
     * Subtraction expression.
     */
    case class MinusExp (left : Expression, right : Expression) extends Expression

    /**
     * Multiplication expression.
     */
    case class StarExp (left : Expression, right : Expression) extends Expression

    /**
     * Integer division expression.
     */
    case class SlashExp (left : Expression, right : Expression) extends Expression

    /**
     * Boolean constant expression.
     */
    case class BoolExp (value : Boolean) extends Expression

    /**
     * Named variable expression.
     */
    case class IdnExp (name : IdnUse) extends Expression

    /**
     * Integer constant expression.
     */
    case class IntExp (value : Int) extends Expression

    /**
     * Conditional expression (if). cond is a Boolean condition. The expression evaluates
     * to the value of left (right) if cond is true (false).
     */
    case class IfExp (cond : Expression, left : Expression, right : Expression) extends Expression

    /**
     * Anonymous function expression. argname and argType give the name and type of the
     * single argument, respectively. The argument name is visible in the body expression.
     */
    case class FunExp (argName: IdnDef, argType : Type, body : Expression) extends Expression

    /**
     * Common superclass of types.
     */
    sealed abstract class Type extends SourceNode

    /**
     * The basic integer type.
     */
    case class IntType () extends Type {
        override def toString () = "int"
    }

    /**
     * The basic Boolean type.
     */
    case class BoolType () extends Type {
        override def toString () = "bool"
    }

    /**
     * A function type from an argument of type `argType` to a result of type
     * `resType`.
     */
    case class FunType (argType : Type, resType : Type) extends Type {
        override def toString () = s"$argType -> $resType"
    }

    /**
     * The type of something whose type we cannot determine.  Compatible
     * with anything.
     */
    case class UnknownType () extends Type

    /**
     * An identifier reference.
     */
    sealed abstract class IdnNode extends SourceNode {
        def idn : String
    }

    /**
     * A defining occurrence of an identifier (i.e., a place where an entity named by
     * the identifier is being introduced).
     */
    case class IdnDef (idn : Identifier) extends IdnNode

    /**
     * An applied occurrence (use) of an identifier (i.e., a place where an entity with
     * this name is being used, but not introduced).
     */
    case class IdnUse (idn : Identifier) extends IdnNode

    /**
     * A representation of identifiers as strings.
     */
    type Identifier = String

}

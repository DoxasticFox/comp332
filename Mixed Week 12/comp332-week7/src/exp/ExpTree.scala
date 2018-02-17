/**
 * Expression language tree definition.
 *
 * Copyright 2009-2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

/**
 * Module containing structures for representing expression language programs.
 */
object ExpTree {

    import org.kiama.attribution.Attributable
    import scala.util.parsing.input.Positional

    /**
     * Interface for all expression tree nodes.
     */
    sealed trait ExpNode extends Attributable with Positional

    /**
     * An expression program consisting of the given statements.
     */
    case class ExpProgram (stmts : List[Statement]) extends ExpNode

    /**
     * Superclass of all statement classes.
     */
    sealed abstract class Statement extends ExpNode

    /**
     * A statement that evaluates an expression and throws away its value.
     */
    case class ExpStmt (exp : Expression) extends Statement

    /**
     * A statement that evaluates an expression and assigns its value to a
     * variable.
     */
    case class SetStmt (v : IdnExp, exp : Expression) extends Statement

    /**
     * A conditional statement that evaluates an expression and, if it is
     * non-zero, executes a sequence of statements.
     */
    case class IfStmt (cond : Expression, stmts : List[Statement]) extends Statement

    /**
     * A statement that declares a new variable with the given name and type.
     */
    case class VarDecl (idndef : IdnDef, tipe : Type) extends Statement

    /**
     * A statement that declares a new constant with the given name and type defined
     * by the given expression.
     */
    case class ConstDecl (idndef : IdnDef, tipe : Type, exp : Expression) extends Statement

    /**
     * Superclass of all types.
     */
    sealed abstract class Type extends ExpNode

    /**
     * The integer type.
     */
    case class IntType () extends Type {
        override def toString = "integer"
    }

    /**
     * The string type.
     */
    case class StrType () extends Type {
        override def toString = "string"
    }

    /**
     * The type of something whose type we cannot determine.  Compatible
     * with anything.
     */
    case class UnknownType () extends Type

    /**
     * Superclass of all expression classes.
     */
    sealed abstract class Expression extends ExpNode

    /**
     * An expression whose value is the current value of a variable.
     */
    case class IdnExp (idnuse : IdnUse) extends Expression

    /**
     * An expression whose value is an integer constant.
     */
    case class IntExp (num : Int) extends Expression

    /**
     * An expression whose value is a string constant.
     */
    case class StrExp (str : String) extends Expression

    /**
     * An expression whose value is the negation of the value of an expression.
     */
    case class NegExp (exp : Expression) extends Expression

    /**
     * An expression whose value is the difference between the values of
     * two expressions.
     */
    case class MinusExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the sum of the values of two expressions.
     */
    case class PlusExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the product of the values of two expressions.
     */
    case class StarExp (left : Expression, right : Expression) extends Expression

    /**
     * An expression whose value is the division of the values of two expressions.
     */
    case class SlashExp (left : Expression, right : Expression) extends Expression

    /**
     * An identifier reference.
     */
    sealed abstract class IdnNode extends ExpNode {
        def idn : String
    }

    /**
     * A defining occurrence of an identifier.
     */
    case class IdnDef (idn : Identifier) extends IdnNode

    /**
     * An applied occurrence (use) of an identifier.
     */
    case class IdnUse (idn : Identifier) extends IdnNode

    /**
     * A representation of identifiers as strings.
     */
    type Identifier = String

}

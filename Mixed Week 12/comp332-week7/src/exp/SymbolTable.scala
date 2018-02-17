/**
 * Symbol tables for the expression language.
 *
 * Copyright 2009-2012, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import org.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating expression language symbol information.
 */
object SymbolTable extends Environments {

    import ExpTree._

    /**
     * A variable entity.
     */
    case class Variable (tipe : Type) extends Entity

    /**
     * A constant entity.  The expression component is the expression
     * that was used to define the constant.
     */
    case class Constant (tipe : Type, exp : Expression) extends Entity

}

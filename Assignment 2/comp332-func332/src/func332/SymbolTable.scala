/**
 * Symbol tables for the Func332 language.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

import org.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating expression language symbol information.
 */
object SymbolTable extends Environments {

    import Func332Tree._

    /**
     * A variable entity bound to the given expression
     */
    case class Variable (exp : Expression) extends Entity

    /**
     * A function argument entity of the given type.
     */
    case class Argument (tipe : Type) extends Entity

}

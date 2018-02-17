/**
 * Expression language parser.
 *
 * Copyright 2009-2011, Anthony Sloane, Macquarie University, All rights reserved.
 */

package exp

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * Module containing parsers for the expression language.
 */
class SyntaxAnalysis extends RegexParsers with PackratParsers {

    import ExpTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[ExpProgram] =
        phrase (program)

    lazy val program : PackratParser[ExpProgram] =
        (statement+) ^^ ExpProgram

    lazy val statement : PackratParser[Statement] =
        positioned (("set" ~> idnuse) ~ ("=" ~> positioned (expression)) ^^
            { case i ~ e => SetStmt (IdnExp (i), e) }) |
        ("if" ~> "(" ~> expression <~ ")") ~ ("{" ~> (statement+) <~ "}") ^^
            { case e ~ ss => IfStmt (e, ss) } |
        positioned (("decl" ~> idndef) ^^
            { case i => VarDecl (i) }) |
        positioned (("const" ~> idndef) ~ ("=" ~> positioned (expression)) ^^
            { case i ~ e => ConstDecl (i, e) }) |
        expression ^^ ExpStmt

    lazy val expression : PackratParser[Expression] =
        expression ~ ("+" ~> term) ^^ { case e ~ t => PlusExp (e, t) } |
        expression ~ ("-" ~> term) ^^ { case e ~ t => MinusExp (e, t) } |
        term

    lazy val term : PackratParser[Expression] =
        term ~ ("*" ~> factor) ^^ { case t ~ f => StarExp (t, f) } |
        term ~ ("/" ~> factor) ^^ { case t ~ f => SlashExp (t, f) } |
        factor

    lazy val factor : PackratParser[Expression] =
        integer ^^ (s => IntExp (s.toInt)) |
        idnuse ^^ IdnExp

    lazy val integer : PackratParser[String] =
        regex ("[0-9]+".r)

    lazy val idndef : PackratParser[IdnDef] =
        positioned (ident ^^ IdnDef)

    lazy val idnuse : PackratParser[IdnUse] =
        positioned (ident ^^ IdnUse)

    lazy val ident : PackratParser[String] =
        regex ("[a-zA-Z]+".r)

}

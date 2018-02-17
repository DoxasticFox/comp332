/**
 * Func332 implementation parser.
 *
 * Copyright 2013, Anthony Sloane, Macquarie University, All rights reserved.
 */

package func332

import org.kiama.util.PositionedParserUtilities

/**
 * Module containing parsers for MiniJava.
 */
class SyntaxAnalysis extends PositionedParserUtilities {

    import Func332Tree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        ((exp <~ ";")+) ^^ Program

    lazy val exp : PackratParser[Expression] =
        ("let" ~> idndef) ~ ("=" ~> exp) ~ ("in" ~> exp) ^^ {
            case n ~ e ~ t => LetExp (n, e, t)
        } |
        ("if" ~> exp) ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ {
            case cond ~ left ~ right =>
                IfExp (cond, left, right)
        } |
        ("(" ~> idndef) ~ (":" ~> tipe <~ ")") ~ ("=>" ~> exp) ^^ {
            case argName ~ argType ~ body => FunExp (argName, argType, body)
        } |
        rel

    lazy val rel : PackratParser[Expression] =
        app ~ ("=" ~> app) ^^ { case l ~ r => EqualExp (l, r) } |
        app ~ ("<" ~> app) ^^ { case l ~ r => LessExp (l, r) } |
        app

    lazy val app : PackratParser[Expression] =
        arith ~ app ^^ { case l ~ r => AppExp (l, r) } |
        arith

    lazy val arith : PackratParser[Expression] =
        arith ~ ("+" ~> star) ^^ { case l ~ r => PlusExp (l, r) } |
        arith ~ ("-" ~> star) ^^ { case l ~ r => MinusExp (l, r) } |
        star

    lazy val star : PackratParser[Expression] =
        star ~ ("*" ~> factor) ^^ { case l ~ r => StarExp (l, r) } |
        star ~ ("/" ~> factor) ^^ { case l ~ r => SlashExp (l, r) } |
        factor

    lazy val factor : PackratParser[Expression] =
        "true" ^^ (_ => BoolExp (true)) |
        "false" ^^ (_ => BoolExp (false)) |
        idnuse ^^ IdnExp |
        integer ^^ (s => IntExp (s.toInt)) |
        "(" ~> exp <~ ")"

    lazy val tipe : PackratParser[Type] =
        basicType ~ ("->" ~> tipe) ^^ {
            case argType ~ resType => FunType (argType, resType)
        } |
        basicType

    lazy val basicType : PackratParser[Type] =
        "int" ^^ (_ => IntType ()) |
        "bool" ^^ (_ => BoolType ()) |
        "(" ~> tipe <~ ")"

    lazy val integer : PackratParser[String] =
        regex ("[0-9]+".r)

    lazy val idndef : PackratParser[IdnDef] =
        identifier ^^ IdnDef

    lazy val idnuse : PackratParser[IdnUse] =
        identifier ^^ IdnUse

    lazy val identifier : PackratParser[String] =
        not (keyword) ~> "[a-zA-Z][a-zA-Z0-9_]*".r

    lazy val keyword =
        "(else|false|if|in|let|then|true)[^a-zA-Z0-9_]".r

    override val whiteSpace =
        """(\s|(//.*\n))+""".r

}

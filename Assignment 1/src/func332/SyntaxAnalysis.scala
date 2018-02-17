/**
 * MiniJava implementation parser.
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
        ("let" ~> idndef) ~ ("=" ~> exp) ~ ("in" ~> exp) ^^
			{ case a ~ b ~ c => LetExp (a, b, c) } |
		("if" ~> exp) ~ ("then" ~> exp) ~ ("else" ~> exp) ^^
			{ case a ~ b ~ c => IfExp (a, b, c) } |
		("(" ~> idndef) ~ (":" ~> tipe) ~ ( ")" ~> "=>" ~> exp) ^^
			{ case a ~ b ~ c => FunExp (a, b, c) } |
		exp0
		
	
	lazy val exp0 : PackratParser[Expression] =
        (exp0 <~ "=") ~ exp1 ^^ { case a ~ b => EqualExp (a, b) } | 
		(exp0 <~ "<") ~ exp1 ^^ { case a ~ b => LessExp  (a, b) } |
		exp1
		
	lazy val exp1 : PackratParser[Expression] =
		exp2 ~ exp1 ^^ { case a ~ b => AppExp (a, b) } |
		exp2
		
	lazy val exp2 : PackratParser[Expression] =
	    (exp2 <~ "+") ~ exp3 ^^ { case a ~ b => PlusExp  (a, b) } |
		(exp2 <~ "-") ~ exp3 ^^ { case a ~ b => MinusExp (a, b) } |
		exp3
		
	lazy val exp3 : PackratParser[Expression] =
		(exp3 <~ "*") ~ exp4 ^^ { case a ~ b => StarExp  (a, b) } | 
		(exp3 <~ "/") ~ exp4 ^^ { case a ~ b => SlashExp (a, b) } | 
		exp4

   lazy val exp4 : PackratParser[Expression] = 
        "true"  ^^ (s => BoolExp (s.toBoolean)) |
		"false" ^^ (s => BoolExp (s.toBoolean)) |
		idnuse  ^^ (s => IdnExp  (s)) |
        integer ^^ (s => IntExp  (s.toInt)) |
		("(" ~> exp <~ ")")
	
	lazy val tipe : PackratParser[Type] =
        (tipe0 <~ "->") ~ tipe ^^ { case t1 ~ t2 => FunType (t1, t2) } |
		tipe0
	
	lazy val tipe0 : PackratParser[Type] =
        "int" ^^ (_ => IntType ()) |
		"bool" ^^ (_ => BoolType ()) |
		"(" ~> tipe <~ ")"

    // You shouldn't need to change anything below here

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

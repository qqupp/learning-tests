package learning.minijson.parsing

import java.io.InputStreamReader
import java.util

import learning.minijson.MiniJson
import learning.minijson.MiniJson.Json
import learning.minijson.parsing.Parser.ErrorOr
import Tokens._

import scala.annotation.{switch, tailrec}

object Parser {


  import MiniJson._
  type ErrorOr[T] = Either[String, T]

  def fromString(input: String): ErrorOr[Json] = ???

}


private[this] object Tokens {

  sealed abstract class Token(val lexicalCategory: LexicalCategory, val repr: String)

  // Lexical categories
  type LexicalCategory = Int
  val TEOF = -1
  val TCharSequence = 0
  val TNumeric = 1
  val TNull = 2
  val TTrue = 3
  val TFalse = 4
  val TLBracket = 5
  val TRBracket = 6
  val TComma = 7
  val TColumn = 8
  val TLBrace = 9
  val TRBrace = 10

  def charSequence(s: String) = new Token(TCharSequence, s){}

  def numeric(s: String) = new Token(TNumeric, s){}

  val `null` = new Token(TNull, null){}

  val  `true` = new Token(TTrue, null){}

  val  `false` = new Token(TFalse, null){}

  val  `{` = new Token(TLBrace, null){}

  val  `}` = new Token(TRBrace, null){}

  val  `,` = new Token(TComma, null){}

  val  `:` = new Token(TColumn, null){}

  val  `[` = new Token(TLBracket, null){}

  val  `]` = new Token(TRBracket, null){}

  val  EOF = new Token(TEOF, null){}

}

private[this] class Tokenizer(is: InputStreamReader) {
  def next: Token = ???
  def end: Unit = is.close()
}


/*
  json grammar

  json
   element

  value
     object
     array
     string
     number
     "true"
     "false"
     "null"

  object
      '{' '}'
      '{' members '}'

  members
      member
      member ',' members

  member
      string ':' element

  array
      '[' ']'
      '[' elements ']'

  elements
      element
      element ',' elements

  element
      value

 */


/*
  json grammar no leftRec

  json
   element

  value
     object
     array
     'string'
     'number'
     'true'
     'false'
     'null'


  object
      '{' object01

  object01
      '}'
      members '}'


  members
      member members01

  members01
      'epsilon'
      ',' members

  member
      string ':' element

  array
      '[' array01

  array01
      ']'
      elements ']'

  elements
      value elements01

  elements01
      'epsilon'
      ',' elements

  element
      value

 */



private[this] class Parser(tokenizer: Tokenizer) {

  trait StackSymbol
  case class Terminal(lexicalCategory: LexicalCategory) extends StackSymbol
  case class NonTerminal(s: String) extends StackSymbol


  val stack: util.Deque[StackSymbol] = new util.ArrayDeque()

  def production(terms: StackSymbol *) =
    terms.reverse.foreach(stack.push(_))




  def decidePoduction(symb: String, token: Tokens.Token): Unit = symb match {
    case "init" => token.lexicalCategory match {
      case TTrue => moveHead()
      case TFalse => moveHead()
      case TLBracket =>
        moveHead()
        production(NonTerminal("array"))
    }

    case "array" => token.lexicalCategory match {
      case TRBracket =>
        moveHead()
      case _ =>
        production(NonTerminal("init"), Terminal(TComma))
    }



  }

  def compareLexicalCat(token: Token, lexicalCategory: LexicalCategory): Unit = {
    if (token.lexicalCategory != lexicalCategory)
      throw new Exception(s"Expected terminal symbol ${lexicalCategory} but got ${token.lexicalCategory}")
  }



  private var token: Option[Token] = None
  private def lookahead(): Token = token match {
    case None =>
      val t = tokenizer.next
      token = Some(t)
      t
    case Some(t) => t
  }

  private def moveHead(): Unit = {
    token = None
    lookahead()
  }


  stack.push(NonTerminal("init"))


  while (!stack.isEmpty) {
    val symb = stack.pop()
    symb match {
      case Terminal(terminalSymbol) =>
        compareLexicalCat(lookahead(), terminalSymbol)
        moveHead()
      case NonTerminal(newSymb) =>
        decidePoduction(newSymb, lookahead())
    }

  }


}

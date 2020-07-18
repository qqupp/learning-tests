package learning.minijson

import scala.collection.mutable.ListBuffer

object Parser {

  import MiniJson._
  type ErrorOr[T] = Either[String, T]

  def fromString(input: String): ErrorOr[Json] =
    for {
      tokens <- data.tokenizer(input)
      json <- data.parseJson(tokens)
    } yield
      json

  private[minijson] object data {

    sealed trait Token
    case class CharSequence(s: String) extends Token
    case class Numeric(s: String) extends Token
    case object `null` extends Token
    case object `true` extends Token
    case object `false` extends Token
    case object `{` extends Token
    case object `}` extends Token
    case object `,` extends Token
    case object `:` extends Token
    case object `[` extends Token
    case object `]` extends Token

    final def tokenizer(s: String): ErrorOr[List[Token]] = {

      val input = s.toList
      val tokens = ListBuffer[Token]()

      def insertToken(t: Token): Unit = {
        tokens.+=(t)
      }

      def consumeWithe(i: List[Char]): List[Char] = i match {
        case Nil => Nil
        case c :: cs => if (c.isWhitespace)
                          consumeWithe(cs)
                        else i
      }

      // doesn't allow escaping fixme
      def consumeString(i: List[Char], acc: ListBuffer[Char] = ListBuffer()): List[Char] = i match {
        case Nil => throw new RuntimeException("non closed quote in string")
        case c :: cs =>
          if (c == '"') {
            insertToken(CharSequence(acc.mkString))
            cs
          } else {
            consumeString(cs, acc.+=(c))
          }
      }

      // rewrite this to nfa automata
      def consumeNumeric(i: List[Char], acc: ListBuffer[Char] = ListBuffer()): List[Char] =
        i match {
          case Nil =>
            if (acc.isEmpty)
              Nil
            else {
              insertToken(Numeric(acc.mkString))
              Nil
            }

          case n :: cs  =>
            if (n.isDigit || n == '.' || n == 'e' || n == 'E' || n == '-' || n == '+')
              consumeNumeric(cs, acc.+=(n))
            else {
              insertToken(Numeric(acc.mkString))
              i
            }
        }

      def consume(i: List[Char]): Unit  = {
        i match {
          case Nil => ()
          case ',' :: cs =>  insertToken(`,`); consume(cs)
          case ':' :: cs =>  insertToken(`:`); consume(cs)
          case '{' :: cs =>  insertToken(`{`); consume(cs)
          case '}' :: cs =>  insertToken(`}`); consume(cs)
          case ']' :: cs =>  insertToken(`]`); consume(cs)
          case '[' :: cs =>  insertToken(`[`); consume(cs)
          case 'n' :: 'u' :: 'l' :: 'l' :: cs =>  insertToken(`null`); consume(cs)
          case 't' :: 'r' :: 'u' :: 'e' :: cs =>  insertToken(`true`); consume(cs)
          case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: cs =>  insertToken(`false`); consume(cs)
          case c :: cs =>
            if (c == '"')
              consume(consumeString(cs))
            else if (c == '-' || c.isDigit)
              consume(consumeNumeric(c::cs))
            else if (c.isWhitespace)
              consume(consumeWithe(cs))
            else
              throw new RuntimeException(s"Unrecognized char '$c' intcode: ${c.toInt} for tokenizer")
        }
      }


      try {
        consume(input)
        Right(tokens.toList)
      } catch {
        case e: RuntimeException => Left(e.getMessage)
      }

    }

    final case class State[T](run: List[Token] => (List[Token], ErrorOr[T])) { self =>

      def map[T1](f: T => T1): State[T1] = State { input =>
        val (s1, errorOrT) = run(input)
        (s1, errorOrT.map(f))
      }

      def *>[T1](newState: State[T1]): State[T1] = self.flatMap(_ => newState)

      def <*[T1](newState: State[T1]): State[T] = self.flatMap(r => newState.map(_ => r))

      def flatMap[T1](f: T => State[T1]): State[T1] = State { input =>
        val (s1, errorOrT) = run(input)
        errorOrT.map(f) match {
          case Left(e) => (s1, Left(e))
          case Right(t) => t.run(s1)
        }
      }

      def orElse[TT >: T](fallBack: => State[TT]): State[TT] = State { input =>
        val (s1, errorOrT) = run(input)
        errorOrT match {
          case Left(_) => fallBack.run(input)
          case Right(t) => (s1, Right(t))
        }
      }

    }

    final def pure[T](t: T): State[T] = State(i => (i, Right(t)))


    final def matchBoolean: State[Json] = State {
      case `true` :: ts => (ts, Right(JTrue))
      case `false` :: ts => (ts, Right(JFalse))
      case others => (others, Left("boolean expected"))
    }

    final def matchNull: State[Json] = State {
      case `null` :: ts => (ts, Right(JNull))
      case others => (others, Left("null expected"))
    }

    final def matchString: State[Json] = State {
      case CharSequence(x) :: ts => (ts, Right(JString(x)))
      case others => (others, Left("charseq expected"))
    }

    def convertNumeric(s: String): ErrorOr[Json] = {
      try {
        Right(JInt(s.toInt))
      } catch {
        case e: java.lang.NumberFormatException => Left(e.getMessage)
      }
    }

    final def matchInt: State[Json] = State {
      case Numeric(x) :: ts => (ts, convertNumeric(x))
      case others => (others, Left("numeric expected"))
    }

    // jArr
    final def matchLBracket: State[Unit] = State {
      case `[` :: ts => (ts, Right(()))
      case others => (others, Left("paren expected"))
    }

    final def matchRBracket: State[Unit] = State {
      case `]` :: ts => (ts, Right(()))
      case others => (others, Left("paren expected"))
    }

    final def matchComma: State[Unit] = State {
      case `,` :: ts => (ts, Right(()))
      case others => (others, Left("comma expected"))
    }

    final def matchNonEmptySequenceOf[T](matcher: State[T]): State[List[T]] = {
      (matcher <* matchComma).flatMap(t => matchNonEmptySequenceOf(matcher).map(ts => t :: ts))
        .orElse(matcher.map(List(_)))
    }

    final def matchArray: State[Json] = {
      val emptyArr: State[Json] = matchLBracket *> matchRBracket *> pure(JArray(Nil))

      def arr: State[Json] = (matchLBracket *> matchNonEmptySequenceOf(matchJson) <* matchRBracket).map(JArray(_))

      emptyArr orElse arr
    }

    // jObj
    final def matchLBrace: State[Unit] = State {
      case `{` :: ts => (ts, Right(()))
      case others => (others, Left("paren expected"))
    }

    final def matchRBrace: State[Unit] = State {
      case `}` :: ts => (ts, Right(()))
      case others => (others, Left("paren expected"))
    }

    final def matchColon: State[Unit] = State {
      case `:` :: ts => (ts, Right(()))
      case others => (others, Left("colon expected"))
    }

    final def matchKey: State[String] = State {
      case CharSequence(x) :: ts => (ts, Right(x))
      case others => (others, Left("charseq expected"))
    }

    final def matchPair: State[(String, Json)] =
      for {
        k <- matchKey <* matchColon
        j <- matchJson
      } yield (k, j)

    final def matchObject: State[Json] = {
      val emptyObj: State[Json] = matchLBrace *> matchRBrace *> pure(JObject(Nil))

      def obj: State[Json] = (matchLBrace *> matchNonEmptySequenceOf(matchPair) <* matchRBrace).map(JObject(_))

      emptyObj orElse obj
    }

    final def matchJson: State[Json] =
      matchNull
        .orElse(matchBoolean)
        .orElse(matchInt)
        .orElse(matchString)
        .orElse(matchArray)
        .orElse(matchObject)

    final def parseJson(tokens: List[Token]): ErrorOr[Json] = {
      val (remainingTokens, result) = matchJson.run(tokens)

      if (remainingTokens.isEmpty)
        result
      else
        Left(s"parseJson end with non empty tokens: $remainingTokens")
    }
  }

}

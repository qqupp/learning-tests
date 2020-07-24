package learning.minijson

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object ParserMonadicTailrec {

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

      class ConsumeException(msg: String) extends RuntimeException(msg)

      def consumeWhitespace(i: List[Char]): List[Char] = i match {
        case Nil => Nil
        case c :: cs => if (c.isWhitespace)
                          consumeWhitespace(cs)
                        else i
      }

      // doesn't allow escaping fixme
      def consumeString(i: List[Char], acc: ListBuffer[Char] = ListBuffer()): List[Char] = i match {
        case Nil => throw new ConsumeException("non closed quote in string")
        case c :: cs =>
          if (c == '"') {
            insertToken(CharSequence(acc.mkString))
            cs
          } else {
            consumeString(cs, acc.+=(c))
          }
      }

      def consumeNumeric(i: List[Char]): List[Char] = {
        val buff: ListBuffer[Char] = ListBuffer()

        val acceptingStates = Set(1,2,6,7)
        def isFinal(state: Int) = acceptingStates.contains(state)

        def is1to9(c: Char) = c.>=('1') && c.<=('9')
        def isZero(c: Char) = c == '0'
        def is0to9(c: Char) = is1to9(c) || isZero(c)
        def isExponent(c: Char) = c == 'e' || c == 'E'
        def isSign(c: Char) = c == '+' || c == '-'

        /*
          State Zero Digit19 Minus Point Exp Sign
            0    1      2      3
            1*                       4    5
            2*   2      2            4    5
            3    1      2
            4    6      6
            5    7      7                      8
            6*   6      6                 5
            7*   7      7
            8    7      7
         */
        @tailrec
        def runDfa(input: List[Char], state: Int): List[Char] = (state, input) match {
          case (_, Nil) => if (isFinal(state)) {
                              insertToken(Numeric(buff.mkString))
                              Nil
                           } else
                              throw new ConsumeException(s"Unexpected number format")
          case (0, '-' :: cs)                 => buff.+=('-'); runDfa(cs, 3)
          case (0, '0' :: cs)                 => buff.+=('0'); runDfa(cs, 1)
          case (0,  c :: cs) if is1to9(c)     => buff.+=(c);   runDfa(cs, 2)
          case (1, '.' :: cs)                 => buff.+=('.'); runDfa(cs, 4)
          case (1,  c :: cs) if isExponent(c) => buff.+=(c);   runDfa(cs, 5)
          case (2, '.' :: cs)                 => buff.+=('.'); runDfa(cs, 4)
          case (2,  c :: cs) if is0to9(c)     => buff.+=(c);   runDfa(cs, 2)
          case (2,  c :: cs) if isExponent(c) => buff.+=(c);   runDfa(cs, 5)
          case (3,  c :: cs) if is1to9(c)     => buff.+=(c);   runDfa(cs, 2)
          case (3, '0' :: cs)                 => buff.+=('0'); runDfa(cs, 1)
          case (4,  c :: cs) if is0to9(c)     => buff.+=(c);   runDfa(cs, 6)
          case (5,  c :: cs) if is0to9(c)     => buff.+=(c);   runDfa(cs, 7)
          case (5,  c :: cs) if isSign(c)     => buff.+=(c);   runDfa(cs, 8)
          case (6,  c :: cs) if is0to9(c)     => buff.+=(c);   runDfa(cs, 6)
          case (6,  c :: cs) if isExponent(c) => buff.+=(c);   runDfa(cs, 5)
          case (7,  c :: cs) if is0to9(c)     => buff.+=(c);   runDfa(cs, 7)
          case (8,  c :: cs) if is0to9(c)     => buff.+=(c);   runDfa(cs, 7)
          case (s,  c :: _) => if (isFinal(s)){
                                  insertToken(Numeric(buff.mkString))
                                  input
                                } else
                                   throw new ConsumeException(s"Unexpected number format ${buff.mkString} $c ")
        }

        runDfa(i, 0)
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
              consume(consumeWhitespace(cs))
            else
              throw new ConsumeException(s"Unrecognized char '$c' intcode: ${c.toInt} for tokenizer")
        }
      }

      try {
        consume(input)
        Right(tokens.toList)
      } catch {
        case e: ConsumeException => Left(e.getMessage)
      }

    }

    import scala.util.control.TailCalls._
    final case class State[T](run: List[Token] => TailRec[(List[Token], ErrorOr[T])]) {
      self =>

      def map[T1](f: T => T1): State[T1] = State { input =>
        tailcall {
          run(input).map { case (s1, errorOrT) => (s1, errorOrT.map(f))
          }
        }
      }

      def *>[T1](newState: State[T1]): State[T1] = self.flatMap(_ => newState)

      def <*[T1](newState: State[T1]): State[T] = self.flatMap(r => newState.map(_ => r))

      def flatMap[T1](f: T => State[T1]): State[T1] = State { input =>
        tailcall {
          run(input).flatMap { case (s1, errorOrT) =>
            errorOrT.map(f) match {
              case Left(e) => done((s1, Left(e)))
              case Right(t) => t.run(s1)
            }
          }
        }
      }

      def orElse[TT >: T](fallBack: => State[TT]): State[TT] = State { input =>
        tailcall {
          run(input).flatMap { case (s1, errorOrT) =>
            errorOrT match {
              case Left(_) => fallBack.run(input)
              case Right(t) => done((s1, Right(t)))
            }
          }
        }
      }
    }

    final def pure[T](t: T): State[T] = State(i => done((i, Right(t))))
    final def state[T](pf: Function1[List[Token], (List[Token], ErrorOr[T])]): State[T] = State(pf.andThen(done))


    final def matchBoolean: State[Json] = state {
      case `true` :: ts => (ts, Right(JTrue))
      case `false` :: ts => (ts, Right(JFalse))
      case others => (others, Left("boolean expected"))
    }

    final def matchNull: State[Json] = state {
      case `null` :: ts => (ts, Right(JNull))
      case others => (others, Left("null expected"))
    }

    final def matchString: State[Json] = state {
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

    final def matchInt: State[Json] = state {
      case Numeric(x) :: ts => (ts, convertNumeric(x))
      case others => (others, Left("numeric expected"))
    }

    // jArr
    final def matchLBracket: State[Unit] = state {
      case `[` :: ts => (ts, Right(()))
      case others => (others, Left("paren expected"))
    }

    final def matchRBracket: State[Unit] = state {
      case `]` :: ts => (ts, Right(()))
      case others => (others, Left("paren expected"))
    }

    final def matchComma: State[Unit] = state {
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
    final def matchLBrace: State[Unit] = state {
      case `{` :: ts => (ts, Right(()))
      case others => (others, Left("paren expected"))
    }

    final def matchRBrace: State[Unit] = state {
      case `}` :: ts => (ts, Right(()))
      case others => (others, Left("paren expected"))
    }

    final def matchColon: State[Unit] = state {
      case `:` :: ts => (ts, Right(()))
      case others => (others, Left("colon expected"))
    }

    final def matchKey: State[String] = state {
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
      val (remainingTokens, result) = matchJson.run(tokens).result

      if (remainingTokens.isEmpty)
        result
      else
        Left(s"parseJson end with non empty tokens: $remainingTokens")
    }
  }

}

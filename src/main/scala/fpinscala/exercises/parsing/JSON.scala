package fpinscala.exercises.parsing

import fpinscala.exercises.parsing.SolParser.run

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*

    def token(s: String) = string(s).token

    def array: Parser[JSON] =
      (token("[") *> value.sep(token(",")).map(vs => JArray(vs.toIndexedSeq)) <* token("]")).scope("array")

    def obj: Parser[JSON] =
      (token("{") *> keyVal.sep(token(",")).map(kvs => JObject(kvs.toMap)) <* token("}")).scope("object")

    def literal: Parser[JSON] = (
      token("null").as(JNull) |
        double.map(JNumber(_)) |
        escapedQuoted.map(JString(_)) |
        token("true").as(JBool(true)) |
        token("false").as(JBool(false))
    ).scope("literal")

    def keyVal: Parser[(String, JSON)] =
      (escapedQuoted ** (token(":") *> value)).scope("key-val")

    def value: Parser[JSON] =
      literal | array | obj

    (whitespace *> (obj | array)).root

@main def main =
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  def printResult[E](e: Either[E, JSON]) =
    e.fold(println, println)

  val parser = JSON.jsonParser(SolParser)
  printResult(parser.run(jsonTxt))
  println("-------------")
  printResult(parser.run(malformedJson1))
  println("-------------")
  printResult(parser.run(malformedJson2))

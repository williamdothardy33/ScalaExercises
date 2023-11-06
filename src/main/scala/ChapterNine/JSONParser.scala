package ChapterNine

import ChapterSeven.Par

enum JSON:
    case JNull
    case JObj(obj: Map[String, JSON])
    case JSeq(seq: IndexedSeq[JSON])
    case JString(value: String)
    case JInt(value: Int)
    case JBoolean(value: Boolean)
    case JDouble(value: Double)



object JSON:
    def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] =
        import P.{given, *}
        def parseJString: Parser[JString] = regex(raw"\w+".r).map(s => JString(s))
        def parseJBoolean: Parser[JBoolean] = ("true" | "false").map(b => JBoolean(b.toBoolean))
        def parseJDouble: Parser[JDouble] = regex(raw"([+-]?\d+)?.\d+([Ee]\d+)?".r).map(d => JDouble(d.toDouble))
        def parseJInt: Parser[JInt] = regex(raw"\d".r).map(i => JInt(i.toInt))
        ???



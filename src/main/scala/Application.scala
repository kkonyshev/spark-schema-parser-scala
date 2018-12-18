import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Application extends App {
  {
    val schemaString = """
                         |schema[
                         |  {
                         |     columnName = EXAMPLE_1
                         |     type = string
                         |  },
                         |  {
                         |     columnName = EXAMPLE_2
                         |     type = string
                         |  },
                         |  {
                         |     columnName = EXAMPLE_3
                         |     type = string
                         |  }
                         |]
                       """.stripMargin

    println(ConfigDSL.parseSchema(schemaString))
  }
}

case class Schema(columns: List[Column])
case class Column(columnName: String, columnType: String)

object ConfigDSL extends StandardTokenParsers {
  lexical.delimiters ++= List("[", "]", "{", "}", ",", " ", "=", "\n")
  lexical.reserved ++= List("schema", "type", "columnName")

  def parseSchema(schemaString: String): Schema =
    schema(new lexical.Scanner(schemaString)) match {
      case Success(columns, _) => Schema(columns)
      case Failure(msg, _) => throw new RuntimeException(msg)
      case Error(msg, _) => throw new RuntimeException(msg)
    }

  def schema: Parser[List[Column]] =
    "schema" ~ "[" ~ listOfColumns ~ "]" ^^ { case _ ~ _ ~ recipeList ~ _ => recipeList }

  def columnDefinition: Parser[Column] =
    "{" ~ "columnName" ~ "=" ~ ident ~ "type" ~ "=" ~ ident ~ "}" ^^ {
      case _ ~ _ ~ _ ~ column ~ _ ~ _ ~ columnType ~ _ => Column(column, columnType)
    }

  def listOfColumns: Parser[List[Column]] =
    repsep(columnDefinition, ",") ^^ { stepList: List[Column] => stepList }
}
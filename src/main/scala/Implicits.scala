import java.sql.{CallableStatement, Connection, PreparedStatement, SQLType}

import scala.collection.mutable

object Implicits {

  implicit class RichConnection(val c: Connection) extends AnyVal {
    def update(s: String): Unit = Repository.update(c, s, Seq())

    def query(s: String)(f: Map[String, _] => Unit): Unit = Repository.query(c, s, Seq())(f)

    def call(s: String, in: Map[String, _], out: Map[String, SQLType])(f: Map[String, _] => Unit): Unit =
      Repository.call(c, s, in, out)(f)
  }

  implicit class RichStatement[Statement <: PreparedStatement](val s: Statement) extends AnyVal {
    def setIn(in: Seq[_]): Unit = in.zipWithIndex.foreach {
      case (param: Unit, index: Int) => s.setObject(index, param)
    }

    def execute(f: Map[String, _] => Unit): Unit = {
      if (s.execute()) {
        do {
          val result   = s.getResultSet
          val metaData = result.getMetaData
          val columns  = metaData.getColumnCount
          while (result.next) {
            val map = mutable.Map[String, AnyRef]()
            for (column <- 1 to columns) {
              val name  = metaData.getColumnName(column)
              val value = result.getObject(column)
              map += name -> value
            }
            f(map.toMap)
          }
          result.close()
        } while (s.getMoreResults)
      }
    }
  }
  implicit class RichCallStatement[Statement <: CallableStatement](val s: Statement) extends AnyVal {
    def setOut(out: Map[String, SQLType]): Unit = out foreach {
      case (param, sqlType) => s.registerOutParameter(param, sqlType)
    }

    def setIn(in: Map[String, _]): Unit = in foreach {
      case (param, any) => s.setObject(param, any)
    }
  }

}

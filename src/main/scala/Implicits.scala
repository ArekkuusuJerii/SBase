import java.sql.{CallableStatement, Connection, PreparedStatement, SQLType}

import sequence.Condition

import scala.collection.mutable
import scala.reflect.ClassTag

object Implicits {

  implicit class RichOption[A](val o: Option[A]) extends AnyVal {
    def ->(f: A => Unit)(implicit tag: ClassTag[A]): Unit = if (o.nonEmpty) f(o.get)
  }

  implicit class RichConnection(val c: Connection) extends AnyVal {
    def update(s: String, seq: Seq[AnyRef] = Seq.empty): Unit = Repository.update(c, s, seq)

    def query(s: String, seq: Seq[AnyRef] = Seq.empty)(f: Map[String, AnyRef] => Unit): Unit =
      Repository.query(c, s, seq)(f)

    def call(s: String, in: Map[String, AnyRef] = Map.empty, out: Map[String, Int] = Map.empty)(
        f: Map[String, AnyRef] => Unit
    ): Unit = Repository.call(c, s, in, out)(f)
  }

  implicit class RichStatement[Statement <: PreparedStatement](val s: Statement) extends AnyVal {
    def setIn(in: Seq[AnyRef]): Unit = in.zipWithIndex.foreach {
      case (param, index) => s.setObject(index, param)
    }

    def execute(f: Map[String, AnyRef] => Unit): Unit = {
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
              map update (name, value)
            }
            f(map.toMap)
          }
          result.close()
        } while (s.getMoreResults)
      }
    }
  }

  implicit class RichCallStatement[Statement <: CallableStatement](val s: Statement) extends AnyVal {
    def setOut(out: Map[String, Int]): Unit = out foreach {
      case (param, sqlType) => s.registerOutParameter(param, sqlType)
    }

    def setIn(in: Map[String, AnyRef]): Unit = in foreach {
      case (param, any) => s.setObject(param, any)
    }
  }

  implicit class RichMap(val m: Map[String, AnyRef]) extends AnyVal {

    def <--[A](key: String, or: A)(implicit tag: ClassTag[A]): A = m.get(key) match {
      case Some(any: A) => any
      case _            => or
    }
  }

  implicit class RichStatementString(val s: String) extends AnyVal {
    def ===(check: AnyVal): Condition = ===(s"'$check'")

    def ===(check: String = "?"): Condition = ->("= " + check)

    def <>(check: AnyVal): Condition = <>(s"'$check'")

    def <>(check: String = "?"): Condition = ->("<> " + check)

    def >>(check: AnyVal): Condition = >>(s"'$check'")

    def >>(check: String = "?"): Condition = ->("> " + check)

    def <<(check: AnyVal): Condition = <<(s"'$check'")

    def <<(check: String = "?"): Condition = ->("< " + check)

    def ->(check: String) = Condition(s, check)

    def <<:(check: AnyVal): Condition = <<:(s"'$check'")

    def <<:(check: String = "?"): Condition = ->(">= " + check)

    def :>>(check: AnyVal): Condition = :>>(s"'$check'")

    def :>>(check: String = "?"): Condition = ->("<= " + check)

    def notNull: Condition = ->(" NOT NULL")

    def isNull: Condition = ->(" IS NULL")
  }

}

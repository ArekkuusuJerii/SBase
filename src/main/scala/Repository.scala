import java.sql.{CallableStatement, Connection, SQLType}

import Implicits._

import scala.collection.mutable

object Repository {
  def update(connection: Connection, sequence: String, data: Seq[AnyRef]): Unit = {
    val statement = connection.prepareStatement(sequence)
    statement.setIn(data)
    statement.executeUpdate
    statement.close()
  }

  def query(connection: Connection, sequence: String, data: Seq[AnyRef])(f: Map[String, AnyRef] => Unit): Unit = {
    val statement = connection.prepareStatement(sequence)
    statement.setIn(data)
    statement.execute(f)
    statement.close()
  }

  def call(connection: Connection, sequence: String, in: Map[String, AnyRef], out: Map[String, SQLType])(
      f: Map[String, AnyRef] => Unit
  ): Unit = {
    def collectOutputs(statement: CallableStatement): Unit = {
      val map = mutable.Map[String, AnyRef]()
      out.keySet foreach (param => map += (param -> statement.getObject(param)))
      f(map.toMap)
    }

    val statement = connection.prepareCall(sequence)
    statement.setOut(out)
    statement.setIn(in)
    if (out.isEmpty)
      statement.execute(f)
    else {
      statement.execute()
      collectOutputs(statement)
    }
    statement.close()
  }

}

package connection {

  import java.sql.DriverManager

  case class Info(ip: String, port: String, database: String, user: String = null, password: String = null)

  case class Driver(format: String, info: Info = null) {
    private[connection] def copy(format: String = format, info: Info = info) = new Driver(format, info)

    def withInfo(info: Info): Driver = copy(info = info)

    def open() = Option.apply(
      DriverManager getConnection (String format (format, info.ip, info.port, info.database), info.user, info.password)
    )
  }
  object Driver {
    val MySQL:     Driver = new Driver("jdbc:mysql://%1$s:%3$s/%2$s")
    val SLQServer: Driver = new Driver("jdbc:sqlserver://%1$s:%2$s;databaseName=%3$s")
  }

}

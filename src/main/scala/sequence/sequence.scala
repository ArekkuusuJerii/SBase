package sequence {

  case class Statement(statement: String) {
    type Self = Statement
    val seq          = new StringBuilder
    var hasCondition = false

    def =+(statement: String): Self = {
      seq ++= " " ++= statement
      this
    }

    def ->(args: String*): Self = {
      seq ++= args.mkString(" ", ",", " ")
      this
    }

    def from(from: String*): Self = {
      seq ++= from.mkString(" FROM ", ",", "")
      this
    }

    def join(join: String): Self = {
      seq ++= s" JOIN $join"
      this
    }

    def innerjoin(innerJoin: String): Self = {
      seq ++= s" INNER JOIN $innerJoin"
      this
    }

    def leftjoin(leftJoin: String): Self = {
      seq ++= s" LEFT JOIN $leftJoin"
      this
    }

    def rightjoin(rightJoin: String): Self = {
      seq ++= s" RIGHT JOIN $rightJoin"
      this
    }

    def on(condition: Condition): Self = {
      seq ++= " ON " ++= condition.toString
      this
    }

    def when(condition: Condition): Self = {
      seq ++= " WHERE " ++= condition.toString
      this
    }

    def end(): String = seq.toString()
  }
  object Statement {
    val Select = new Statement("SELECT")
    val Update = new Statement("UPDATE")
    val Delete = new Statement("DELETE")
  }

  case class Condition(value: String, check: String = "") {
    type Self = Condition
    val seq = new StringBuilder(s"$value $check")

    def and(condition: Condition): Self = {
      seq ++= s" AND ${condition.value} ${condition.check}"
      this
    }

    def or(condition: Condition): Self = {
      seq ++= s" OR ${condition.value} ${condition.check}"
      this
    }

    override def toString: String = seq.toString()
  }

}

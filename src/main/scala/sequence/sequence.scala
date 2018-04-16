package sequence {

  case class Statement(statement: String) {
    type Self = Statement
    val seq = new StringBuilder

    def =+(statement: String): Self = {
      seq ++= " " ++= statement
      this
    }

    def ->(args: String*): Self = {
      seq ++= args.mkString(" ", ",", " ")
      this
    }

    def <(from: String*): Self = {
      seq ++= from.mkString(" ", ",", " ")
      this
    }

    def -(join: String): Self = {
      seq ++= s" JOIN $join"
      this
    }

    def --(innerJoin: String): Self = {
      seq ++= s" INNER JOIN $innerJoin"
      this
    }

    def |-(leftJoin: String): Self = {
      seq ++= s" LEFT JOIN $leftJoin"
      this
    }

    def -|(rightJoin: String): Self = {
      seq ++= s" RIGHT JOIN $rightJoin"
      this
    }

    def ?(conditions: List[Condition]): Self = {
      seq ++= " WHERE " ++= conditions.mkString(",")
      this
    }

    def >(): String = seq.toString()
  }
  object Statement {
    val Select = new Statement("SELECT")
    val Update = new Statement("UPDATE")
    val Delete = new Statement("DELETE")
  }

  case class Condition(condition: String, value: String = _, check: String = _) {
    def copy(condition: String = condition, value: String = value, check: String = check) =
      new Condition(condition, value, check)

    def ->(s: String): Condition = copy(value = s)

    def ==(s: String = "?"): Condition = copy(check = "= " + s)

    def >(s: String = "?"): Condition = copy(check = "> " + s)

    def <(s: String = "?"): Condition = copy(check = "< " + s)

    def >=(s: String = "?"): Condition = copy(check = ">= " + s)

    def <=(s: String = "?"): Condition = copy(check = "<= " + s)

    def <>(s: String = "?"): Condition = copy(check = "<> " + s)

    def like(s: String = "?"): Condition = copy(check = "like " + s)

    def notNull: Condition = copy(check = " NOT NULL")

    def isNull: Condition = copy(check = " IS NULL")

    override def toString: String = s"$condition $value $check"
  }
  object Condition {
    val && = new Condition("&&")
    val || = new Condition("||")
  }

}

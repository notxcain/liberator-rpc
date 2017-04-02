package io.aecor.cs

object Protocol {
  sealed trait Request
  object Request {
    case class Add(lhs: Int, rhs: Int) extends Request
  }

  sealed trait Response
  object Response {
    case class IntResult(value: Int) extends Response
  }
}

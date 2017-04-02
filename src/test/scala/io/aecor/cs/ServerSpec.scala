package io.aecor.cs

import cats.Id
import io.aecor.liberator.macros.algebra
import org.scalatest.{FunSuite, Matchers}
import Protocol._

@algebra
trait Adder[F[_]] {
  def add(lhs: Int, rhs: Int): F[Int]
}

object Adder {
  val default: Adder[Id] = new Adder[Id] {
    override def add(lhs: Int, rhs: Int): Id[Int] = lhs + rhs
  }
}

class ServerSpec extends FunSuite with Matchers {

  implicit val addDecoder: Decoder[Id, Request, Adder.AdderOp[_]] = {
    case Request.Add(lhs, rhs) => Adder.AdderOp.Add(lhs, rhs)
  }

  implicit val encode: Encode[Int, Response] = Response.IntResult(_: Int)

  val server = Server[Request, Response, Id].serve(Adder.default)

  test("Server should return result") {
    val result: Response = server(Request.Add(1, 2))
    result shouldBe Response.IntResult(3)
  }
}

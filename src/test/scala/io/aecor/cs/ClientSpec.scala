package io.aecor.cs

import cats.data.Nested
import cats.{Functor, Id, ~>}
import io.aecor.cs.Protocol.Response.IntResult
import io.aecor.cs.Protocol._
import io.aecor.cs.Test.OpHandler
import org.scalatest.{FunSuite, Matchers}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Poly1}
import cats.implicits._
class ClientSpec extends FunSuite with Matchers {
  val inner: Request => Id[Response] = {
    case Request.Add(lhs, rhs) => Response.IntResult(lhs + rhs)
  }

  implicit def encode: Encode[Adder.AdderOp[_], Request] = {
    case Adder.AdderOp.Add(lhs, rhs) => Request.Add(lhs, rhs)
  }

  implicit val decoder: Decoder[Id, Response, Int] =
    new Decoder[Id, Response, Int] {
      override def decode(a: Response): Id[Int] = a match {
        case IntResult(value) => value
      }
    }

  type Hndlr[F[_], In, Out, G[_], Op[_]] = In => F[Out] => Op ~> F

  def convert[In, Out, G[_], Op[_]] = new MkConvert[In, Out, G, Op] {}

  trait MkConvert[In, Out, G[_], Op[_]] {
    def apply[Repr <: Coproduct, F[_]: Functor, GO <: Coproduct](
        f: In => F[Out])(
        implicit gen: Generic.Aux[Op[_], Repr],
        G: Functor[G],
        h: OpHandler[In, Out, F, G, Repr, GO]): Op ~> Nested[F, G, ?] =
      new (Op ~> Nested[F, G, ?]) {
        override def apply[A](fa: Op[A]): Nested[F, G, A] = {
          val handler = h(f)
          Nested(handler(gen.to(fa))).map { cp =>
            Coproduct.unsafeGet(cp).asInstanceOf[A]
          }
        }
      }
  }

  val client =
    Adder.fromFunctionK(convert[Request, Response, Id, Adder.AdderOp](inner))

//  val client = Client[Adder](inner)

  val gen = shapeless.Generic[Adder.AdderOp[_]]

  implicitly[OpHandler[Request, Response, Id, Id, Adder.AdderOp.Add, Int]]

  test("Client should be able to call underlying function") {
    val result: Int = client.add(1, 2).value
    result shouldBe 3
  }
}

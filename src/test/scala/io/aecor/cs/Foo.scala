package io.aecor.cs

import cats.{ Functor, Id, ~> }
import Protocol._
import cats.data.Nested
import io.aecor.cs.Protocol.Response.IntResult
import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil }
import cats.implicits._

object Foo {
  type Handler[In, F[_], Out, G[_], A] = (In => F[Out]) => F[G[A]]

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

  trait EncoderDecoder[Op[_], OpA <: Op[A], In, F[_], Out, G[_], A] {
    def handler: OpA => Handler[In, F, Out, G, A]
  }

  implicit def encoderDecoder[Op[_], OpA <: Op[A], In, F[_], Out, G[_], A](
    implicit aDecode: Decoder[F, Out, A],
    opEncoder: Encode[Op[_], In]
  ) =
    new EncoderDecoder[Op, OpA, In, F, Out, G, A] {
      override def handler: (OpA) => Handler[In, F, Out, G, A] =
        op => f => f(opEncoder(op)).map(aDecode.decode)
    }

  sealed trait HandlersFor[Op[_], A] {
    type Out
    def value: Out
  }
  object HandlersFor {
    type Aux[Op[_], A, Out0] = HandlersFor[Op, A] {
      type Out = Out0
    }
    implicit def handlersForCnil[Op[_]]: Aux[Op, CNil, HNil] = new HandlersFor[Op, CNil] {
      override type Out = HNil
      override def value: HNil = HNil
    }
    implicit def handlersForCCons[Req, F[_], Resp, G[_], Op[_], A, OpA <: Op[A], OpT <: Coproduct, TOut <: HList](
      implicit encoderDecoder: EncoderDecoder[Op, OpA, Req, F, Resp, G, A],
      T: Aux[Op, OpT, TOut]
    ): Aux[Op, OpA :+: OpT, (OpA => Handler[Req, F, Resp, G, A]) :: TOut] =
      new HandlersFor[Op, OpA :+: OpT] {
        override type Out = (OpA => Handler[Req, F, Resp, G, A]) :: TOut
        override def value: ::[(OpA) => Handler[Req, F, Resp, G, A], TOut] =
          encoderDecoder.handler :: T.value
      }
  }

  trait Mk[C[_], In, F[_], Out, G[_]] {
    def apply[Repr, H, O <: Coproduct]()(
      implicit Repr: Generic.Aux[C[_], Repr],
      F: Functor[F],
      G: Functor[G],
      hf: HandlersFor.Aux[C, Repr, H],
      hh: HasHandlers.Aux[C, Handler[In, F, Out, G, ?], H, Repr, O]
    ): C ~> Handler[In, F, Out, G, ?] = new (C ~> Handler[In, F, Out, G, ?]) {
      override def apply[A](fa: C[A]): Handler[In, F, Out, G, A] =
        hh(hf.value, Repr.to(fa))
          .andThen(x => Nested(x).map(x => Coproduct.unsafeGet(x).asInstanceOf[A]).value)
    }
  }

}

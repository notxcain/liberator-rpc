package io.aecor.cs

import cats.data.Nested
import cats.implicits._
import cats.{ Functor, ~> }
import io.aecor.cs.Test.OpHandler
import io.aecor.liberator.Algebra
import shapeless.{ :+:, CNil, Coproduct, Generic, _ }
import cats.implicits._
import shapeless.ops.coproduct.Inject

object Client {

  trait MkClient[I[_[_]]] {
    def apply[In, F[_], Out, Op[_], Decoded[_]](f: In => F[Out])(
      implicit ops: Algebra.Aux[I, Op],
      opHandler: OpHandler[In, Out, F, Decoded, Op, _]
    ): I[λ[x => F[Decoded[x]]]] = ???
//      ops.fromFunctionK[λ[x => F[Decoded[x]]]](decoderK(new (Op ~> ConstT[F, Out, ?]) {
//        override def apply[A](fa: Op[A]): ConstT[F, Out, A] =
//          f(encodeOp(fa))
//      }))
  }

  def apply[I[_[_]]]: MkClient[I] = new MkClient[I] {}

}

object Test {

  sealed trait OpHandler[In, Out, F[_], G[_], OpA, A] {
    def apply(f: In => F[Out]): OpA => F[G[A]]
  }

  object OpHandler {
    implicit def anOpHandler[In, Out, F[_]: Functor, G[_], OpA, Op[_], A](
      implicit unapply: OpA <:< Op[A],
      opEncoder: Encode[Op[_], In],
      aDecode: Decoder[G, Out, A]
    ): OpHandler[In, Out, F, G, OpA, A] =
      new OpHandler[In, Out, F, G, OpA, A] {
        override def apply(f: (In) => F[Out]): OpA => F[G[A]] =
          op => f(opEncoder(op)).map(aDecode.decode)
      }

    implicit def cnilOpHandler[In, Out, F[_], G[_], OpA, Op[_], A]
      : OpHandler[In, Out, F, G, CNil, CNil] =
      new OpHandler[In, Out, F, G, CNil, CNil] {
        override def apply(f: (In) => F[Out]): (CNil) => F[G[CNil]] =
          _.impossible
      }

    implicit def cConsOpHandler[In, Out, F[_]: Functor, G[_]: Functor, H, HOut, T <: Coproduct, TOut <: Coproduct](
      implicit H: OpHandler[In, Out, F, G, H, HOut],
      T: OpHandler[In, Out, F, G, T, TOut]
    ) =
      new OpHandler[In, Out, F, G, H :+: T, HOut :+: TOut] {
        override def apply(f: (In) => F[Out]): (:+:[H, T]) => F[G[:+:[HOut, TOut]]] =
          _.eliminate(
            H(f).andThen(x => Nested(x).map[HOut :+: TOut](Inl(_)).value),
            T(f).andThen(x => Nested(x).map[HOut :+: TOut](Inr(_)).value)
          )
      }
  }
}

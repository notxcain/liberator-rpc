package io.aecor.cs

import cats.data.Nested
import cats.implicits._
import cats.{Functor, Id, ~>}
import io.aecor.liberator.Algebra
import shapeless.{
  :+:,
  ::,
  CNil,
  Coproduct,
  Generic,
  HList,
  HNil,
  Poly1,
  the,
  _
}
import cats.implicits._
object Client {

  trait MkClient[I[_[_]]] {
    def apply[In, F[_], Out, Op[_], Decoded[_]](f: In => F[Out])(
        implicit ops: Algebra.Aux[I, Op],
        encodeOp: Encode[Op[_], In],
        decoderK: DecoderK[In, F, Out, Op, Decoded]
    ): I[λ[x => F[Decoded[x]]]] =
      ops.fromFunctionK[λ[x => F[Decoded[x]]]](
        decoderK(new (Op ~> ConstT[F, Out, ?]) {
          override def apply[A](fa: Op[A]): ConstT[F, Out, A] =
            f(encodeOp(fa))
        }))
  }

  def apply[I[_[_]]]: MkClient[I] = new MkClient[I] {}

}

object Test {
  sealed trait Ops[A]
  case class Add(lhs: Int, rhs: Int) extends Ops[Int]
  case class Concat(lhs: String, rhs: String) extends Ops[String]

  sealed trait OpHandler[In, Out, F[_], G[_], Op, A] {
    def apply(f: In => F[Out]): Op => F[G[A]]
  }

  object OpHandler {
    implicit def anOpHandler[In, Out, F[_]: Functor, G[_], OpA, Op[_], A](
        implicit unapply: OpA <:< Op[A],
        opEncoder: Encode[Op[_], In],
        aDecode: Decoder[G, Out, A]): OpHandler[In, Out, F, G, OpA, A] =
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

    implicit def cConsOpHandler[In,
                                Out,
                                F[_]: Functor,
                                G[_]: Functor,
                                OpA,
                                A,
                                T <: Coproduct,
                                TOut <: Coproduct](
        implicit h: OpHandler[In, Out, F, G, OpA, A],
        t: OpHandler[In, Out, F, G, T, TOut]) =
      new OpHandler[In, Out, F, G, OpA :+: T, A :+: TOut] {
        override def apply(
            f: (In) => F[Out]): (:+:[OpA, T]) => F[G[:+:[A, TOut]]] =
          _.eliminate(
            h(f).andThen(x =>
              Nested(x).map(Inl[A, TOut](_): A :+: TOut).value),
            t(f).andThen(x => Nested(x).map(Inr[A, TOut](_): A :+: TOut).value)
          )
      }
  }

  type OpsR = Add :+: Concat :+: CNil
  type OpHandlers[In, Out, F[_], G[_]] =
    OpHandler[In, Out, F, G, Add, Int] ::
      OpHandler[In, Out, F, G, Concat, String] ::
      HNil

  val op: OpsR = ???
  object handle extends Poly1 {
    implicit val atAdd = at[Add] {
      case Add(l, r) => r + l
    }
    implicit val atConcat = at[Concat] {
      case Concat(l, r) => l ++ r
    }
  }

  new (Ops ~> Id) {
    override def apply[A](fa: Ops[A]): Id[A] =
      Generic[Ops[_]].to(fa).map(handle).asInstanceOf[Id[A]]
  }

  val x: Int :+: String :+: CNil = op.map(handle)

}

trait DecoderK[In, F[_], Out, C[_], Decoded[_]] {
  def apply(cf: C ~> ConstT[F, Out, ?]): C ~> λ[x => F[Decoded[x]]]
}

object DecoderK {
  implicit def generic[C[_],
                       Decoded[_],
                       F[_],
                       Repr <: shapeless.Coproduct,
                       In,
                       Out](
      implicit gen: Generic.Aux[C[_], Repr],
      fam: FAm[C, Decoded, Out],
      F: Functor[F]
  ): DecoderK[In, F, Out, C, Decoded] =
    (cf: C ~> ConstT[F, Out, ?]) =>
      new (C ~> λ[x => F[Decoded[x]]]) {
        override def apply[A](fa: C[A]): F[Decoded[A]] = {
          val decoder: Decoder[Decoded, Out, A] = fam(fa)
          cf(fa).map(x => decoder.decode(x))
        }
    }
}

trait FAm[C[_], F[_], Out] {
  def apply[A](ca: C[A]): Decoder[F, Out, A]
}

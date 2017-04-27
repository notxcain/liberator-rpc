package io.aecor.cs

import cats.{ Functor, Id, ~> }
import cats.implicits._
import io.aecor.cs.HasHandlers.Op.{ Add, Concat }
import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr }

sealed abstract class HasHandlers[C[_], F[_], H, A] {
  type Out
  def apply(h: H, a: A): F[Out]
}

object HasHandlers extends App {
  type Aux[C[_], F[_], H, A, Out0] = HasHandlers[C, F, H, A] {
    type Out = Out0
  }
  implicit def hnilHasHanlders[C[_], F[_]]: Aux[C, F, HNil, CNil, CNil] =
    new HasHandlers[C, F, HNil, CNil] {
      final override type Out = CNil
      final override def apply(h: HNil, a: CNil): F[CNil] = a.impossible
    }

  implicit def hconsHasHandlers[C[_], F[_]: Functor, OpA, A, OpT <: Coproduct, HT <: HList, TOut <: Coproduct](
    implicit T: HasHandlers.Aux[C, F, HT, OpT, TOut],
    ev: OpA <:< C[A]
  ): Aux[C, F, (OpA => F[A]) :: HT, OpA :+: OpT, A :+: TOut] =
    new HasHandlers[C, F, (OpA => F[A]) :: HT, OpA :+: OpT] {
      final override type Out = A :+: TOut
      final override def apply(h: ::[(OpA) => F[A], HT], a: :+:[OpA, OpT]): F[:+:[A, TOut]] =
        a.eliminate(
          opA => h.head(opA).map[A :+: TOut](Inl(_)),
          opT => T(h.tail, opT).map[A :+: TOut](Inr(_))
        )

    }

  final class ToFunctionK[C[_], F[_]] {
    def apply[H, Repr, Out <: Coproduct](handlers: H)(
      implicit F: Functor[F],
      Repr: Generic.Aux[C[_], Repr],
      ev: HasHandlers.Aux[C, F, H, Repr, Out]
    ): C ~> F = new (C ~> F) {
      override def apply[A](ca: C[A]): F[A] =
        ev(handlers, Repr.to(ca)).map(x => Coproduct.unsafeGet(x).asInstanceOf[A])
    }
  }

  def toFunctionK[C[_], F[_]]: ToFunctionK[C, F] = new ToFunctionK[C, F]

  sealed trait Op[A]
  object Op {
    case class Add(a: Int, b: Int) extends Op[Int]
    case class Concat(l: String, r: String) extends Op[String]
  }

  val handlers: (Add => List[Int]) :: (Concat => List[String]) :: HNil = { x: Add =>
    List(x.a + x.b)
  } :: { x: Concat =>
    List(x.l ++ x.r)
  } :: HNil
  val functionK = toFunctionK[Op, List](handlers)
  val result = functionK(Add(1, 1))
  println(result)
}

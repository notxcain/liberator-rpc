package io.aecor.cs
import cats.implicits._
import cats.{Functor, ~>}
import io.aecor.liberator.Algebra
import shapeless.{:+:, CNil, Generic}

trait Decoder[F[_], A, B] { outer =>
  def decode(a: A): F[B]
  def map[C](f: B => C)(implicit F: Functor[F]): Decoder[F, A, C] =
    (a: A) => outer.decode(a).map(f)
}

trait EncoderK[C[_], F[_], Out] {
  def apply(cf: C ~> F): C ~> ConstT[F, Out, ?]
}

object EncoderK {
  def apply[C[_], F[_], Out](
      implicit ev: EncoderK[C, F, Out]): EncoderK[C, F, Out] = ev

  implicit def fromProjectOp[C[_], F[_], Repr, Out](
      implicit gen: Generic.Aux[C[_], Repr],
      handleEncode: HandleEncode[C, F, Repr, Out]
  ): EncoderK[C, F, Out] =
    (cf: C ~> F) =>
      new (C ~> ConstT[F, Out, ?]) {
        override def apply[A](fa: C[A]): ConstT[F, Out, A] =
          handleEncode(cf)(gen.to(fa))
    }
}

trait HandleEncode[C[_], F[_], T, Out] { outer =>
  def apply(cf: C ~> F): T => F[Out]
  def contramap[T1](f: T1 => T): HandleEncode[C, F, T1, Out] =
    (cf: C ~> F) => (x: T1) => outer(cf)(f(x))
}

object HandleEncode {

  implicit def handlerCnil[C[_], F[_], Out]: HandleEncode[C, F, CNil, Out] =
    (cf: ~>[C, F]) => _.impossible

  implicit def handlerCCons[CA, C[_], A, F[_], T <: shapeless.Coproduct, Out](
      implicit unapply: CA <:< C[A],
      encoder: Encode[A, Out],
      F: Functor[F],
      tailComponentEncoder: HandleEncode[C, F, T, Out]
  ): HandleEncode[C, F, CA :+: T, Out] =
    (cf: ~>[C, F]) =>
      _.eliminate(ca => cf(unapply(ca)).map(encoder(_)),
                  tailComponentEncoder(cf))
}

class Server[In, Out, Decoded[_]: Functor] {
  def serve[I[_[_]], F[_], Op[_]](i: I[F])(
      implicit ops: Algebra.Aux[I, Op],
      decoder: Decoder[Decoded, In, Op[_]],
      encoderK: EncoderK[Op, F, Out]
  ): In => Decoded[F[Out]] = {
    val encoder = encoderK(ops.toFunctionK(i))
    decoder.map(op => encoder(op)).decode
  }
}

trait Encode[A, B] {
  def apply(a: A): B
}

object Server {
  def apply[Request, Response, Decoded[_]: Functor]
    : Server[Request, Response, Decoded] =
    new Server[Request, Response, Decoded]
}

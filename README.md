# liberator-rpc

Client and Server using Liberator Algebra


```scala
@algebra
trait Adder[F[_]] {
  def add(lhs: Int, rhs: Int): F[Int]
}

object Adder {
  val default: Adder[Id] = new Adder[Id] {
    override def add(lhs: Int, rhs: Int): Id[Int] = lhs + rhs
  }
}

sealed trait Request
object Request {
  case class Add(lhs: Int, rhs: Int) extends Request
}

sealed trait Response
object Response {
  case class IntResult(value: Int) extends Response
}

implicit val addDecoder: Decoder[Id, Request, Adder.AdderOp[_]] = {
  case Request.Add(lhs, rhs) => Adder.AdderOp.Add(lhs, rhs)
}

implicit val encode: Encode[Int, Response] = Response.IntResult(_: Int)

val server = Server[Request, Response, Id].serve(Adder.default)

val result: Response = server(Request.Add(1, 2))

assert(result == Response.IntResult(3))

```

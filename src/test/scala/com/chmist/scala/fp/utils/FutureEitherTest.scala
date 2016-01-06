package com.chmist.scala.fp.utils

import org.specs2.mutable.Specification
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class FutureEitherTest extends Specification {
  case class MyError(msg:String)

  type Result[A] = FutureEither[MyError, A]

  "FutureEither" should {
    "map id function" in {
      val eventualEither = FutureEither.point(123).map(a => a)
      eventualEither.run must be_==(Right(123)).await
    }
    "map" in {
      val eventualEither = FutureEither.point(123).map(_ * 2)
      eventualEither.run must be_==(Right(246)).await
    }
    "failure map" in {
      val eventualEither = FutureEither.failure[MyError, Int](MyError("error")).map(_ * 2)
      eventualEither.run must be_==(Left(MyError("error"))).await
    }
    "flatMap" in {
      val eventualEither = FutureEither.point[MyError, Int](123).flatMap(v => FutureEither.point(v * 2))
      eventualEither.run must be_==(Right(246)).await
    }
    "forComprehension" in {
      val result = for {
        a <- FutureEither.point[MyError, Int](10)
        b <- FutureEither.point(20)
      } yield a + b
      result.run must be_==(Right(30)).await
    }
    "forComprehension with map" in {
      val result = for {
        a <- FutureEither.point[MyError, Int](10)
        b = a + 10
      } yield b
      result.run must be_==(Right(20)).await
    }
    "forComprehension with exceptions" in pending {
      // TODO peut-on tester une exception ?
      val result = for {
        a <- FutureEither[MyError, Int](Future{throw new IllegalStateException()})
        b <- FutureEither(Future(Right(5)))
      } yield a + b
      result.run must throwA[IllegalStateException].await
    }
    "forComprehension with Futures" in {
      val result = for {
        a <- FutureEither.point[MyError, Int] {10}
        b <- FutureEither(Future(Right(5)))
      } yield a + b
      result.run must be_==(Right(15)).await
    }
    "forComprehension with error before" in {
      val result = for {
        a <- FutureEither.failure[MyError, Int](MyError("error"))
        b <- FutureEither.point{throw new IllegalStateException(); 10}
      } yield a + b
      result.run must be_==(Left(MyError("error"))).await
    }
    "forComprehension with error after" in {
      val result = for {
        a <- FutureEither.point[MyError, Int](10)
        b <- FutureEither.failure[MyError, Int](MyError("error"))
      } yield a + b
      result.run must be_==(Left(MyError("error"))).await
    }
    "fold point" in {
      val result = FutureEither.point[MyError, Int](10).fold(e => None, s => Some(s.toString))
      result must be_==(Some("10")).await
    }
    "fold failure" in {
      val result = FutureEither.failure[MyError, Int](MyError("gosh")).fold(e => None, s => Some(s.toString))
      result must be_==(None).await
    }
  }
}


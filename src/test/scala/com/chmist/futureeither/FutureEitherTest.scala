package com.chmist.futureeither

import org.specs2.mutable.Specification

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.DurationInt

import scala.concurrent.ExecutionContext.Implicits.global

class FutureEitherTest extends Specification {
  case class MyError(msg:String)

  type Result[A] = FutureEither[MyError, A]

  "FutureEither" should {
    "map id function" in {
      val eventualEither = FutureEither.point(123).map(a => a)
      Await.result(eventualEither.run, 10 second) ==== Right(123)
    }
    "map" in {
      val eventualEither = FutureEither.point(123).map(_ * 2)
      Await.result(eventualEither.run, 10 second) ==== Right(246)
    }
    "failure map" in {
      val eventualEither = FutureEither.failure[MyError, Int](MyError("error")).map(_ * 2)
      Await.result(eventualEither.run, 10 second) ==== Left(MyError("error"))
    }
    "flatMap" in {
      val eventualEither = FutureEither.point(123).flatMap(v => FutureEither.point(v * 2))
      Await.result(eventualEither.run, 10 second) ==== Right(246)
    }
    "forComprehension" in {
      val result = for {
        a <- FutureEither.point(10)
        b <- FutureEither.point(20)
      } yield a + b
      Await.result(result.run, 10 second) ==== Right(30)
    }
    "forComprehension with exceptions" in {
      val result = for {
        a <- FutureEither(Future{throw new IllegalStateException(); Right(10)})
        b <- FutureEither(Future(Right(5)))
      } yield a + b
      result.run must throwA[IllegalStateException].await
    }
    "forComprehension with Futures" in {
      val result = for {
        a <- FutureEither.point {10}
        b <- FutureEither(Future(Right(5)))
      } yield a + b
      result.run must beEqualTo(Right(15)).await
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
        a <- FutureEither.point(10)
        b <- FutureEither.failure(MyError("error"))
      } yield a + b
      result.run must be_==(Left(MyError("error"))).await
    }
    "fold" in {
      val result = FutureEither.point(10).fold(e => None, s => Some(s.toString))
      result must be_==(Some("10")).await
    }
  }
}

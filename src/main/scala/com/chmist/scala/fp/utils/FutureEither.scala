package com.chmist.scala.fp.utils

import scala.concurrent.{ExecutionContext, Future}

case class FutureEither[A,+B](inner: Future[Either[A, B]]) {
  def map[C](f: B => C)(implicit ec: ExecutionContext): FutureEither[A,C] = FutureEither(inner.map(_.right.map(f)))

  def flatMap[C](f: B => FutureEither[A,C])(implicit ec: ExecutionContext): FutureEither[A,C] =
    FutureEither(
      inner.flatMap(_.fold(
        error => Future.successful(Left(error)),
        value => f(value).inner
      ))
    )

  def fold[C](e: A => C, s: B => C)(implicit ec: ExecutionContext): Future[C] = inner.map(_.fold(e, s))

  def run : Future[Either[A, B]] = inner
}

object FutureEither {
  def failure[A,B<:Any](a: A): FutureEither[A,B] = FutureEither(Future.successful(Left(a)))

  def point[A<:Any,B](b: B): FutureEither[A,B] = FutureEither(Future.successful(Right(b)))

  def pointFromFuture[A<:Any,B](fb: Future[B])(implicit ec: ExecutionContext): FutureEither[A, B] = FutureEither(fb.map(b => Right(b)))

  def failureFromFuture[A,B<:Any](fa: Future[A])(implicit ec: ExecutionContext): FutureEither[A,B] = FutureEither(fa.map(a => Left(a)))

  def fromEither[A,B](a: Either[A, B]): FutureEither[A,B] = FutureEither(Future.successful(a))
}


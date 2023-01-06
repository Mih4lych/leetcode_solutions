package learning.cats.rvjexercises

import cats.Applicative

object CatsExApplicative extends App {
  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ???
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val func = applicative.map(wa)(a => (b: B) => (a, b))
    ap(func)(wb)
  }
}

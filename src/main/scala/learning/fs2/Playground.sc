import fs2._

val s = Stream.unfold(1)(s => if (s == 5) None else Option((s.toString, s + 1)))

s.take(20).toList

val s1 = Stream(1, 2, 3)
val s3 = Stream(4, 5, 6)
val nats = Stream.iterate(1)(_ + 1)

nats.take(10).fold(0)(_ + _).toList


(for {
  n1 <- s1
  n2 <- s3
  n3 <- nats
} yield n1 + n2 + n3).take(10).toList
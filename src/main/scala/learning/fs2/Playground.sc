import fs2._

val s = Stream.unfold(1)(s => if (s == 5) None else Option((s.toString, s + 1)))

s.take(20).toList
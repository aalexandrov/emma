import org.emmalanguage.api._

import cats.data.State

/*
 * Input Data:      DataBag[S]
 *
 * Key:             K
 * Key Selector:    S => K, injection
 *
 * State Type:      StateBag[S] ~=~ Map[K, Option[S]]
 *
 * Update Data:     DataBag[U]
 */
object Stateful {

  sealed class Value[S]

  sealed class Bag[S: Meta, K: Meta](xs: DataBag[S], k: S => K) {

    type Updater[U, B] = State[Option[S], DataBag[B]]

    val sm = xs.fetch().groupBy(k).mapValues(_.headOption)

    def update[U, R](fu: Group[K, U] => Updater[U, R])(us: DataBag[Group[K, U]]) = {

      val builder = Seq.newBuilder[(K, Option[S])]

      val updates = for {
        Group(k, u) <- us
      } yield {
        val (s, rs) = fu(Group(k, u)).run(sm(k)).value
        builder += k -> s
      }


    }

  }

  object Bag {

    def apply[A: Meta, K: Meta](xs: DataBag[A], k: A => K): Bag[A, K] =
      new Bag(xs, k)
  }

}

val xs = DataBag(Seq(1, 2, 3, 4))

val ss = Stateful.Bag(xs.map(x => (x, x)), x => x)

// ss.updateWith()
package devinsideyou
package functionplayground

object Library {
  object PointFree {
    def andThen[A, B, C](ab: A => B, bc: B => C): A => C =
      a => bc(ab(a))

    def andThenKleisli[A, B, C, F[_]: Monad](
        afb: A => F[B],
        bfc: B => F[C]
      ): A => F[C] =
      a => the[Monad[F]].flatMap(afb(a))(bfc)

    def compose[A, B, C](bc: B => C, ab: A => B): A => C =
      a => bc(ab(a))

    def flip[A, B, C](abc: A => B => C): B => A => C =
      b => a => abc(a)(b)
  }

  trait Functor[F[_]] {
    def map[B, C](fa: F[B])(f: B => C): F[C]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def pure[B](b: B): F[B]
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[B, C](fb: F[B])(bfc: B => F[C]): F[C] =
      flatten(map(fb)(bfc))

    def flatten[B](ffb: F[F[B]]): F[B]
  }

  trait ContravariantFunctor[F[_]] {
    def contramap[B, C](fb: F[B])(f: C => B): F[C]
  }

  type Contra[F[_]] = ContravariantFunctor[F]

  final implicit class SyntaxForAndThen[A, B](private val ab: A => B) {
    @inline final def -->[C](bc: B => C): A => C =
      PointFree.andThen(ab, bc)
  }

  final implicit class SyntaxForAndCompose[B, C](private val bc: B => C) {
    @inline final def <--[A](ab: A => B): A => C =
      PointFree.compose(bc, ab)

    @inline final def after[A](ab: A => B): A => C =
      PointFree.compose(bc, ab)
  }

  final implicit class SyntaxForPipe[A](private val a: A) {
    @inline final def pipe[B](ab: A => B): B = ab(a)

    @inline final def -->[B](ab: A => B): B = ab(a)

    @inline final def tap[U](ab: A => U): A = { ab(a); a }
  }

  final implicit class SyntaxForFlip[A, B, C](private val abc: A => B => C) {
    @inline final def flipped: B => A => C =
      PointFree.flip(abc)
  }

  final implicit def MonadForFunctionsFrom[A]: Monad[A => *] =
    new Monad[From[A]#To] {
      def pure[B](b: B): A => B =
        _ => b // a is ignored

      def map[B, C](fa: A => B)(f: B => C): A => C =
        fa andThen f

      // unnecessary
      override def flatMap[B, C](fb: A => B)(bfc: B => (A => C)): A => C =
        a => bfc(fb(a))(a)

      def flatten[B](ffb: A => (A => B)): A => B =
        a => ffb(a)(a)
    }

  private[this] type From[-A] = {
    type To[+B] = A => B
  }

  private[this] type To[+A] = {
    type From[-B] = B => A
  }

  final implicit def ContraForFunctionsFrom[A]: Contra[* => A] =
    new Contra[To[A]#From] {
      def contramap[B, C](fb: B => A)(f: C => B): C => A =
        fb compose f
    }

  final implicit class SyntaxForMap[F[_]: Functor, B](private val fb: F[B]) {
    @inline final def map[C](f: B => C): F[C] =
      the[Functor[F]].map(fb)(f)

    @inline final def ->>[C](f: B => C): F[C] =
      map(f)
  }

  final implicit class SyntaxForAndThenKleisli[A, B, F[_]: Monad](
      private val afb: A => F[B]
    ) {
    @inline final def >=>[C](bfc: B => F[C]): A => F[C] =
      PointFree.andThenKleisli(afb, bfc)
  }

  final implicit class SyntaxForFlatMap[F[_]: Monad, B](private val fb: F[B]) {
    @inline final def flatMap[C](bfc: B => F[C]): F[C] =
      the[Monad[F]].flatMap(fb)(bfc)

    @inline final def flatten[C](implicit view: F[B] => F[F[C]]): F[C] =
      the[Monad[F]].flatten(view(fb))
  }

  final implicit class SyntaxForPure[A](private val a: A) {
    def pure[F[_]: Applicative]: F[A] =
      the[Applicative[F]].pure(a)
  }

  final implicit class SyntaxForContraMap[F[_]: Contra, B](
      private val fb: F[B]
    ) {
    @inline final def contramap[C](f: C => B): F[C] =
      the[Contra[F]].contramap(fb)(f)

    @inline final def <<-[C](f: C => B): F[C] =
      contramap(f)
  }

  final implicit class SyntaxForContraMapSpecificallyForFunctions[A, B](
      private val fb: B => A
    ) {
    @inline final def contramap[C](f: C => B): C => A =
      the[Contra[* => A]].contramap(fb)(f)

    @inline final def <<-[C](f: C => B): C => A =
      contramap(f)
  }

  object the {
    def apply[A](implicit a: A): A = a
  }
}

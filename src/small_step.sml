structure SmallStep : SMALL_STEP =
struct
  open Evidence EvidenceAbt

  type term = EvidenceAbt.t
  datatype t = STEP of term | CANON
  exception Stuck of term

  infix $ $$ //
  infixr \ \\

  fun stepApBeta (F, A) =
    case out F of
        LAM $ #[B] => B // A
      | _ => raise Stuck (AP $$ #[F, A])

  fun stepDecideBeta (S, L, R) =
    case out S of
        INL $ #[A] => L // A
      | INR $ #[B] => R // B
      | _ => raise Stuck (DECIDE $$ #[S, L, R])

  fun stepFstBeta M =
    case out M of
        PAIR $ #[M, N] => M
      | _ => raise Stuck (FST $$ #[M])

  fun stepSndBeta M =
    case out M of
        PAIR $ #[M, N] => N
      | _ => raise Stuck (SND $$ #[M])

  fun step E =
    case out E of
       AP $ #[M, N] => STEP (stepApBeta (M, N))
     | DECIDE $ #[M, L, R] => STEP (stepDecideBeta (M, L, R))
     | FST $ #[M] => STEP (stepFstBeta M)
     | SND $ #[M] => STEP (stepSndBeta M)
     | LAM $ _ => CANON
     | INL $ _ => CANON
     | INR $ _ => CANON
     | PAIR $ _ => CANON
     | _ => raise Stuck E
end

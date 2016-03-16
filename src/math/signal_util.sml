(*
* file: signal_util.sml
* author: Marius Geitle <marius.geitle@hiof.no
* 
* This file contains utilities for transforming signals.
*)


structure SignalUtil = 
struct

   (*
    * For an input vector x of length N, the discrete Fourier transform is 
    * the length N vector X given by
    *
    *                 N-1
    *    X(k) =       sum x(n)*exp(-i*2*pi*k*n/N), for 0 <= k <= N-1
    *                 n=0
    *
    *)
  fun dft( vector : real Vector.vector ) : Complex.number Vector.vector =
  let
     val expFactor = ( ~2.0*( Math.pi/real( Vector.length vector ) ) )

     fun calcElmt( n : int, k : int ) : Complex.number =
       Complex.exp( 
         Complex.complex(
           0.0, Real.*(Real.*(expFactor, Real.fromInt(k)), Real.fromInt(n))))

     fun calculateDft( k : int ) : Complex.number =
       Vector.foldli 
         ( fn ( n, x, a ) => 
           Complex.plus a (
             Complex.times (Complex.complex(x, 0.0)) (calcElmt(n, k)))) 
         ( Complex.complex( 0.0, 0.0 ) ) 
         vector
  in
    Array.vector( 
      Array.tabulate( Vector.length vector, fn k => calculateDft k ) )
  end

  (*
   *  The inverse discrete Fourier transform is given by
   *
   *                 N-1
   *    x(n) = (1/N) sum X(k)*exp( i*2*pi*k*n/N), for 0 <= n <= N-1
   *                 k=0
   *)
  fun idft( vector: Complex.number Vector.vector) 
      : Complex.number Vector.vector =
  let
    val expFactor = ( 2.0*( Math.pi/Real.fromInt( Vector.length vector ) ) )

    fun calcElmt(n : int, k : int) : Complex.number =
      Complex.exp(
        Complex.complex(
          0.0, 
          Real.*(Real.*(expFactor, Real.fromInt(k)), Real.fromInt(n))));

    fun calculateDft(k : int) : Complex.number =
      Vector.foldli 
        ( fn ( n, x, a ) => 
            Complex.plus a ( Complex.times x ( calcElmt( n, k ) ) ) ) 
        ( Complex.complex( 0.0, 0.0 ) ) 
        vector
  in
    Array.vector(
      Array.tabulate( Vector.length vector, 
        fn k => 
          Complex.divide
            ( calculateDft k )
            ( Complex.complex( Real.fromInt( Vector.length vector ), 0.0 ) ) ) )
  end

  (*
   * Hilbert transform is implemented similar to Matlab which
   * does not actually compute the Hilbert transform directly,
   * but instead computes the analytical signal by first taking the fourier
   * transform and then deleting negative frequencies, applying inverse fourier
   * transform and returning the imaginary part.
   *
   *)
  fun hilbert( vector: real Vector.vector ) : real Vector.vector =
  let
    val lowerPart = Vector.length vector div 2

    fun hilbertify( i : int, x : Complex.number ) : Complex.number =
      if i = 0 then 
        Complex.complex( 1.0, 0.0 )
      else if i > lowerPart then 
        Complex.complex( 0.0, 0.0 )
      else 
        Complex.times x ( Complex.complex( 2.0, 0.0 ) )

    val transformed = dft vector  

    val hilberted = 
      Vector.tabulate( 
        Vector.length transformed, 
        fn i => hilbertify( i, Vector.sub( transformed, i ) ) )

    val inversed = idft hilberted
  in
    Vector.tabulate( Vector.length(vector),
      fn i => Complex.im( Vector.sub( inversed, i ) ) )
  end

end (* structure SignalUtil *)

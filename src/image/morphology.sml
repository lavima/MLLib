(*
* file: morphology.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with common morphological algorithms.
*)

structure Morphology =
struct

  fun thin( Image as { Width, Height, ... }: BooleanImage.image ) 
      : BooleanImage.image =
  let
    val T = SOME true
    val F = SOME false
    val TF = NONE

    val M1 = Array2D.fromList( 3, 3, [ F, F, F, TF, T, TF, T, T, T ] )
    val M2 = Array2D.fromList( 3, 3, [ TF, F, F, T, T, F, T, T, TF ] )
    val M3 = Array2D.fromList( 3, 3, [ T, TF, F, T, T, F, T, TF, F ] )
    val M4 = Array2D.fromList( 3, 3, [ T, T, TF, T, T, F, TF, F, F ] )
    val M5 = Array2D.fromList( 3, 3, [ T, T, T, TF, T, TF, F, F, F ] )
    val M6 = Array2D.fromList( 3, 3, [ TF, T, T, F, T, T, F, F, TF ] )
    val M7 = Array2D.fromList( 3, 3, [ F, TF, T, F, T, T, F, TF, T ] )
    val M8 = Array2D.fromList( 3, 3, [ F, F, TF, F, T, T, TF, T, T ] )

    val Masks = [ M1, M2, M3, M4, M5, M6, M7, M8 ]
  

    fun valid( Edge : bool, Mask : bool option ) : bool =
      if Mask=TF then
        true
      else if Mask=T then
        Edge
      else 
        not Edge

    fun iter( Current : BooleanImage.image ) : BooleanImage.image =
    let
      val Thinned = BooleanImage.image( Width, Height, false )
      val _ = BooleanImage.appi
        ( fn( I, X ) =>
            BooleanImage.update'( Thinned, I, X ) )
        Current

      fun sub( Image : BooleanImage.image, X : int, Y : int ) : bool =
        if X<Width andalso X>=0 andalso Y<Height andalso Y>=0 then
          BooleanImage.sub( Image, X, Y )
        else 
          false

      fun apply( Masks : bool option Array2D.array list ) : unit =
        case Masks of
          [] => ()
        | Mask::RMasks => 
          let
            val Matched = BooleanImage.image( Width, Height, false )

            val _ = BooleanImage.appxy
              ( fn( X, Y, _ ) => 
                let
                  val Valid = Array2D.foldlij
                    ( fn( I, J, M, V ) => 
                      let 
                        val E = sub( Thinned, X+J-1, Y+I-1 )
                      in
                        if V andalso not( valid( E, M ) ) then
                          false
                        else 
                          V
                      end )
                    true
                    Mask
                in
                  if Valid then
                    BooleanImage.update( Matched, X, Y, true )
                  else
                    ()                   
                end )
              Thinned
           
            val _ = BooleanImage.subtract'( Thinned, Matched )

          in
            apply RMasks
          end

      val _ = apply Masks
    in
      if BooleanImage.equal( Current, Thinned ) then
        Current
      else
        iter Thinned
    end
    
  in
    iter Image
  end

  fun thicken( Image as { Width, Height, ... } : BooleanImage.image ) 
      : BooleanImage.image =
  let 
    val Complement = BooleanImage.image( Width, Height, false )
    val _ = BooleanImage.modifyi
      ( fn( I, _ ) =>
          if BooleanImage.sub'( Image, I ) then
            false
          else 
            true )
      Complement
  in
    thin Complement
  end

end (* structure Morphology *)

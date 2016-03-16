(*
* file: morphology.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with common morphological algorithms.
*)

structure Morphology =
struct

  fun thin( im : BooleanImage.image ) 
      : BooleanImage.image =
  let

    val ( height, width ) = BooleanImage.dimensions im

    val t = SOME true
    val f = SOME false
    val tf = NONE

    val m1 = Array2.fromList( [ [ f, f, f ], [ tf, t, tf ], [ t, t, t ] ] )
    val m2 = Array2.fromList( [ [ tf, f, f ], [ t, t, f ], [ t, t, tf ] ] )
    val m3 = Array2.fromList( [ [ t, tf, f ], [ t, t, f ], [ t, tf, f ] ] )
    val m4 = Array2.fromList( [ [ t, t, tf ], [ t, t, f ], [ tf, f, f ] ] )
    val m5 = Array2.fromList( [ [ t, t, t ], [ tf, t, tf ], [ f, f, f ] ] )
    val m6 = Array2.fromList( [ [ tf, t, t ], [ f, t, t ], [ f, f, tf ] ] )
    val m7 = Array2.fromList( [ [ f, tf, t ], [ f, t, t ], [ f, tf, t ] ] )
    val m8 = Array2.fromList( [ [ f, f, tf ], [ f, t, t ], [ tf, t, t ] ] )

    val masks = [ m1, m2, m3, m4, m5, m6, m7, m8 ]
  

    fun valid( edge : bool, mask : bool option ) : bool =
      if mask=tf then
        true
      else if mask=t then
        edge
      else 
        not edge

    fun iter( current : BooleanImage.image ) : BooleanImage.image =
    let
      val thinned = BooleanImage.zeroImage( height, width )
      val _ = BooleanImage.appi BooleanImage.RowMajor
        ( fn( i, j, x ) =>
            BooleanImage.update( thinned, i, j, x ) )
        current

      fun sub( im : BooleanImage.image, y : int, x : int ) : bool =
        if x<width andalso x>=0 andalso y<height andalso y>=0 then
          BooleanImage.sub( im, y, x )
        else 
          false

      fun apply( masks : bool option Array2.array list ) : unit =
        case masks of
          [] => ()
        | mask::masks' => 
          let
            val matched = BooleanImage.zeroImage( height, width )

            val _ = 
              BooleanImage.appi BooleanImage.RowMajor
                ( fn( y, x, _ ) => 
                  let
                    val valid = 
                      Array2.fold Array2.RowMajor
                        ( fn( i, j, m, v ) => 
                          let 
                            val e = sub( thinned, y+i-1, x+j-1 )
                          in
                            if v andalso not( valid( e, m ) ) then
                              false
                            else 
                              v
                          end )
                        true
                        { base=mask, row=0, col=0, nrows=NONE, ncols=NONE } 
                  in
                    if valid then
                      BooleanImage.update( matched, y, x, true )
                    else
                      ()                   
                  end )
              ( BooleanImage.full thinned )
           
            val _ = BooleanImage.subtract'( thinned, matched )

          in
            apply masks'
          end

      val _ = apply masks
    in
      if BooleanImage.equal( current, thinned ) then
        current
      else
        iter thinned
    end
    
  in
    iter im
  end

  fun thicken( im as { width, height, ... } : BooleanImage.image ) 
      : BooleanImage.image =
  let 
    val complement = BooleanImage.image( width, height, false )
    val _ = BooleanImage.modifyi
      ( fn( i, _ ) =>
          if BooleanImage.sub'( im, i ) then
            false
          else 
            true )
      complement
  in
    thin complement
  end

end (* structure Morphology *)

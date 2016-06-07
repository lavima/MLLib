(*
* filename: segment.sml
* author: Lars Vidar Magnusson <lars.v.magnusson@hiof.no>
*
* This file contains a structure with image segmentation functionality.
*)

structure Segment =
struct

  type segMap = IntGrayscaleImage.image
  type edgeMap = BooleanImage.image

  fun toEdgeMap( seg : segMap ) : edgeMap =
  let
    val ( height, width ) = IntGrayscaleImage.dimensions seg

    val bdry = BooleanImage.zeroImage( height*2+1, width*2+1 )
    val ( bdryHeight, bdryWidth ) = BooleanImage.dimensions bdry

    val edgelsV = BooleanImage.zeroImage( height, width )
    val edgelsH = BooleanImage.zeroImage( height, width )

    val _ = 
      Util.loopFromToInt
        ( fn x => 
            Util.loopFromToInt 
              ( fn y => 
                  BooleanImage.update( 
                    edgelsH, 
                    y, x, 
                    not( 
                      IntGrayscaleImage.sub( seg, y, x )
                      =
                      IntGrayscaleImage.sub( seg, y, x+1 ) ) ) )
              ( 0, height-1, 1 ) )
        ( 0, width-2, 1 )

    val _ = 
      Util.loopFromToInt
        ( fn y => 
            Util.loopFromToInt 
              ( fn x => 
                  BooleanImage.update( 
                    edgelsV, 
                    y, x, 
                    not( 
                      IntGrayscaleImage.sub( seg, y, x )
                      =
                      IntGrayscaleImage.sub( seg, y+1, x ) ) ) )
              ( 0, width-1, 1 ) )
        ( 0, height-2, 1 )

    val _ = 
      Util.loopFromToInt
        ( fn x => 
            Util.loopFromToInt 
              ( fn y => (
                  BooleanImage.update( 
                    bdry, 
                    y, x+1, 
                    BooleanImage.sub( edgelsH, y div 2, x div 2 ) );
                  BooleanImage.update( 
                    bdry, 
                    y+1, x,  
                    BooleanImage.sub( edgelsV, y div 2, x div 2 ) );
                  if x<bdryWidth-2 andalso y<bdryHeight-2 then
                    BooleanImage.update( 
                      bdry, 
                      y+1, x+1, 
                      BooleanImage.sub( edgelsH, y div 2, x div 2 ) 
                      orelse
                      BooleanImage.sub( edgelsH, ( y div 2 )+1, x div 2 ) 
                      orelse
                      BooleanImage.sub( edgelsV, y div 2, x div 2 ) 
                      orelse
                      BooleanImage.sub( edgelsV, y div 2, ( x div 2 )+1 ) )
                  else
                    () ) )
              ( 1, bdryHeight-2, 2 ) )
        ( 1, bdryWidth-2, 2 )

    val _ = 
      Util.loopFromToInt
        ( fn x => (
            BooleanImage.update( 
              bdry, 
              0, x, 
              BooleanImage.sub( bdry, 1, x ) ); 
            BooleanImage.update( 
              bdry, 
              bdryHeight-1, x, 
              BooleanImage.sub( bdry, bdryHeight-2, x ) ) ) )
        ( 0, bdryWidth-1, 1 )

    val _ = 
      Util.loopFromToInt
        ( fn y => (
            BooleanImage.update( 
              bdry, 
              y, 0, 
              BooleanImage.sub( bdry, y, 1 ) ); 
            BooleanImage.update( 
              bdry, 
              y, bdryWidth-1, 
              BooleanImage.sub( bdry, y, bdryWidth-2 ) ) ) )
        ( 0, bdryHeight-1, 1 )

    val out = BooleanImage.zeroImage( height, width )
    val _ = 
      Util.loopFromToInt
        ( fn x => 
            Util.loopFromToInt 
              ( fn y => 
                  BooleanImage.update( 
                    out, 
                    ( y-1 ) div 2, ( x-1 ) div 2, 
                    BooleanImage.sub( bdry, y, x ) ) )
              ( 2, bdryHeight-1, 2 ) )
        ( 2, bdryWidth-1, 2 )
  in
    out
  end

end (* struct Segment *)

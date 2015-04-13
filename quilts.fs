(*
   Quilts an implementation of Chapter 4 case study

   Skeleton code (incl. Graphics module) provided by Dr. Fenwick
   Sewing module implemented by: YOUR NAME
*)

module JQuilts

    (* Let's define a "type Texture" to be a union of Arcs or Bands,
       and a "type Direction" to a union of NE, SE, SW, NW
    *)
    type Texture =  | Arcs
                    | Bands
    type Direction = | NE
                     | SE
                     | SW
                     | NW

    (* Let's define the two starting squares for anyone to use! *)
    let arcsNE = [[(Arcs, NE)]]
    let bandsNE = [[(Bands, NE)]];;


    (*************************************************************
     *******         S E W I N G           M O D U L E   *********
     *******                                             *********
     *******   Student writes all the sewing functions.  *********
     ************************************************************)
    module Sewing =

       let rec sew quilt1 quilt2 =
           let isEmpty list = List.forall (fun element -> element = []) list
           if isEmpty quilt1 && isEmpty quilt2
           then []
           //else List.zip quilt1 quilt2
           else
               (quilt1.Head @ quilt2.Head) :: (sew quilt1.Tail quilt2.Tail)

       let clockwise (dir:Direction) =
           match dir with
           | NE -> SE
           | SE -> SW
           | SW -> NW
           | NW -> NE
//
       let turnSquare sq =
           let texture = fst sq
           let direction = snd sq
           (texture, clockwise direction)
//
//       let rec turn quilt =
//
//       let unturn quilt =
//
//       (* Do this like it would be sewn, so can only use sewing functions
//          like sew, pile, turn, etc. *)
//       let rec pile quilt1 quilt2 =
//
//       (* An efficient one-statement way to combine the lists of tuples
//          to achieve a pile. *)
//       let rec effpile quilt1 quilt2 =



    (*************************************************************
     *******         G R A P H I C S       M O D U L E   *********
     *******                                             *********
     *******    does all the bitmap-y image stuff.       *********
     ************************************************************)
    module Graphics =
       open System.Drawing

       let encodeImage square =
          match square with
          | Bands,NE ->  new Bitmap("bandsNE.JPG");
          | Bands,SE ->  new Bitmap("bandsSE.JPG");
          | Bands,SW ->  new Bitmap("bandsSW.JPG");
          | Bands,NW ->  new Bitmap("bandsNW.JPG");
          | Arcs,NE -> new Bitmap("arcsNE.JPG");
          | Arcs,SE -> new Bitmap("arcsSE.JPG");
          | Arcs,SW -> new Bitmap("arcsSW.JPG");
          | Arcs,NW -> new Bitmap("arcsNW.JPG");

       let stitchRowImage r = List.map encodeImage r

       let rec calcWidth (r : List<Bitmap>) width =
          match r with
          | [] -> width
          | hd::td -> let r = calcWidth td width
                      hd.Width + r

       let rec calcHeight x =
          match x with
          | [] -> 0
          | hd::td -> 80 + calcHeight td

       let calcRowWidth r = calcWidth r 0

       let saveFinalImage (bitmaps : List<List<Bitmap>>)
                       (final : Bitmap) (fileName: string) =
          let g = Graphics.FromImage(final)
          let mutable widthOffset = 0
          let mutable heightOffset = 0
          for row in bitmaps do
             widthOffset <- 0
             for b in row do
                g.DrawImage(b, new Rectangle(widthOffset, heightOffset, 80, 80))
                widthOffset <- widthOffset + 80;
             heightOffset <- heightOffset + 80
          final.Save(fileName, System.Drawing.Imaging.ImageFormat.Jpeg);

       let createQuiltImage quilt file =
          let bitmaps = List.map stitchRowImage quilt
          let width = calcRowWidth (List.head bitmaps)
          let height = calcHeight bitmaps
          let final = new Bitmap(width, height)
          saveFinalImage bitmaps final file
          ()



    (*************************************************************
     *******         T E S T I N G         M O D U L E   *********
     *******                                             *********
     *******    Puts it all together!                    *********
     ************************************************************)
//    module Testing =
//       open Sewing
//       open Graphics
//
//       (* Example testing as shown in textbook *)
//       let slice =
//          let aa = pile (unturn arcsNE) (turn arcsNE)
//          let bb = pile bandsNE (turn (turn bandsNE))
//          let q = sew aa bb
//          pile q q
//       let page47quilt = sew slice (sew slice slice)
//
//       let makePage47quilt =
//          createQuiltImage  page47quilt "p47quilt.jpg";;



       (* Student testing code to make a 2x12 "runner" quilt, save
          in "runner.jpg" file. *)




       (* Student testing code to make a 30 square quilt, save
          in "squares30.jpg" file. *)



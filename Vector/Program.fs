// Learn more about F# at http://fsharp.org
module Program
module V = col.Vector
type intVector = col.Vector<int>
module SV = col.Vector.SubVector
let smallData () =
    let emptyInt = intVector.EMPTY
    let a = 
      intVector.EMPTY 
      |> V.add 11 
      |> V.add 22
    let b = 
      intVector.EMPTY
      |> V.add 11 
      |> V.add 22
    let c = 
      intVector.EMPTY
      |> V.add 11 
      |> V.add 33
    let sa = V.sub 0 2 a
             |> Option.get 
    let ra = V.rev a
    struct {|a= a; b= b; c= c; sa = sa; ra = ra|}

let tryList () =
    let (a,b,c) = let data = smallData () 
                  (data.a, data.b, data.c)
    if  (V.list a) = [11;22] |> not then failwith "Not added"

let tryEquals () =
    let (a,b,c) = let data = smallData () 
                  (data.a, data.b, data.c)
    
    if not (a = b)
    then failwith "not equal"
    if a = c 
    then failwith "Should not be equal"

let tryFirst () =
    let a= (smallData ()).a 
    if not (Some 11 = (V.first a)) then failwith "not first"
   
let trysize () =
    let a= (smallData ()).a 
    if not (V.size a = 2) then failwith "not good size"

let tryget () =
    let a= (smallData ()).a 
    if not (Some 22 = (V.get 1 a)) then failwith "Not second"

let tryConcatAndAppend ()=
    let (a,b,c) = let data = smallData () 
                  (data.a, data.b, data.c)
    let ac = 
         intVector.EMPTY
         |> V.add 11 
         |> V.add 22
         |> V.add 11 
         |> V.add 33
    if not (ac = V.concat c a) 
    then failwith "not concatted"
    if not (ac = V.append [11;33] a)
    then failwith "not appended"
let tryOfList () =
    let a= (smallData ()).a 
    if not (a = V.ofList [11;22])
    then failwith "not ofList"

let trySet () =
    let (a,b,c) = let data = smallData () 
                  (data.a, data.b, data.c)
    if not (Some c = V.set 1 33 a)
    then failwith "Could not set"
let tryDrop () =
    let a= (smallData ()).a
    if not (Some (V.empty () |> V.add 11) = V.drop 1 a)
    then failwith "no drop"
let tryLast ()=
    let a= (smallData ()).a
    if not (Some 22 = V.last a)
    then failwith "Not last"
let tryFold ()=
    let a= (smallData ()).a
    if not (33 = V.fold (+) 0 a)
    then failwith "not folding sum"
let tryFilter ()=
    let a= (smallData ()).a
    if not ( (V.ofList [11]) = (V.filter (fun x -> x % 2 = 1) a))
    then failwith "not filtered"
let tryCut ()=
  let a= (smallData ()).a
  if not ((V.cut 1 a |> function Some v -> SV.list v
                                 | None -> failwith "was none") = [11])
    then failwith "no cut"
let tryOfSub ()=
    let a= (smallData ()).a
    if not ((=) (V.empty ()
                 |> V.add 22
                 |> V.add 11)
                (V.rev a
                 |> V.ofSub))
    then failwith "no of Sub"

let subList () =
    let (a,sa) = let data = smallData () 
                 (data.a, data.sa)
    if not (SV.list sa = V.list a)
    then failwith "lists are not equal"
    
let trySubGet () = 
    let (a,sa,ra) = let data = smallData () 
                    (data.a, data.sa, data.ra)
    if not (SV.get 0 sa = V.get 0 a)
    then failwith "first is not equal"
    if not (SV.get 2 sa = V.get 2 a)
    then failwith "first is not equal"
    if not (None = SV.get 2 sa)
    then failwith "first outofbounds"
    if not (SV.get -1 sa = V.get -1 a)
    then failwith "first is not equal"
    if not (None = SV.get -1 sa)
    then failwith "first outofbounds"
    if not (Some 11 = SV.get 0 sa)
    then failwith "not the first"
    if not (Some 11 = SV.get 1 ra)
    then failwith "no get on rev"
    if not (None = SV.get 2 ra)
    then failwith "outofbounds"
    if not (None = SV.get -1 ra)
    then failwith "less than zero rev"
    if not (Some 11 = SV.first sa)
    then failwith "not the first"

let tryFirstRev ()=
    let (sa,ra) = let data = smallData () 
                  (data.sa, data.ra)
    if not (sa = SV.rev ra)
    then failwith "no rev rev"
    if not (Some 22 = SV.first ra)
    then failwith "no get on rev"
    if not (Some 11 = SV.last ra)
    then failwith "last ra"
    if not (SV.first sa = SV.last ra)
    then failwith "last first"
let trySubDrops ()=
    let (sa,ra) = let data = smallData () 
                  (data.sa, data.ra)
    if not (Some 22 =(SV.drop 1 ra 
                      |> function Some v -> SV.last v 
                                | None -> failwith "none"))
    then failwith "no reverse drops"
    if not (Some 11 =(SV.drop 1 sa 
                         |> function Some v -> SV.last v 
                                   | None -> failwith "none"))
     then failwith "no forward drops"
let trySubCuts () =
    let (sa,ra) = let data = smallData () 
                  (data.sa, data.ra)
    let h1 = intVector.EMPTY  |> V.add 11 |> V.sub 0 1 
    let h2 = SV.cut 1 ra
    if not ( h1 = h2 )
    then failwith "not reverse cuts"
  
    if not (V.empty () |> V.add 22 |> V.sub 0 1 = SV.cut 1 sa )
    then failwith "no cuts"
let trySubSubs () =
    let (sa,a) = let data = smallData () 
                 (data.sa, data.a)
    if not ((V.sub 1 1 a) = (SV.sub 1 1 sa))
    then failwith "no subsub"
    let x = V.ofList [3;2;1] |> V.sub 0 3
    let y1 =  V.rev (V.ofList [1;2;3;4])
    let y = SV.sub 1 3 y1
    if not (x = y)
    then failwith "no reverse sub"

let trySubFolds () =
    let (sa,ra) = let data = smallData () 
                  (data.sa, data.ra)
    if not ([22;11] = SV.fold (fun a x -> x::a) [] sa)
    then failwith "not folding sum"
    
    if not ([11;22] = SV.fold (fun a x -> x::a) [] ra)
    then failwith "not folding sum"

    if not ([11;22] = SV.foldback (fun x a -> x::a) sa [])
    then failwith "not folding sum"
    
    if not ([22;11] = SV.foldback (fun x a -> x::a) ra [])
    then failwith "not folding sum"
let tryRevReving () =
    let (sa,ra) = let data = smallData () 
                  (data.sa, data.ra)
    if not (sa = SV.rev ra)
    then failwith "not reving the rev"
let trySubListing ()=
    let (sa,ra) = let data = smallData () 
                  (data.sa, data.ra)
    if not ([11;22] = SV.list sa)
    then failwith "not listing sub"

    if not ([22;11] = SV.list ra)
    then failwith "not listing rev"

let tryABitLarger ()=
    let n = 1900
    let r = List.fold (fun a v -> V.add v a) (V.empty ()) [0..n]  
    List.iter (fun x -> if not (Some x = V.get x r) then failwith "unexpected when indexing bigger") [0..n]
    let r = List.fold (fun a x -> (V.set x (1 + (V.get x r).Value) a).Value) r [0..n]
    List.iter (fun x -> if not (Some (x+1) = V.get x r) then failwith "unexpected when modifying bigger") [0..n]

    let r = List.fold (fun a v -> V.add v a) (V.empty ()) [0..1056] 
    let r = List.fold (fun a v -> (V.pop a).Value) r [0..1056]
    if not (r=V.empty ()) then failwith "not emptied"

let loo () =
    tryList ()
    tryEquals ()
    tryFirst ()
    trysize ()
    tryget ()
    tryConcatAndAppend ()
    tryOfList ()
    trySet ()
    tryDrop ()
    tryLast ()
    tryFold ()
    tryFilter ()

    tryCut ()
    tryOfSub ()
    trySubGet ()
    tryFirstRev ()
    trySubDrops ()
    trySubCuts ()
    trySubSubs ()
    trySubFolds ()
    tryRevReving ()
    trySubListing ()
    tryABitLarger ()
    

    
    
    

    printfn "The skinny goat says all is nice and shiny"
    

[<EntryPoint>]
let main argv = 
  for i in 0..0 do
    loo ()
  0

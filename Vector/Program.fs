// Learn more about F# at http://fsharp.org
module Program
module V = fsgrit.Vector
type V<'T when 'T : equality> = fsgrit.Vector<'T>
module SV = fsgrit.SubVector

  
     

let tryList () =
    let a = V.ofList [11;22]
    if  (V.toList a) = [11;22] |> not then failwith "Not added"

let tryEquals () =
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (a = b)
    then failwith "not equal"
    if a = c 
    then failwith "Should not be equal"

let tryHash () =
    let a = [1,2,3]
    let b = V.ofList a
    if not (a.GetHashCode () = b.GetHashCode())
    then failwith "hash"

let tryFirst () =
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (Some 11 = (V.first a)) then failwith "not first"
   
let trysize () =
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (V.size a = 2) then failwith "not good size"
    if not (V.size (V.empty ()) = 0) then failwith "not good size"

let tryget () =
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (Some 22 = (V.get 1 a)) then failwith "Not second" 
    if not  (Some 11 = (V.get 0 a)) then failwith "Not First"
    if not (None = (V.get -1 a)) then failwith "No outofbounds"
    if not (None = (V.get 2 a)) then failwith "No outof upper bounds"
    
let tryAddAllAndAppend ()=
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    let ac = 
         V.EMPTY
         |> V.add 11 
         |> V.add 22
         |> V.add 11 
         |> V.add 33
    if not (ac = V.append a c) 
    then failwith "not appended"
    if not (ac = V.addAll [11;33] a)
    then failwith "not addAlled"
    if not ( (V.empty ()) = V.append (V.empty ()) (V.empty ()) )
    then failwith "no empty append"
    if not (a = V.addAll [] a)
    then failwith "not addAll empty"
    

let tryOfList () =
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (a = V.ofList [11;22])
    then failwith "not ofList"
    if not ((V.empty ()) = V.ofList [])
    then failwith "no empty list"

let trySet () =
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (Some c = V.set 1 33 a)
    then failwith "Could not set"
    let x = V.add 1 (V.empty ())
    let y = (V.set 0 1 (V.empty ())).Value 
    if not (x = y)  
    then failwith "set on empty"
    if not (None = (V.set 1 1 (V.empty ())))
    then failwith "Setting out of bounds"
    if not (None = (V.set -1 1 (V.empty ())))
    then failwith "Setting out of bounds"
    

let tryDrop () =
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (Some (V.empty () |> V.add 11) = V.drop 1 a)
    then failwith "no drop"
    if not (Some (V.empty () ) = V.drop 2 a)
    then failwith "no drop all"
    if not (None = V.drop 3 a)
    then failwith "no drop too much"
    
    let n = 33
    let big = List.fold (fun a v -> V.add v a) (V.empty ()) [1..n]
    let res = List.fold (fun a v -> (V.pop  a).Value) big [2..n] 
    if V.isEmpty (res) 
    then failwith "Is empty" 
    if not (V.isEmpty (V.pop res).Value) 
    then failwith "Not empty"
 
    let n = 34
    let big = List.fold (fun a v -> V.add v a) (V.empty ()) [1..n]
    let res = List.fold (fun a v -> (V.pop  a).Value) big [2..n] 
    if V.isEmpty (res) 
    then failwith "Is empty" 
    if not (V.isEmpty (V.pop res).Value) 
    then failwith "Not empty"


    let n = 32*32+ 32//+32+1
    let big = List.fold (fun a v -> V.add v a) (V.empty ()) [1..n]
    let res = List.fold (fun a v -> (V.pop  a).Value) big [2..n] 
    if V.isEmpty (res) 
    then failwith "Is empty" 
    if not (V.isEmpty (V.pop res).Value) 
    then failwith "Not empty" 

    let n = 32*32+ 32+1//+32+1
    let big = List.fold (fun a v -> V.add v a) (V.empty ()) [1..n]
    let res = List.fold (fun a v -> (V.pop  a).Value) big [2..n] 
    if V.isEmpty (res) 
    then failwith "Is empty" 
    if not (V.isEmpty (V.pop res).Value) 
    then failwith "Not empty" 
    
    let n = 32*32*32+32+1
    let big = List.fold (fun a v -> V.add v a) (V.empty ()) [1..n]
    let res = List.fold (fun a v -> (V.pop  a).Value) big [2..n]
    if V.isEmpty (res) 
    then failwith "Is empty" 
    if not (V.isEmpty (V.pop res).Value) 
    then failwith "Not empty"

    let n = 10000
    let big = List.fold (fun a v -> V.add v a) (V.empty ()) [1..n]
    let half = (V.drop 5000 big).Value;
    V.fold (fun a v -> if not (a = v) 
                          then failwith "not equal" 
                          else a + 1) 
           1 
           half
    |> ignore<int>
    let less = (V.drop 4980 half).Value
    V.fold (fun a v -> if not (a = v) 
                          then failwith "not equal" 
                          else a + 1) 
           1 
           less
    |> ignore<int>
    if not (20 = (V.size less))
    then failwith "not 20"
    
let tryLast ()=
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (Some 22 = V.last a)
    then failwith "Not last"
    if not (None = V.last (V.empty ()))
    then failwith "Not last empty"

let tryFold ()=
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not (33 = V.fold (+) 0 a)
    then failwith "not folding sum"
    if not (0 = V.fold (+) 0 (V.empty ()))
    then failwith "not folding empty"

let tryFilter ()=
    let a = V.ofList [11;22]
    let b = V.ofList [11;22]
    let c = V.ofList [11;33]
    if not ( (V.ofList [11]) = (V.filter (fun x -> x % 2 = 1) a))
    then failwith "not filtered"
    if not (V.empty() = V.filter  (fun x -> x % 2 = 1) (V.empty ()))
    then failwith "not nothing filter"

let tryCut ()=
  let a = V.ofList [11;22]
  if not ((V.cut 1 a |> function Some v -> SV.toList v
                               | None -> failwith "was none") = [22])
  then failwith "no cut"
  if not (None = (V.cut 1 (V.empty ()))) 
  then failwith "emptyCut"

  if not (None = (V.cut -1 (V.empty ()))) 
  then failwith "emptyCut"

  if not (None = (V.cut -1 a)) 
  then failwith "emptyCut"
  if not ((V.cut 0 (V.empty ()) |> function Some v -> SV.toList v
                                          | None -> failwith "was none") = [])
  then failwith "no cut 0"


let tryOfSub ()=
    let a = V.ofList [11;22]
    if not ((=) (V.empty ()
                 |> V.add 22
                 |> V.add 11)
                (V.rev a
                 |> V.ofSub))
    then failwith "no of Sub"
    let x = V.empty ()
    let y = V.empty () 
            |> (V.sub 0 0)
            |> Option.get
            |> V.ofSub
    if not (x=y)
    then failwith "no empty of Sub"
    if not (None = V.sub -1 2 a)
    then failwith "No lower bounds on sub"
    if not (None = V.sub 10 1 a)
    then failwith "No high bounds on sub"
    if not (None = V.sub 0 3 a)
    then failwith "no upper bounds on sub"
    
let subList () =
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    if not ((SV.toList sa) = (V.toList a))
    then failwith "lists are not equal"
    if not ([] = (V.empty () |> V.rev |> SV.toList))
    then failwith "no empty sublist"
    
let trySubGet () = 
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    let ra = V.rev a
    if not (SV.get 0 sa = V.get 0 a)
    then failwith "first is not equal"
    if not (SV.get 2 sa = V.get 2 a)
    then failwith "first is not equal"
    if not (None = SV.get 2 sa)
    then failwith "first outofbounds"
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
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    let ra = V.rev a
    if not (sa = SV.rev ra)
    then failwith "no rev rev"
    if not (Some 22 = SV.first ra)
    then failwith "no get on rev"
    if not (Some 11 = SV.last ra)
    then failwith "last ra"
    if not (SV.first sa = SV.last ra)
    then failwith "last first"

let trySubDrops ()=
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    let ra = V.rev a
    if not (Some 22 =(SV.drop 1 ra 
                      |> function Some v -> SV.last v 
                                | None -> failwith "none"))
    then failwith "no reverse drops"
    if not (Some 11 =(SV.drop 1 sa 
                         |> function Some v -> SV.last v 
                                   | None -> failwith "none"))
     then failwith "no forward drops"
let trySubCuts () =
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    let ra = V.rev a
    let h1 = V.EMPTY  |> V.add 11 |> V.sub 0 1 
    let h2 = SV.cut 1 ra
    if not ( h1 = h2 )
    then failwith "not reverse cuts"
  
    if not (V.empty () |> V.add 22 |> V.sub 0 1 = SV.cut 1 sa )
    then failwith "no cuts"
let trySubSubs () =
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    let ra = V.rev a
    if not ((V.sub 1 1 a) = (SV.sub 1 1 sa))
    then failwith "no subsub"
    let x = V.ofList [3;2;1] |> V.sub 0 3
    let y1 =  V.rev (V.ofList [1;2;3;4])
    let y = SV.sub 1 3 y1
    if not (x = y)
    then failwith "no reverse sub"

let trySubFolds () =
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    let ra = V.rev a
    if not ([22;11] = SV.fold (fun a x -> x::a) [] sa)
    then failwith "not folding sum"
    
    if not ([11;22] = SV.fold (fun a x -> x::a) [] ra)
    then failwith "not folding sum"

    if not ([11;22] = SV.foldBack (fun x a -> x::a) sa [])
    then failwith "not folding sum"
    
    if not ([22;11] = SV.foldBack (fun x a -> x::a) ra [])
    then failwith "not folding sum"

let tryRevReving () =
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    let ra = V.rev a
    if not (sa = SV.rev ra)
    then failwith "not reving the rev"

let trySubListing ()=
    let a = V.ofList [11;22]
    let sa = (V.sub 0 2 a).Value
    let ra = V.rev a
    if not ([11;22] = SV.toList sa)
    then failwith "not listing sub"

    if not ([22;11] = SV.toList ra)
    then failwith "not listing rev"

let tryBind () =
    let a = V.ofList [1;2;3]
    let x = V.ofList [0;1;0;2;0;3]
    let y = V.bind (fun x -> V.ofList [0;x] ) a
    if not (x = y)
    then failwith "no bind"

let mutable (data:Map<string, int64 list>)  = Map.empty
let duration str f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    timer.Stop ()
    match Map.tryFind str data with
    | Some l -> data <- Map.add  str ((timer.ElapsedMilliseconds)::l) data
    | None -> data <- Map.add  str [timer.ElapsedMilliseconds] data
   
    returnValue  

let performanceCopyOnWrite n = 
   duration (sprintf "Add copyOnWrite %i" n) 
                    (fun _ -> List.fold (fun (a:System.Collections.Generic.List<int>) v ->
                                             let b =System.Collections.Generic.List<int> a
                                             b.Add v |> ignore
                                             b) 
                                         ( System.Collections.Generic.List<int> ()) 
                                         [0..n]) |> ignore
let performance n =
   
   for y in [0..2] do
      printfn "%i %i" n y 
      let r = duration  (sprintf "Add %i" n)  
                        (fun _ -> List.fold (fun a v -> V.add v a) (V.empty ()) [0..n])
      duration  (sprintf "Add linked list %i" n)  
                        (fun _ -> List.fold (fun a v -> v::a) ([]) [0..n]) |> ignore
      duration  (sprintf "Add linked reverse %i" n)  
                   (fun _ -> List.fold (fun a v -> v::a) ([]) [0..n]) |> List.rev |> ignore
      let al = duration (sprintf "Add Generic List %i" n) 
                        (fun _ -> List.fold (fun (a:System.Collections.Generic.List<int>) v -> 
                                                a.Add v |> ignore
                                                a) 
                                            ( System.Collections.Generic.List<int> ()) 
                                            [0..n])

      duration (sprintf "Get %i" n) 
               (fun x -> List.iter (fun x -> if not ( x = V.getOrFail x r) 
                                             then failwith "unexpected when indexing bigger") 
                                   [0..n])
      duration (sprintf "Get Generic List %i" n)
               (fun x -> List.iter (fun (x:int) -> if not ( x = al.[x] ) 
                                                   then failwith "unexpected when indexing bigger") 
                                   [0..n])
      let r = duration (sprintf "Set %i" n) 
                       (fun x -> List.fold (fun a x -> (let inc = 1 + (V.getOrFail x r)
                                                       V.setOrFail x inc a)) 
                                           r 
                                           [0..n])

      let al = duration ( sprintf "Set Generic list %i" n) 
                        (fun x -> List.fold (fun (a:System.Collections.Generic.List<int>) x -> 
                                                  let inc = 1 + ( (al.[x]))
                                                  a.[x] <- inc
                                                  a) 
                                            al 
                                            [0..n])
      let no = duration (sprintf "Removing %i" n )
                         (fun x -> List.fold (fun a x -> 
                                                  V.popOrFail a) 
                                             r 
                                             [0..n])
      List.iter (fun x -> if not (Some (x+1) = V.get x r) then failwith "unexpected when modifying bigger") [0..n]

      let n32 = List.fold (fun a v -> V.add v a) (V.empty ()) [1..32]
      let n15 = List.fold (fun a v -> V.add v a) (V.empty ()) [1..15]
      let appended = duration (sprintf "append to 32 %i" n)  
                              (fun _ -> V.append n32 r)
      let appended = duration (sprintf "append to 15 %i" n)  
                              (fun _ -> V.append n15 r)

      let l32 = List.fold (fun (a:System.Collections.Generic.List<int>) v -> 
                                      a.Add v
                                      a) 
                          (System.Collections.Generic.List<int> ()) [1..32]
      duration (sprintf "append to 32 list %i" n) 
               (fun _ -> (l32).AddRange al)

      duration (sprintf "fold %i" n) (fun () -> V.fold (+) 0 r) |> ignore
      duration (sprintf "fold subvec %i" n) (fun () -> SV.fold (+) 0 (V.sub 0 (V.size r) r).Value) |> ignore
       

   let r = List.fold (fun a v -> V.add v a) (V.empty ()) [0..1056] 
   let r = List.fold (fun a v -> (V.pop a).Value) r [0..1056]
   if not (r=V.empty ()) then failwith "not emptied"

let printData () =
  Map.iter (fun k v -> List.sort v
                       |> List.item (((List.length v)/2)) 
                       |> printfn "%s:%i" k) 
                       data
let tryFoldUntil () =
    let a = V.ofList [11;22]
    if not ((33, None) = ( V.foldUntil (fun a v -> (a+v, None)) 0 a))
    then failwith "not fold until all the way"
    if not ((11, Some "Hi") = ( V.foldUntil (fun a v -> (a+v, Some "Hi")) 0 a))
    then failwith "not fold until once"
    if not ((22, Some "Hi") = ( SV.foldUntil (fun a v -> (a+v, Some "Hi")) 0 (V.rev a)))
    then failwith "not fold until once backwards"
    if not ((33, None) = ( SV.foldUntil (fun a v -> (a+v, None)) 0 (V.rev a)))
    then failwith "not fold until all backwards"

let tryFoldWhile () =
    let a = V.ofList [11;22]
    if not (33 = ( V.foldWhile (fun a v -> (a+v, true)) 0 a))
    then failwith "not fold until all the way"
    if not (11 = ( V.foldWhile (fun a v -> (a+v, false)) 0 a))
    then failwith "not fold until once"
    if not (22 = ( SV.foldWhile (fun a v -> (a+v, false)) 0 (V.rev a)))
    then failwith "not fold until once rev"
    if not (33 = ( SV.foldWhile (fun a v -> (a+v, true)) 0 (V.rev a)))
    then failwith "not fold until all rev"
    if not ( 2 = SV.size (V.toSub a))
    then failwith "not full sub"


let loo () =
    tryList ()
    tryEquals ()
    tryHash ()
    tryFirst ()
    trysize ()
    tryget ()
    tryAddAllAndAppend ()
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
    tryBind ()
    performance 10000
    performance 100000
    performance 500000
    performanceCopyOnWrite 10000
    tryFoldUntil ()
    tryFoldWhile ()
    printData ()
    printfn "The skinny goat says all is nice and shiny"
module Vector = fsgrit.Vector
module SubVector = fsgrit.SubVector
type Vector<'A  when 'A : equality> = fsgrit.Vector<'A>
type SubVector<'A when 'A : equality> = fsgrit.SubVector<'A>

let tutorial () =
  let fail x = fun () -> failwith x
  let orFail s t = if t then () else failwith s 
  let (v:int Vector) = Vector.EMPTY
  let v = Vector.empty ()
          |> Vector.add "a"
          |> Vector.add "b"

  let v = Vector.singleton "a"
          |> Vector.add "b"
  let v= Vector.ofList ["a";"b"]
  let both = Vector.append v (Vector.ofList ["c";"d"])
  printfn "%A" (Vector.toList both)
  let both = Vector.addAll ["c"; "d"] v
  printfn "%A" (Vector.toList both)
  let all = Vector.concat (Vector.ofList [Vector.singleton "a"
                                          Vector.singleton "b"
                                          Vector.singleton "c"])
  printfn "%A" (Vector.toList all)


  let modified = Vector.setOrFail 1 "B" all
  Vector.toList modified |> printfn "%A" 
  let modified = match Vector.set 1 "B" all with
                 | Some v -> v 
                 | None -> failwith "Huh?"
  printfn "%A->%A" (Vector.toList all) (Vector.toList modified)

  let trans = Vector.ofList [1;2;3]
              |> Vector.map (fun v -> v + 1) 
  let trans = Vector.ofList [1;2;3]
              |> Vector.filter (fun v -> v % 2 = 0)
  let trans = Vector.ofList [1;2;3]
              |> Vector.bind (fun x -> Vector.singleton (x + 1))
  let sum = Vector.ofList [1;2;3]
            |> Vector.fold (fun a x -> a + x) 0

  let sum = Vector.foldWhile (fun a x -> (a + x, x < 2)) 0 (Vector.ofList [1;2;3])

  let item = Vector.getOrFail 1 all
  let item = Vector.get 1 all
             |> Option.defaultWith (fail "Huh?")
           
  printfn "%s" item
  
  let item =  Vector.first all
              |> Option.defaultWith (fail "Huh!")
             
                        
  
  let item = Vector.last all
              |> Option.defaultWith (fail "Huh?")
                        
  let init = Vector.pop all
             |> Option.defaultWith (fail "Huh?")
                     
  let init = Vector.drop 1 all
             |> Option.defaultWith (fail "Huh?")
  let (tail:_ SubVector) = Vector.rest all
                          |> Option.defaultWith (fail "Huh?")
   
  (((SubVector.toList tail) = ["b";"c"] ) || failwith "Huh!" )
  |> ignore

  let sub = Vector.sub 1 2 all
            |> Option.defaultWith (fail "Huh?")
  let rev = SubVector.rev sub
  (["c";"b"] = (SubVector.toList rev))
  |> orFail "Huh?"
    

  ()
             



[<EntryPoint>]
let main argv = 
  for i in 0..0 do
    loo ()
  tutorial ()
  0

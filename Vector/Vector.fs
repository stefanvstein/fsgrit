module col

type  Node<'T> = | Leaf of 'T array
                 | Branch of Node<'T> array
                 | EmptyNode


module private Tools =   
    let  isOutOfBounds size i = 
         i < 0 || i >=size
    
    [<Literal>]
    let BucketBits = 5
      
    let BucketSize = 1 <<< BucketBits
     
    let (>>>>) x y = System.Convert.ToInt32 ((uint32 x) >>> y)
    
    let tailStartOffset size = 
         if size < BucketSize
         then 0  
         else ((size - 1) >>>> BucketBits) <<< BucketBits
    
    let inTailIndex vecsize i =
        let t = tailStartOffset vecsize
        if i >= t
        then struct (true, i-t)
        else struct (false, 0)
    
    let levels size =
         let size = size - 1
         if size < (BucketSize*BucketBits)
         then 1
         else let mutable size = (size - BucketSize) >>>> BucketBits
              let mutable levels = 0
              while size <> 0 do
                levels <- levels + 1
                size <- size >>>> BucketBits
              levels
    
    let indexOfArrayAt offset = offset &&& (BucketSize - 1)
    
    let shift levels = levels * BucketBits 
    
    let indexAtLevel index level = indexOfArrayAt <| (index >>>> (shift level))
    
    let range (from:int) (step:int) (until:int) : int list = [for i in from..step..until -> i]
    
    let pathToValue size index =
         List.map (indexAtLevel index) <| range (levels size) -1 0
    
    let pathToNode size index =
         List.map (indexAtLevel index) <| range (levels size) -1 1
    
    let isInTail index size =
         index >= (tailStartOffset size)
    
    let rec nodeFor path node =
         match path with
         | [i] -> match node with
                  | Leaf _ -> node
                  | _ -> failwith "expecting Leaf"
         | (i::is) -> match node with
                      | Branch ar -> nodeFor is (Array.get ar i) 
                      | _ -> failwith "Should be Branch"
         | [] -> failwith "empty path"
    
    let arrayFor index size root tail =
      if isOutOfBounds size index  then failwith "Outofbounds"
      elif isInTail index size then tail
      else let path = pathToValue size index
           match nodeFor path root with
           | Leaf ar -> ar
           | _ -> failwith "expected leaf"
    
    let rec valueIn path node =
        match path with
        | [i] -> match node with
                 | Leaf ar -> Array.get ar i 
                 | EmptyNode -> failwith "Found empty node"
                 | Branch _ -> failwith "Found a branch"
        | [] -> failwith "Empty path"
        | x::xs -> match node with
                   | Branch ar -> Array.get ar x
                                  |> valueIn xs 
                   | EmptyNode -> failwith "Got emptyNode"
                   | Leaf _ -> failwith "Got leaf"
    
    let  getInNode index size tail root =
        if isOutOfBounds size index
        then failwith "out of bounds"
        match inTailIndex size index with
        | true, v -> Array.get tail v
        | false, _ -> let path = pathToValue size index
                      valueIn path root
    
    
    let tailSize size = size - (tailStartOffset size)
    
    let isFullTail size = tailSize size >= BucketSize
    
    let roomInTail size = BucketSize - (tailSize size)
    
    let isFullRoot size levels = 
      (size >>>> BucketBits) > (1 <<< (shift levels))
    

    
    
    
    

[<CustomEquality>]
[<NoComparison>]
type Vector<'T when 'T : equality> = 
  { size : int
    root : 'T Node
    tail : 'T array 
    mutable hash : int }
  
  override x.Equals(o) = 
    match o with
      | :? Vector<'T> as y ->
           let rec calc i = 
             if i = x.size then true 
             else if (Tools.getInNode i x.size x.tail x.root) = (Tools.getInNode i y.size y.tail y.root)
                  then calc (i+1)
                  else false
           if x.size = y.size 
           then calc 0
           else false
      | _ -> false 
  override x.GetHashCode () =
    if x.hash = -1
    then let rec calc i a = 
              if i = x.size 
              then a.GetHashCode ()
              else calc (i + 1) ((Tools.getInNode i x.size x.tail x.root)::a)
         x.hash <- calc 0 []             
         x.hash
       else x.hash
  static member EMPTY = staticVectorHolder<'T>.emptyVector

and staticVectorHolder<'T when 'T : equality> () =
    static let EMPTY_VECTOR =
       { size = 0
         root = EmptyNode
         tail = Array.empty 
         hash = 0} : 'T Vector
    static member emptyVector = EMPTY_VECTOR



module Get =
    open Tools
    let get index vector =
      if Tools.isOutOfBounds vector.size index
      then None
      else match inTailIndex vector.size index with
            | true, v -> Array.get vector.tail v  |> Some
            | false, _ -> let path = pathToValue vector.size index
                          valueIn path vector.root  |> Some
    
    
    let getOrFail index vector =
      getInNode index vector.size vector.tail vector.root  
    
 
      

[<CustomEquality>]
[<NoComparison>]
type SubVector<'T when 'T : equality> = 
 
  { v : Vector<'T>
    i : int
    n : int
    forward : bool
    mutable hash : int }

  static member EMPTY = staticSubVectorHolder<'T>.emptySubVector
  override x.Equals(o) =
      match o with
      | :? SubVector<'T> as y -> 
        if x.n = y.n
        then let until =  x.n  
             let rec check i =
               if i = until then true
               elif Get.getOrFail (if x.forward then (x.i+i) else (x.i+x.n-i-1)) 
                                         x.v = 
                    Get.getOrFail (if y.forward then (y.i+i) else (y.i+y.n-i-1))
                                         y.v
               then check (i+1)
               else false
             check 0

        else false
      | _ -> false
    override x.GetHashCode () = 
       if x.hash = -1
       then let rec calcf i a = 
              if i = x.n 
              then a.GetHashCode ()
              else calcf (i + 1) ((Get.getOrFail (i+x.i) x.v)::a)
            let rec calcb i a =
              if i = x.n
              then a.GetHashCode ()
              else calcb (i + 1) ((Get.getOrFail (x.i+x.n - i-1) x.v)::a)
            x.hash <- if x.forward 
                      then (calcf 0 [])
                      else (calcb 0 [])
            x.hash

       else x.hash
    static member empty = staticSubVectorHolder<'T>.emptySubVector

and  staticSubVectorHolder<'T when 'T : equality> () =
  static let EMPTY_SUB_VECTOR = 
     { v=staticVectorHolder.emptyVector
       i=0
       n=0
       forward = true
       hash = 0} : 'T SubVector

  static member emptySubVector = EMPTY_SUB_VECTOR

 
module rec Vector =
 
  
  let empty () = Vector.EMPTY

  let get(index:int) (vector:'T Vector) : 'T option = 
    Get.get index vector
    
   
  let first (vector:'T Vector) = get 0 vector

  let size vector = vector.size

  let add element vector = Add.add element vector
  
  
  let append (elements:'T list) vector : 'T Vector = 
    //plz optimize
    List.fold (fun a x -> add x a) vector elements
    
  let ofList elements = 
    Vector.EMPTY  |> append elements 


  let last vector : 'T option = get ((size vector) - 1) vector 

  module SubVector =
   
    let get index subvector : 'T option = 
      if index < subvector.n && index >= 0
      then if subvector.forward 
           then Vector.get (subvector.i + index) subvector.v 
           else Vector.get (subvector.i + subvector.n - 1 - index ) subvector.v
      else None
   
    let first v= get 0 v
    
    let size subvector : int = subvector.n
    
    let last (subvector: 'T SubVector) : 'T option = 
      get (size subvector - 1 ) subvector
    
    let drop n subvector : 'T SubVector option = 
      if subvector.n > n
      then if subvector.forward 
           then Some {subvector with n = subvector.n - n
                                     hash = -1}
           else Some {subvector with i = subvector.i + n
                                     n = subvector.n - n
                                     hash = -1}
      else None
    
    let pop vector = drop 1 vector 
    
    let cut n subvector : 'T SubVector option =
      match struct (subvector.n, n, subvector.forward) with
      | _,0,_ -> Some subvector
      | 0,_,_ -> None
      | sn,n,_ when n > sn -> None
      | sn,n,_ when n = sn -> Some SubVector.empty
      | _, n, true -> Some {subvector with i = subvector.i + n
                                           n = subvector.n - n
                                           hash = -1}
      | _, n, false -> Some {subvector with n = subvector.n - n
                                            hash = -1}

    //let decons vector = get 0 vector, cut 1 vector  
    
    let sub i n subvector: 'T SubVector option =
      if i<0 || n<0 then None
      elif i + n > subvector.n then None
      elif n = 0 then Some SubVector.empty
      elif subvector.n = 0 then None
      elif subvector.forward then Some {subvector with i = subvector.i + i
                                                       n = n
                                                       hash = -1}
      else let i' = subvector.i + subvector.n  - i
           Some {subvector with n = subvector.n - i
                                i = subvector.n - i - n
                                hash = -1} 

    
    let rec fold f a subvector =
      //Plz, optimize by running on each Leaf
      let mutable acc = a 
      let v = subvector.v
      if subvector.forward
      then let ending = subvector.i + subvector.n - 1
           for i = subvector.i to ending do
              acc <- f acc (Get.getOrFail i v)
      else for i = subvector.i+subvector.n-1 downto subvector.i do
              acc <- f acc (Get.getOrFail i v)
      acc 

    let rev subvector : 'T SubVector = 
      { subvector with forward = not subvector.forward
                       hash = -1 }

    let foldback f subvector a=
      fold (fun a x -> f x a) a (rev subvector)
    
    let list  (subvector:'T SubVector) : 'T list = 
      foldback (fun x a -> x::a) subvector [] 

  module Add =
    let leafOfArray array = Leaf array
    let branch array = Branch array

    let rec newPath levels aNode =
      if levels = 0
      then aNode
      else newPath (levels - 1) aNode 
           |> Array.singleton 
           |> branch  

    let newLevelRoot currentRoot levels lastTail =
      let tailNode = leafOfArray lastTail 
      branch [|currentRoot; newPath levels tailNode|] 

    let withPushedIntoDeeperTree vec v = 
      let currentRoot = vec.root
      let newRoot = newLevelRoot currentRoot
                                 <| Tools.levels vec.size
                                 <| vec.tail
      { size = vec.size + 1
        root = newRoot
        tail = Array.singleton v 
        hash = -1} : Vector<'T>
    
    let atIndex  = Array.tryItem

    let cloneNodeWithValue node value index =
      match node with
      | Branch ar -> 
         let len = Array.length ar
         let nar =  if len = index
                    then let t = Array.zeroCreate (len+1) //EmptyNode 
                         Array.blit ar 0 t 0 len
                         t
                    else Array.copy ar
         Array.set nar index value
         branch nar
      | Leaf _ -> node
      | EmptyNode -> failwith "EmptyNode can not contain anything"

    let rec withNodeIn path mother newNode = 
      match path with
      | [index] -> 
          match mother with
          | Branch _ -> cloneNodeWithValue mother newNode index
          | EmptyNode _ -> branch (Array.singleton newNode)
          | Leaf _ -> failwith "Can not modify Leaf nodes"
      | (index::remaining) -> 
          match mother with
          | Branch ar -> 
              match atIndex index ar with
              | Some child -> cloneNodeWithValue mother (withNodeIn remaining child newNode) index
              | None -> cloneNodeWithValue mother (newPath ((List.length path) - 1) newNode ) index                     
          | _ -> failwith "Can only modify branch nodes"
      | [] -> failwith "path is empty"

    let withPushedIntoEquallySizedTree vector v= 
      let size = vector.size 
      let last = vector.size - 1
      let path = Tools.pathToNode size last 
      let newNode = leafOfArray vector.tail
      let newRoot = withNodeIn path vector.root newNode
      { size=size+1
        root=newRoot
        tail = Array.singleton v
        hash = -1 }

    let withPushedIntoTree vector v =
        let size = vector.size
        let levels = Tools.levels size
        if Tools.isFullRoot size levels
        then withPushedIntoDeeperTree vector v
        else withPushedIntoEquallySizedTree vector v
    
    let withPushedIntoTail vector element = 
      {vector with hash = -1
                   size = vector.size + 1 
                   tail = Array.append vector.tail [|element|]}

    let add element vector =
      if Tools.isFullTail vector.size
      then withPushedIntoTree vector element
      else withPushedIntoTail vector element

  module Modify =
    let isAtEnd i vec = vec.size = i
    let rec nodeWith path node value = 
      match path with
      | [i'] -> match node with 
                | Leaf ar -> Leaf (let a = Array.copy ar
                                   Array.set a i' value
                                   a)
                | _ -> failwith "Found something else than a leaf in the path tail"
      | (i' :: ix) -> match node with
                      | Branch ar -> let n = nodeWith ix (Array.get ar i') value
                                     let a = Array.copy ar
                                     Array.set a i' n
                                     Branch a
                      | _ -> failwith "not a branch node"
      | _ -> failwith "empty path"
    
    let modify i value vector =  
      match Tools.inTailIndex vector.size i with
      | true, i' -> {vector with tail = let a = Array.copy vector.tail
                                        Array.set a i' value
                                        a
                                 hash = -1} 
      | false,_ -> let path = Tools.pathToValue vector.size i
                   { vector with hash = -1
                                 root = nodeWith path vector.root value }
    let set i v vec =
      if isAtEnd i vec then Add.add v vec |> Some
      elif Tools.isOutOfBounds vec.size i then None
      else modify i v vec |> Some

  module Delete =
    let rec rootWithoutLast path node =
      match path with
      | [i] -> if i = 0
               then EmptyNode 
               else match node with        
                    | Branch ar -> let na = Array.zeroCreate i
                                   Array.blit ar 0 na 0 i
                                   Branch na
                    | Leaf _ -> failwith "Should not be a leaf"
                    | EmptyNode -> failwith "Should not be empty node"

                                   
      | (i::is) -> match node with
                   | Branch ar -> let nc = rootWithoutLast is (Array.get ar i) 
                                  let na = Array.copy ar
                                  Array.set na i nc
                                  Branch na
                   | Leaf _ -> failwith "Should not be leaf"
                   | EmptyNode -> failwith "Should not be Empty node"
      | [] -> failwith "no path"

    let indexOfLastAfterWithout vec = vec.size - 2

    let secondIsEmpty root  =    
      match root with 
           | Branch ar ->  Array.length ar > 1 && Array.get ar 1 = EmptyNode                        
           | _ -> false

    let withoutInTree vec =
      let path = Tools.pathToNode vec.size (indexOfLastAfterWithout vec)
      let newRoot = rootWithoutLast path vec.root
      let newTail = Tools.arrayFor (vec.size - 2) vec.size vec.root  vec.tail
      if Tools.levels vec.size > 1 && secondIsEmpty newRoot  //move to root without 
      then { vec with hash = -1
                      size = vec.size - 1
                      root = match newRoot with
                             | Branch ar -> Array.get ar 0
                             | _ -> failwith "There is no branch"
                      tail = newTail }
      else { vec with hash = -1
                      size = vec.size - 1
                      root = newRoot
                      tail = newTail }

    let without vector =
      let i = vector.size - 1
      match Tools.inTailIndex vector.size i with
      | true, 0 -> withoutInTree vector //
      | true, i' -> {vector with size = vector.size - 1 
                                 hash = -1
                                 tail = let a = Array.zeroCreate i'
                                        Array.blit vector.tail 0 a 0 i'
                                        a}
      | false, _ -> failwith "there is no tail"

    let pop vec = 
      if vec.size = 0 then None
      elif vec.size = 1 then Vector.EMPTY |> Some
      else without vec |> Some

  let set index element vector : 'T Vector option= 
   Modify.set index element vector
  
  let drop n vector: 'T Vector option = 
    //plz optimize, at least by dropping tail-wise
    if n < 0 then None
    elif n > vector.size then None
    else 
      let tsize = Tools.tailSize vector.size
      if n < tsize
      then {vector with hash = -1
                        size = vector.size - n
                        tail = let ar = Array.zeroCreate (tsize - n)
                               Array.blit vector.tail 0 ar 0 n
                               ar}
           |> Some
      else 
        List.fold (fun a x -> Delete.pop (a.Value)) (Some vector) [1..n]
                                         
  let pop vector : 'T Vector option = Delete.pop vector   
  
  let sub (i:int) (n:int) vector : 'T SubVector option = 
    if (i > vector.size) then None
    elif i < 0 then None
    elif n = 0 then Some <| SubVector.empty
    elif n < 0 then None
    elif i + n > vector.size then None
    else  Some { v = vector
                 i = i
                 n = n
                 forward = true 
                 hash = -1} 


  
  let cut n vector : 'T SubVector option = sub 0 n vector

  let rev vector : 'T SubVector = 
    if vector.size = 0
    then SubVector.empty
    else { v=vector
           i = 0
           n = vector.size 
           forward = false 
           hash = -1} 

  let list vector : 'T list = 
    SubVector.fold (fun a x -> x::a) [] (rev vector)
    
  let fold f a xs =
    match sub 0 (size xs) xs with
    | Some xs' -> SubVector.fold f a xs'
    | None -> a

  let map f xs = 
    fold (fun a x -> add (f x) a) Vector.EMPTY xs

  let filter p xs =
    fold (fun a x -> if p x
                        then add x a
                        else a) 
         Vector.EMPTY
         xs

  let concat tail vector = 
    //plz optimize. We could add blockvise
    fold (fun a x -> Vector.add x a) vector tail

  let ofSub (sub:'T SubVector) =
    if sub.i = 0 && sub.n = sub.v.size && sub.forward
    then sub.v
    else SubVector.fold (fun a x -> add x a) Vector.EMPTY sub
         

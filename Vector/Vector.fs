module col

type internal Node<'T> = | Leaf of 'T array
                         | Branch of Node<'T> array
                         | EmptyNode


module private Tools =   
    let inline isOutOfBounds size i = 
         i < 0 || i >=size
    
    [<Literal>]
    let BucketBits = 5
      
    let BucketSize = 1 <<< BucketBits
     
    let inline (>>>>) x y = System.Convert.ToInt32 ((uint32 x) >>> y)
    
    let inline tailStartOffset size = 
         if size < BucketSize
         then 0  
         else ((size - 1) >>>> BucketBits) <<< BucketBits
    
    let inline inTailIndex vecsize i =
        let t = tailStartOffset vecsize
        if i >= t
        then struct (true, i-t)
        else struct (false, 0)
    
    let inline levels size =
         let size = size - 1
         if size < (BucketSize*BucketBits)
         then 1
         else let mutable size = (size - BucketSize) >>>> BucketBits
              let mutable levels = 0
              while size <> 0 do
                levels <- levels + 1
                size <- size >>>> BucketBits
              levels
    
    let inline indexOfArrayAt offset = offset &&& (BucketSize - 1)
    
    let inline shift levels = levels * BucketBits 
    
    let inline indexAtLevel index level = indexOfArrayAt <| (index >>>> (shift level))
    //Make tables of these
    let inline range (from:int) (step:int) (until:int) : int list = [for i in from..step..until -> i]
    
    let inline pathToValue size index =
         List.map (indexAtLevel index) <| range (levels size) -1 0
    
    let inline pathToNode size index =
         List.map (indexAtLevel index) <| range (levels size) -1 1


    let inline leafPaths size =
      seq [0..BucketSize..size]
      |> Seq.map (pathToNode size) 
    
    let asSeq (root:'T Node) = 
      let rec traverse (stack: (int * Node<'T>) list) = 
        match stack with
        | [] -> None
        | ((i, node)::xs) -> 
               match node with
               | EmptyNode -> traverse xs   
               | Leaf ar -> if i < Array.length ar
                            then Some ((Array.get ar i), ((i + 1, node) :: xs))
                            else traverse xs
               | Branch ar -> if i < Array.length ar
                              then traverse ((0, Array.get ar i) :: ((i + 1, node) :: xs))
                              else traverse xs
      Seq.unfold traverse [(0,root)]

    let inline arrays (root:'T Node) = 
         let rec traverse (stack: (int * Node<'T>) list) = 
           match stack with
           | [] -> None
           | ((i, node)::xs) -> 
                  match node with
                  | EmptyNode -> traverse xs   
                  | Leaf ar -> Some (ar, xs)
                  | Branch ar -> if i < Array.length ar
                                 then traverse ((0, Array.get ar i) :: ((i + 1, node) :: xs))
                                 else traverse xs
         Seq.unfold traverse [(0,root)]
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
    
    let inline arrayFor index size root tail =
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
    
    let inline getInNode index size tail root =
        if isOutOfBounds size index
        then failwith "out of bounds"
        match inTailIndex size index with
        | true, v -> Array.get tail v
        | false, _ -> let path = pathToValue size index
                      valueIn path root
    
    
    let inline tailSize size = size - (tailStartOffset size)
    
    let inline isFullTail size = tailSize size >= BucketSize
    
    let inline roomInTail size = BucketSize - (tailSize size)
    
    let inline isFullRoot size levels = 
      (size >>>> BucketBits) > (1 <<< (shift levels))
    
/// A persistent immutable random access list that grows in the tail
/// Mutation is represented by logically new lists, that share data with previos, and maintain the same algoritmic complexity
/// Access is typically O log32 n
type Vector<'T when 'T : equality>  private (size:int, root : 'T Node, tail : 'T array, hash :int) = 
  let mutable hash = hash
  static let EMPTY_VECTOR =  Vector<'T>(0, EmptyNode, Array.empty, 0)
  //new (size:int, root : 'T Node, tail : 'T array ) = Vector (size, root, tail, -1)
  member internal x.size = size
  member internal x.root= root
  member internal x.tail = tail
  member internal x.withRoot root = Vector(size, root, tail, -1) 
  member internal x.withTail tail = Vector(size, root, tail, -1)
  member internal x.withTailSize tail size = Vector(size, root, tail, -1)
  member internal x.withRootTailSize root tail size  = Vector(size, root, tail, -1)
  
  // The empty vector
  static member EMPTY = EMPTY_VECTOR                       

  override x.Equals(o) = 
    match o with
      | :? Vector<'T> as y ->
           let rec calc i = 
             if i = size then true 
             else if (Tools.getInNode i size tail root) = (Tools.getInNode i y.size y.tail y.root)
                  then calc (i+1)
                  else false
           if size = y.size 
           then calc 0
           else false
      | _ -> false 
  override x.GetHashCode () =
    if hash = -1
    then let rec calc i a = 
              if i = size 
              then a.GetHashCode ()
              else calc (i + 1) ((Tools.getInNode i size tail root)::a)
         hash <- calc 0 []             
         hash
       else hash    

module private Get =
    open Tools
    let get (index:int) (vector:'T Vector) : 'T option =
      if Tools.isOutOfBounds vector.size index
      then None
      else match inTailIndex vector.size index with
            | true, v -> Array.get vector.tail v  |> Some
            | false, _ -> let path = pathToValue vector.size index
                          valueIn path vector.root  |> Some
        
    let getOrFail (index:int) (vector:'T Vector) =
      getInNode index vector.size vector.tail vector.root  
   
/// As subvector is a portion of a vector, possibly in reverse order. This is a view
type SubVector<'T when 'T : equality> private (v:Vector<'T>, i :int , n:int ,forward :bool, hash:int) = 
  let mutable hash = hash
  static let EMPTY_SUBVECTOR =  SubVector<'T>(Vector<'T>.EMPTY, 0, 0, true, 0)
  member internal x.v = v
  member internal x.i = i
  member internal x.n = n
  member internal x.forward = forward
  member internal x.withLength n = SubVector (v ,i ,n, forward, -1)
  member internal x.withIndexLenght i n = SubVector (v, i, n, forward, -1)
  member internal x.withIndex i = SubVector (v, i, n, forward, -1)
  member internal x.withForward forward = SubVector (v, i ,n, forward, -1 )
  internal new (v:'T Vector, i:int, n: int, forward:bool) = SubVector(v,i,n,forward, -1)
  /// The empty SubVector
  static member EMPTY = EMPTY_SUBVECTOR  
  
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
       if hash = -1
       then let rec calcf i a = 
              if i = x.n 
              then a.GetHashCode ()
              else calcf (i + 1) ((Get.getOrFail (i+x.i) x.v)::a)
            let rec calcb i a =
              if i = x.n
              then a.GetHashCode ()
              else calcb (i + 1) ((Get.getOrFail (x.i+x.n - i-1) x.v)::a)
            hash <- if x.forward 
                      then (calcf 0 [])
                      else (calcb 0 [])
            hash
       else hash


/// A persistent immutable random access list that grows in the tail
/// Mutation is represented by logically new lists, that share data with previos, and maintain the same algoritmic complexity
/// Access is typically O log32 n
module rec Vector =

/// The empty vector  
  let empty () = Vector<_>.EMPTY

/// element at index
  let get(index:int) (vector:'T Vector) : 'T option = 
    Get.get index vector
    
/// first element, as in the first added, position 0
  let first (vector:'T Vector) = get 0 vector
/// number of elements in the vector 
  let size (vector:_ Vector) = vector.size

/// returns a vector with attitional element in the tail 
  let add element vector = Add.add element vector
  
 /// add all elements, int the same order
  let addAll (elements:'T list) vector : 'T Vector = 
    //plz optimize
    List.fold (fun a x -> add x a) vector elements
    
/// add all elements to a new list, in the same order
  let ofList (elements:'T list) : 'T Vector = 
    Vector<'T>.EMPTY  |> addAll elements 

/// last element in the vector, most recently appended
  let last vector : 'T option = get ((size vector) - 1) vector 

/// As subvector is a portion of a vector, possibly in reverse order. This is a view
  module SubVector =

/// element at index   
    let get index (subvector:'T SubVector) : 'T option = 
      if index < subvector.n && index >= 0
      then if subvector.forward 
           then Vector.get (subvector.i + index) subvector.v 
           else Vector.get (subvector.i + subvector.n - 1 - index ) subvector.v
      else None
 /// first element, as in the first added, position 0  
    let first v= get 0 v
/// first element, ot abnormal failure if not present
    let firstOrFail (v:'T SubVector) = 
       if 0 < v.n 
       then if v.forward
            then Get.getOrFail v.i v.v
            else Get.getOrFail (v.i + v.n - 1) v.v 
       else failwith "vector is empty" 
///number of elements the subvector respresents    
    let size (subvector:'T SubVector) : int = subvector.n

/// last element in the vector, most recently appended    
    let last (subvector: 'T SubVector) : 'T option = 
      get (size subvector - 1 ) subvector

/// subvector without the n last elements
    let drop n (subvector:'T SubVector) : 'T SubVector option = 
      if subvector.n > n
      then if subvector.forward 
           then subvector.withLength (subvector.n-n) 
                |> Some       
           else subvector.withIndexLenght (subvector.i + n)
                                          (subvector.n - n)
                |> Some                            
      else None

 /// subvector without last element   
    let pop vector = drop 1 vector 

 /// subvector without the first n elements   
    let cut n (subvector:'T SubVector) : 'T SubVector option = 
      match struct (subvector.n, n, subvector.forward) with
      | _,0,_ -> Some subvector
      | 0,_,_ -> None
      | sn,n,_ when n > sn -> None
      | sn,n,_ when n = sn -> Some SubVector<'T>.EMPTY
      | _, n, true -> subvector.withIndexLenght (subvector.i + n)
                                                (subvector.n - n)
                      |> Some
      | _, n, false -> subvector.withLength (subvector.n - n)
                       |> Some
 /// A more narrow portion of the subset. n elements from i, counting from current
    let sub i n (subvector:'T SubVector) : 'T SubVector option =
      if i<0 || n<0 then None
      elif i + n > subvector.n then None
      elif n = 0 then Some SubVector.EMPTY
      elif subvector.n = 0 then None
      elif subvector.forward then Some (subvector.withIndexLenght (subvector.i + i) n)
      else Some (subvector.withIndexLenght  (subvector.n - i - n) (subvector.n - i))
 
 /// all except first, possibly empty, or abnormal failure
    let nextOrFail (v:'T SubVector) =
      if v.n = 0
      then failwith "empty subvector"
      elif v.n = 1 
      then SubVector.EMPTY
      elif v.forward
      then v.withIndexLenght (v.i + 1) (v.n - 1)
      else v.withLength (v.n - 1)
 
 /// combine result of recursivly processing each element
    let fold f a (subvector: 'T SubVector) =
      let f' = OptimizedClosures.FSharpFunc<_,_,_>.Adapt f
      //Plz, optimize by running on each Leaf
      let mutable acc = a 
      let v = subvector.v
      if subvector.forward
      then let ending = subvector.i + subvector.n - 1
           for i = subvector.i to ending do
              acc <- f'.Invoke (acc, Get.getOrFail i v)
      else for i = subvector.i+subvector.n-1 downto subvector.i do
              acc <- f'.Invoke (acc, Get.getOrFail i v)
      acc 
/// reverse view of subvector
    let rev (subvector:'T SubVector) : 'T SubVector = 
       subvector.withForward (not subvector.forward)
                   
/// fold from the back
    let foldBack f subvector a=
      fold (fun a x -> f x a) a (rev subvector)

 /// A list containing all elements in the same order
    let toList  (subvector:'T SubVector) : 'T list = 
      foldBack (fun x a -> x::a) subvector [] 

 /// combine result of recursivly processing each element until some additional value is returned 
    let foldUntil (f: 'a->'x -> struct ('a * 'm option)) (seed:'a) (vector:'x SubVector) : ('a * 'm option) =
      let rec foldav' (f' :OptimizedClosures.FSharpFunc<_,_,_>) (a:'a) (l : 'x SubVector) : struct ('a * 'm option) =
        if size l = 0 
        then struct (a, None)
        else match f'.Invoke (a, firstOrFail l) with
             | struct (a', Some m) -> struct (a', Some m)
             | struct (a', None) -> foldav' f' a' (nextOrFail l)
      match foldav' (OptimizedClosures.FSharpFunc<_,_,_>.Adapt f) seed vector with
      | struct (a,m) -> a,m
             
/// combine result of recursivly processing each element while a condition is met
    let foldWhile (f : 'a -> 'x -> struct ('a * bool)) (a:'a) (xs: 'x SubVector) : 'a =
      let rec folda' (f' : OptimizedClosures.FSharpFunc<_,_,struct ('a * bool)>) (a:'a) (xs: 'x SubVector) : 'a =
         if size xs = 0
         then a
         else match f'.Invoke (a, firstOrFail xs) with
                      | struct (a', false) -> a'
                      | struct (a', true) -> folda' f' a' (nextOrFail xs)
      folda' (OptimizedClosures.FSharpFunc<_,_, struct ('a * bool)>.Adapt f) a xs

  module internal Add =
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

    let withPushedIntoDeeperTree (vec:'T Vector) v = 
      let currentRoot = vec.root
      let newRoot = newLevelRoot currentRoot
                                 <| Tools.levels vec.size
                                 <| vec.tail
      vec.withRootTailSize newRoot (Array.singleton v) (vec.size + 1)


      
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

    let withPushedIntoEquallySizedTree (vector:'T Vector) v= 
      let size = vector.size 
      let last = vector.size - 1
      let path = Tools.pathToNode size last 
      let newNode = leafOfArray vector.tail
      let newRoot = withNodeIn path vector.root newNode
      let newTail = Array.singleton v
      vector.withRootTailSize newRoot newTail (vector.size + 1)

   

    let withPushedIntoTree (vector:'T Vector) v =
        let size = vector.size
        let levels = Tools.levels size
        if Tools.isFullRoot size levels
        then withPushedIntoDeeperTree vector v
        else withPushedIntoEquallySizedTree vector v
    
    let withPushedIntoTail (vector:'T Vector) element = 
      vector.withTailSize (Array.append vector.tail [|element|]) (vector.size + 1)

    let add element vector =
      if Tools.isFullTail vector.size
      then withPushedIntoTree vector element
      else withPushedIntoTail vector element

    let withPushedNodeIntoDeeperTree (vec:'T Vector) newTail = 
        let currentRoot = vec.root
        let newRoot = newLevelRoot currentRoot
                                   <| Tools.levels vec.size
                                   <| vec.tail
        vec.withRootTailSize newRoot newTail (vec.size + Array.length newTail)

    let withPushedNodeIntoEquallySizedTree (vector:'T Vector) newTail= 
            let size = vector.size 
            let last = vector.size - 1
            let path = Tools.pathToNode size last 
            let newNode = leafOfArray vector.tail
            let newRoot = withNodeIn path vector.root newNode
            vector.withRootTailSize newRoot newTail (vector.size + Array.length newTail)

    let withPushedNodeIntoTree (vector:'T Vector) newTail =
        let size = vector.size
        let levels = Tools.levels size
        if Tools.isFullRoot size levels
        then withPushedNodeIntoDeeperTree vector newTail
        else withPushedNodeIntoEquallySizedTree vector newTail

    let addBucket array (vector: 'T Vector) =
      
       if  (Array.length array > Tools.BucketSize)
       then failwith "trying to add more than a bucket"
       elif vector.size = 0
       then vector.withTailSize array (Array.length array)
       elif not (Tools.isFullTail vector.size)
             then failwith "not full tail"
       else
         withPushedNodeIntoTree vector array

  module private Modify =
    let inline isAtEnd i (vec:'T Vector) = vec.size = i
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
    
    let modify i value (vector:'T Vector) =  
      match Tools.inTailIndex vector.size i with
      | true, i' -> vector.withTail ( let a = Array.copy vector.tail
                                      Array.set a i' value
                                      a)
      | false,_ -> let path = Tools.pathToValue vector.size i
                   vector.withRoot (nodeWith path vector.root value)
    let set i v vec =
      if isAtEnd i vec then Add.add v vec |> Some
      elif Tools.isOutOfBounds vec.size i then None
      else modify i v vec |> Some

  module private Delete =
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

    let indexOfLastAfterWithout (vec:'T Vector) = vec.size - 2

    let secondIsEmpty root  =    
      match root with 
           | Branch ar ->  Array.length ar > 1 && Array.get ar 1 = EmptyNode                        
           | _ -> false

    let withoutInTree (vec:'T Vector) =
      let path = Tools.pathToNode vec.size (indexOfLastAfterWithout vec)
      let newRoot = rootWithoutLast path vec.root
      let newTail = Tools.arrayFor (vec.size - 2) vec.size vec.root vec.tail
      if Tools.levels vec.size > 1 && secondIsEmpty newRoot 
      then let nextRoot = match newRoot with
                          | Branch ar -> Array.get ar 0
                          | _ -> failwith "There is no branch"
           vec.withRootTailSize nextRoot
                                newTail
                                (vec.size - 1)
      else vec.withRootTailSize newRoot newTail (vec.size - 1)

    let without (vector:'T Vector) =
      let i = vector.size - 1
      match Tools.inTailIndex vector.size i with
      | true, 0 -> withoutInTree vector //
      | true, i' -> let newTail = let a = Array.zeroCreate i'
                                  Array.blit vector.tail 0 a 0 i'
                                  a
                    vector.withTailSize newTail (vector.size - 1) 
      | false, _ -> failwith "there is no tail"

    let pop (vec:'T Vector) = 
      if vec.size = 0 then None
      elif vec.size = 1 then Vector.EMPTY |> Some
      else without vec |> Some

/// vector with with a element set at index. Index at tail means added element
  let set index element vector : 'T Vector option= 
   Modify.set index element vector
  
/// vector without n last elements 
  let drop n (vector:'T Vector): 'T Vector option = 
    //plz optimize, at least by dropping tail-wise
    if n < 0 then None
    elif n > vector.size then None
    else 
      let tsize = Tools.tailSize vector.size
      if n < tsize
      then let newTail = let ar = Array.zeroCreate (tsize - n)
                         Array.blit vector.tail 0 ar 0 n
                         ar
           vector.withTailSize newTail (vector.size - 1)
           |> Some
      else 
        List.fold (fun a x -> Delete.pop (a.Value)) (Some vector) [1..n]

/// vector without the last element                                         
  let pop vector : 'T Vector option = Delete.pop vector   
/// subvector respresented by n element from index  
  let sub index n (vector:'T Vector) : 'T SubVector option = 
    if (index > vector.size) then None
    elif index < 0 then None
    elif n = 0 then Some <| SubVector.EMPTY
    elif n < 0 then None
    elif index + n > vector.size then None
    else Some ( SubVector (vector, index, n, true))
    
/// subvector represented by all but n first elements 
  let cut n vector : 'T SubVector option = sub 0 n vector

/// subvector as reverse of vector
  let rev (vector: 'T Vector) : 'T SubVector = 
    if vector.size = 0
    then SubVector.EMPTY
    else SubVector (vector, 0, vector.size, false)

/// list of all elements in same order
  let toList vector : 'T list = 
    SubVector.fold (fun a x -> x::a) [] (rev vector)

/// combine result of recursivly processing each element  
  let fold f a xs =
    match sub 0 (size xs) xs with
    | Some xs' -> SubVector.fold f a xs'
    | None -> a

/// combine result of recursivly processing each element until some additional value is returned 
  let foldUntil f s v =
    match sub 0 (size v) v with
    | Some xs -> SubVector.foldUntil f s xs
    | None -> (s, None)

/// combine result of recursivly processing each element while a condition is met
  let foldWhile f s v =
    match sub 0 (size v) v with
    | Some xs -> SubVector.foldWhile f s xs
    | None -> s

/// vector with f applied to each
  let map f xs = 
    fold (fun a x -> add (f x) a) Vector.EMPTY xs

/// vector with all matching p
  let filter p xs =
    fold (fun a x -> if p x
                        then add x a
                        else a) 
         Vector.EMPTY
         xs

  
  let private transferBlocks src dest = 
      Seq.fold (fun a x -> Add.addBucket x a) dest (arrays src)

  let private appendToTail src dest = 
      fold (fun a x -> add x a) dest src

  let private transferSubBlocks (src:'T Vector) (dest:'T Vector) = 
      let available = Tools.roomInTail dest.size
      let arrays = Tools.arrays src.root 
      if Seq.isEmpty arrays
      then Array.fold (fun a v -> add v a) dest src.tail
      else 
           let headArray = Seq.head arrays
           let tailArrays = Seq.tail arrays
           
           let (s:int seq) = seq {0..(available-1)}
           let withFullTail = Seq.fold (fun a v -> add (Array.get headArray v ) a) dest s
           let remainSize = (Tools.BucketSize - available )
           let fillSize = Tools.BucketSize - remainSize
           let remaining = Array.zeroCreate remainSize
           Array.blit headArray available remaining 0  remainSize
           let folding (dest,remain) x = 
                                       
                                         if (Array.length x) = Tools.BucketSize
                                         then let toAdd = Array.zeroCreate Tools.BucketSize
                                              Array.blit remain 0 toAdd 0 remainSize
                                              Array.blit x 0 toAdd remainSize fillSize
                                              Array.blit x  fillSize remain 0 remainSize
                                              (Add.addBucket toAdd dest), remain
                                         elif Array.length x <= fillSize 
                                         then let newRemainSize = remainSize + Array.length x
                                               
                                              let newRemain = Array.zeroCreate newRemainSize
           
                                              Array.blit remain 0 newRemain 0 remainSize
                                              Array.blit x 0 newRemain remainSize (Array.length x)
                                              dest, newRemain
                                         else 
                                              let toAdd = Array.zeroCreate Tools.BucketSize
                                              Array.blit remain 0 toAdd 0 remainSize
                                              Array.blit x 0 toAdd remainSize fillSize
           
                                              let newRemainSize = (Array.length x) - fillSize
                                              let newRemain = Array.zeroCreate newRemainSize
                                              Array.blit x fillSize newRemain 0 newRemainSize
                                              (Add.addBucket toAdd dest), newRemain
                                              
           let solution, finalCut = Seq.fold folding (withFullTail,remaining) tailArrays
           
           Array.fold (fun a v -> add v a) solution (Array.append finalCut src.tail)
       // | [x] -> Array.fold (fun a v -> add v a) dest x

  let duration s f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    timer.Stop ()
    let i = timer.ElapsedMilliseconds
    if i > 0L 
    then printfn "Elapsed Time: %s %i" s timer.ElapsedMilliseconds

    returnValue    

  let oldAppend = 
    appendToTail  
/// vector with tail appended 
  let append (tail:'T Vector) (vector:'T Vector) = 
    if size vector = 0
    then tail
    elif Tools.isFullTail vector.size 
    then transferBlocks tail vector
    elif tail.size >= Tools.roomInTail vector.size 
    then transferSubBlocks tail vector
    else appendToTail tail vector
    
/// A real vector of all elements found in a subvector
  let ofSub (sub:'T SubVector) =
    if sub.i = 0 && sub.n = sub.v.size && sub.forward
    then sub.v
    else SubVector.fold (fun a x -> add x a) Vector.EMPTY sub

/// flattened vector
  let concat (vecOfvec: 'T Vector Vector) :  'T Vector = 
    fold (fun a v -> append v a) Vector.EMPTY vecOfvec

/// The monadic map-cat:er
  let bind (f:'T -> 'V Vector)  (v : 'T Vector) : 'V Vector = 
    //Better append each in a fold?
    map f v |> concat

// experiments 
  let leafpaths s = Tools.leafPaths s |> Seq.toList
  let sequence (v:'T Vector) = Seq.append (Tools.asSeq v.root |> Seq.toList)
                                          (Array.toSeq v.tail)

  let arrays (v) = Seq.append (Tools.arrays v.root)
                              (Seq.singleton v.tail)
 

         

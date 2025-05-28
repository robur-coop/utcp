type t =
  | Empty
  | Node of node
and node =
  { x : int
  ; y : int
  ; l : t
  ; r : t
  ; h : int }

let height = function
  | Empty -> 0
  | Node n -> n.h

let empty = Empty

let is_empty = function
  | Empty -> true
  | _ -> false

let rec node x y l r =
  let hl = height l and hr = height r in
  if hl > hr + 2
  then
    let[@warning "-8"] Node { x= lx; y= ly; l= ll; r= lr; _ } = l in
    if height ll >= height lr
    then node lx ly ll (node x y lr r)
    else
      let[@warning "-8"] Node { x= lrx; y= lry; l= lrl; r= lrr; _ } = lr in
      node lrx lry (node lx ly ll lrl) (node x y lrr r)
  else if hr > hl + 2
  then
    let[@warning "-8"] Node { x= rx; y= ry; l= rl; r= rr; _ } = r in
    if height rr >= height rl
    then node rx ry (node x y l rl) rr
    else
      let[@warning "-8"] Node { x= rlx; y= rly; l= rll; r= rlr; _ } = rl in
      node rlx rly (node x y l rll) (node rx ry rlr rr)
  else
    let h = Int.max (height l) (height r) + 1 in
    Node { x; y; l; r; h }

let rec split_max = function
  | { x; y; l; r= Empty; _ } -> x, y, l
  | { r= Node r; _ } as n ->
      let u, v, r' = split_max r in
      u, v, node n.x n.y n.l r'

(*
let rec split_min = function
  | { x; y; l= Empty; r; _ } -> x, y, r
  | { l= Node l; _ } as n ->
      let u, v, l' = split_min l in
      u, v, node n.x n.y l' n.r

let add_left = function
  | { l= Empty; _ } as n -> n
  | { l= Node l; _ } as n ->
      let x', y', l' = split_max l in
      if y' + 1 == n.x
      then { n with x= x'; l= l' }
      else n

let add_right = function
  | { r= Empty; _ } as n -> n
  | { r= Node r; _ } as n ->
      let x', y', r' = split_min r in
      if n.y + 1 == x'
      then { n with y= y'; r= r' }
      else n
*)

exception Overlap

let () = Printexc.register_printer @@ function
  | Overlap -> Some "Fragment overlap"
  | _ -> None

let rec add (x, y) t =
  match t with
  | Empty -> node x y Empty Empty
  | Node n when y < n.x ->
      let l = add (x, y) n.l in
      node n.x n.y l n.r
  | Node n when n.y < x ->
      let r = add (x, y) n.r in
      node n.x n.y n.l r
  | _ -> raise_notrace Overlap

let add ~off ~len t =
  let x = off and y = off + len - 1 in
  add (x, y) t

let merge l r = match l, r with
  | l, Empty -> l
  | Empty, r -> r
  | Node l, r ->
      let x, y, l' = split_max l in
      node x y l' r

let rec remove (x, y) t =
  match t with
  | Empty -> Empty
  (* completely to the left *)
  | Node n when y < n.x ->
      let l = remove (x, y) n.l in
      node n.x n.y l n.r
  (* completely to the right *)
  | Node n when n.y < x ->
      let r = remove (x, y) n.r in
      node n.x n.y n.l r
  (* overlap on the left only *)
  | Node n when x < n.x && y < n.y ->
      let n' = node (y+1) n.y n.l n.r in
      remove (x, n.x-1) n'
  (* overlap on the right only *)
  | Node n when y > n.y && x > n.x ->
      let n' = node n.x (x-1) n.l n.r in
      remove (n.y+1, y) n'
  (* overlap on both side *)
  | Node n when x <= n.x && y >= n.y ->
      let l = remove (x, n.x) n.l in
      let r = remove (n.y, y) n.r in
      merge l r
  (* completely within *)
  | Node n when y == n.y ->
      node n.x (x-1) n.l n.r
  | Node n when x == n.x ->
      node (y+1) n.y n.l n.r
  | Node n ->
      assert (n.x <= x-1);
      assert (y+1 <= n.y);
      let r = node (y+1) n.y Empty n.r in
      node n.x (x-1) n.l r

let rec fold fn t acc = match t with
  | Empty -> acc
  | Node n ->
      let acc = fold fn n.l acc in
      let acc = fn (n.x, n.y) acc in
      fold fn n.r acc

let diff a b = fold remove a b

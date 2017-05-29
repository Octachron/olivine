module type dim = sig val value:int end

module type array = sig

  type 'a t
  val length: float t -> int
  val get: float t -> int -> float
  val set : float t -> int -> float -> unit
  val zeroes: int -> float t
  val copy: float t -> float t
  val init : int -> (int -> float) -> float t
  val map: (float -> float) -> float t -> float t
  val map2: (float -> float -> float) -> float t -> float t -> float t

end

module Scalar = struct
  let (+) = (+.)
  let ( * )= ( *. )
  let ( - ) = (-.)
  let ( / ) = (/.)
  let (~-) = (~-.)
end

module type vec = sig
  val dim: int
  type data
  type rank = [`vec|`matrix]
  type 'a t constraint 'a = [< rank]
  type matrix = [`matrix] t
  type vec = [`vec] t

  val pp: Format.formatter -> 'a t -> unit
  val zero: ([<rank] as 'r) -> 'r t
  val id: matrix
  val vec: (int -> float) -> vec
  val matrix: (int -> int -> float) -> matrix
  val data: 'a t -> data
  val blit_to: from:'a t -> to':data -> unit

  module Bigarray : sig
    module Array1: sig
      val get: vec -> int -> float
      val set: vec -> int -> float -> unit
    end
    module Array2: sig
      val get: [`matrix] t -> int -> int -> float
      val set: [`matrix] t -> int -> int -> float -> unit
    end
  end

  type 'a op = ([<rank] as 'a) t -> 'a t -> 'a t
  val (+): 'a op
  val (-): 'a op
  val ( *. ): float -> 'a t -> 'a t
  val ( /. ): 'a t -> float -> 'a t
  val ( ~-): 'a t -> 'a t

  val ( * ): [`matrix] t -> 'a t -> 'a t

  val (|*|): 'a t -> 'a t -> float

  val axis_rotation: int -> int -> float -> [`matrix] t

end

module Make(Array:array)(D:dim): vec with type data = float Array.t = struct
  let dim = D.value
  let dim' = dim - 1
  type rank = [`vec|`matrix]
  type data = float Array.t
  type 'a t = float Array.t constraint 'a = [< rank]
  type matrix = [`matrix] t
  type vec = [`vec] t

  let pp ppf v =
    let fp x = Format.fprintf ppf x in
    fp "@[<v>[";
    for i = 0 to Array.length v / dim - 1 do
      fp "@[<h>";
      for j = 0 to dim' do
        fp " %f," v.(j + dim * i)
      done;
        fp "@]@;";
      done;
    fp "]@]"

  let zero = function
    | `vec -> Array.zeroes dim
    | `matrix -> Array.zeroes (dim*dim)

  let data x = x
  let blit_to ~from:v ~to':a =
    for i = 0 to Array.length v - 1 do
      a.(i) <- v.(i)
    done

  module Bigarray = struct
    module Array1 = struct
      let get (v:vec) i=
        v.(i)
      let set (v:vec) i x =
        v.(i) <- x
    end
    module Array2 = struct
      let get (m:[`matrix] t) i j=
        m.(i + j * dim)
      let set (m:[`matrix] t) i j x =
        m.(i + j * dim) <- x
    end
  end


  let vec f = Array.init dim f
  let matrix f =
    let m = zero `matrix in
    for i = 0 to dim' do
      for j = 0 to dim' do
        m.{i,j} <- f i j
      done
    done;
    m

  let id = matrix (fun i j -> if i=j then 1. else 0.)

  type 'a op = ([<rank] as 'a) t -> 'a t -> 'a t

  let matrix_mult m n =
    matrix (fun i j ->
        let s = ref 0. in
      for k=0 to dim' do
          s := !s +. m.{i,k} *. n.{k,j}
        done;
      !s
      )

  let matrix_apply m v =
    vec (fun i ->
        let s = ref 0. in
        for k = 0 to dim' do
          s:= !s +. m.{i,k} *. v.{k}
        done; !s
      )

  let ( * ): 'a. matrix -> ([<rank] as 'a) t -> 'a t =
    fun m t ->
    if Array.length t = dim then
      matrix_apply m t
    else
      matrix_mult m t

  let ( |*| ) v w =
    let s = ref 0. in
    for i = 0 to Array.length v - 1 do
      s:= !s +. v.(i) *. w.(i)
    done;
    !s

  let norm2 v = (v |*| v)
  let norm v = sqrt @@ norm2 v

  let (^) v w =
    matrix (fun i j ->
        v.{i} *. w.{j} -. w.{i} *. v.{i}
      )

  let (+) = Array.map2 (+.)
  let (-) = Array.map2 (-.)
  let ( *. ) l = Array.map ( ( *. ) l)
  let ( /. ) a l = Array.map (fun x -> x /. l) a
  let (~-) = Array.map (~-.)

  let orthon2 x y =
    let x = x /. norm x in
    x, y - (x|*|y) *. x

  let frot x y theta v =
    let sin = sin theta and cos = cos theta in
    let x, y = orthon2 x y in
    let vx = (x|*|v) and vy = (y|*|v) in
    let dx = Scalar.( (cos - 1.) * vx - sin * vy )
    and dy = Scalar.( (cos - 1.) * vy + sin * vx ) in
    v + dx *. x + dy *. y

  let axis_rotation k l theta =
    let sin = sin theta and cos = cos theta in
    let m = Array.copy id in
    m.{k,k} <- cos; m.{l,k} <- sin;
    m.{k,l} <- -.sin; m.{l,l} <- cos;
    m

 end

module Affine(V:vec) = struct
  type t = { m: V.matrix; t: V.vec }
  let ( * ) a b =
    { m = V.( a.m * b.m);
      t = V.( a.t + a.m * b.t )
    }

end

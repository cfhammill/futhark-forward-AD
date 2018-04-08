module Dual(T: real): {
  type r = T.t
  type t = (r, r)
  val inject: r -> t
  val set_dual: t -> r -> t
  val get_dual: t -> r
  val make_dual: r -> r -> t

  -- from_prim
  val i8: i8 -> t
  val i16: i16 -> t
  val i32: i32 -> t
  val i64: i64 -> t
  val u8: u8 -> t
  val u16: u16 -> t
  val u32: u32 -> t
  val u64: u64 -> t
  val f32: f32 -> t
  val f64: f64 -> t
  val bool: bool -> t

  -- numeric
  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t
  val **: t -> t -> t

  val ==: t -> t -> bool
  val <: t -> t -> bool
  val >: t -> t -> bool
  val <=: t -> t -> bool
  val >=: t -> t -> bool
  val !=: t -> t -> bool

  val negate: t-> t
  val max: t -> t -> t
  val min: t -> t -> t

  val abs: t -> t

  val sgn: t -> t

  val largest: t
  val smallest: t

  -- | Returns zero on empty input.
  val sum: []t -> t

  -- | Returns one on empty input.
  val product: []t -> t

  -- | Returns `smallest` on empty input.
  val maximum: []t -> t
  -- | Returns `largest` on empty input.
  val minimum: []t -> t

  -- real
  val from_fraction: i32 -> i32 -> t
  val to_i32: t -> i32
  val to_i64: t -> i64
  val to_f64: t -> f64

  val sqrt: t -> t
  val exp: t -> t
  val cos: t -> t
  val sin: t -> t
  val asin: t -> t
  val acos: t -> t
  val atan: t -> t
  val atan2: t -> t -> t

  val log: t -> t

  val ceil : t -> t
  val floor : t -> t
  val trunc : t -> t
  val round : t -> t

  val isinf: t -> bool
  val isnan: t -> bool

  val inf: t
  val nan: t

  val pi: t
  val e: t
  
} = {
  type r = T.t
  type t = (r, r)
  let inject (x : r) : t = (x, T.i32 0)
  let i8 (x : i8) = inject (T.i8 x)
  let i16 (x : i16) = inject (T.i16 x)
  let i32 (x : i32) = inject (T.i32 x)
  let i64 (x : i64) = inject (T.i64 x)
  let f32 (x : f32) = inject (T.f32 x)
  let f64 (x : f64) : t = inject (T.f64 x)
  let u8 (x : u8) = inject (T.u8 x)
  let u16 x = inject (T.u16 x)
  let u32 x = inject (T.u32 x)
  let u64 x = inject (T.u64 x)
  let bool x = inject (T.bool x)

  let (x,x') + (y,y') = (x T.+ y, x' T.+ y') 
  let (x,x') - (y,y') = (x T.- y, x' T.- y') 
  let (x,x') * (y,y') = (x T.* y, x' T.* y T.+ x T.* y') 

  let (x,x') / (y,y') =
    (x T./ y
    , (x' T.* y T.- x T.* y') T./ y T.** (T.i32 2))
  
  let (x,x') ** (y,y') =
    (x T./ y
    , (x' T.* y T.- x T.* y') T./ y T.** (T.i32 2))
  
  let (x,_) == (y,_) = x T.== y
  let (x,_) < (y,_) = x T.< y
  let (x,_) > (y,_) = x T.> y
  let (x,_) <= (y,_) = x T.<= y
  let (x,_) >= (y,_) = x T.>= y
  let (x,_) != (y,_) = x T.!= y
  let negate (x,x') = (T.negate x, T.negate x')
  let max x y = if x >= y then x else y
  let min x y = if x <= y then x else y
  let abs (x,x') = (T.abs x, x')
  let sgn (x,x') = (T.sgn x, x')
  let largest = inject T.largest
  let smallest = inject T.smallest
  -- | Returns zero on empty input.
  let sum = reduce (+) (inject (T.i32 0)) 
  -- | Returns one on empty input.
  let product = reduce (*) (inject (T.i32 1))
  -- | Returns `smallest` on empty input.
  let maximum = reduce min largest
  -- | Returns `largest` on empty input.
  let minimum = reduce max smallest


  -- val from_fraction: i32 -> i32 -> t
  let from_fraction x y = inject (T.from_fraction x y)
  -- val to_i32: t -> i32
  let to_i32 (x,_) = T.to_i32 x
  let to_i64 (x,_) = T.to_i64 x
  let to_f64 (x,_) = T.to_f64 x


  -- val sqrt: t -> t
  let sqrt (x,x') = (T.sqrt x, x' T./ (T.i32 2 T.* T.sqrt x))
  -- val exp: t -> t
  let exp (x,x') = (T.exp x, x' T.* T.exp x)
  -- val cos: t -> t
  let cos (x, x') = (T.cos x, T.negate x' T.* T.sin x)
  -- val sin: t -> t
  let sin (x, x') = (T.sin x, x' T.* T.cos x)
  -- val asin: t -> t
  let asin (x, x') = (T.asin x, x' T./ (T.sqrt (T.i32 1 T.- x T.** T.i32 2)))
  -- val acos: t -> t1
  let acos (x, x') =
    (T.acos x, T.negate x' T./ (T.sqrt (T.i32 1 T.- x T.** T.i32 2))) 
  -- val atan: t -> t
  let atan (x, x') = (T.atan x, x' T./ (T.i32 1 T.+ x T.** T.i32 2))
  -- val atan2: t -> t -> t
  -- I know this isn't right but can't figure it out now
  let atan2 (x,_) (y,_) = inject (T.atan2 x y)

  -- val log: t -> t
  let log (x, x') = (T.log x, x' T./ x)

  -- val ceil : t -> t
  let ceil (x, x') = (T.ceil x, x')
  -- val floor : t -> t
  let floor (x, x') = (T.floor x, x')
  -- val trunc : t -> t
  let trunc (x, x') = (T.trunc x, x')
  -- val round : t -> t
  let round (x, x') = (T.round x, x')

  -- val isinf: t -> bool
  let isinf (x,_) = T.isinf x
  -- val isnan: t -> bool
  let isnan (x,_) = T.isnan x

  -- val inf: t
  let inf = inject T.inf
  -- val nan: t
  let nan = inject T.nan

  -- val pi: t
  let pi = inject T.pi
  -- val e: t
  let e = inject T.e

  let get_dual (_,x') = x'
  let set_dual ((x,_) : t) (x' : r) : t = (x,x')
  let make_dual (x : r) (x' : r) = (x,x')
}

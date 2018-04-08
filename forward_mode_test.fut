import "/futlib/linalg"
import "forward_mode_futhark"

module d = Dual f64
module l = linalg d

let main [n] (A : [n]f64): f64 =
  let ds = map d.f64 A
  let ds' = ds with [0] <- d.set_dual ds[0] (f64.i32 1)
  let (ones : []d.t) = [d.i32 1, d.i32 1, d.i32 1]
  let prod = l.dotprod ds' ones 
  let (_,x') = prod in
    x'

  
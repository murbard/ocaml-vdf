open Z

type quadratic  = t * t * t

let four = of_int 4
let two = of_int 2

(** Solve ax = b mod m for x.
    Return s, t whrere x = s + k * t yields all solution for integer k
*)

let solve_mod a b m =
  if a = zero && erem b m = zero then
    (zero, one)
  else begin
    assert ( m > zero) ;
    let (g, d, _) = gcdext a m in
    let (q, r) = ediv_rem b g in
    if r != zero then
      let error =  Format.sprintf "No solutions to %s x = %s mod %s" (to_string a) (to_string b) (to_string m) in
      raise (Invalid_argument error)
    else
      assert (b = q * g) ;
    (erem (q * d ) m, ediv m g)
  end

let of_coefs a b c =
  (a, b, c)

let normalize x =
  let (a, b, c) = x in
  if -a < b && b <= a then
    x
  else
    let r = ediv (a - b) (two * a) in
    let b2 = b + two * r * a in
    let c2 = a * r * r + b * r + c in
    (a, b2, c2)

let reduce x =
  let x = ref (normalize x) in
  while
    let (a, b, c) = !x in a > c || (a = c && b < zero) do
    let (a, b, c) = !x in
    let s = ediv (c + b) (c + c) in
    x:= (
        c, -b + two * s * c,
        c * s * s - b * s + a) ;
  done; normalize !x

let discriminant (a, b, c) =
  b * b - four * a * c

let from_ab_discriminant a b d =
  assert (d < zero) ;
  assert (erem d four = one) ;
  let c = ediv (b * b - d) (four * a) in

  let p = reduce (a, b, c) in
  assert (discriminant p = d); p


let identity_for_discriminant d =
  from_ab_discriminant one one d


let inverse (a, b, c) =
  (a, -b, c)

let gcd_with_zero a b =
  if a = zero then b
  else if b = zero then a
  else gcd a b

let multiply x y =
  let (a1, b1, c1) = reduce x
  and (a2, b2, _) = reduce y in

  let g = ediv (b2 + b1) two
  and h = ediv (b2 - b1) two in
  let w = (gcd_with_zero (gcd_with_zero a1 a2) g) in

  let j = w
  and r = zero
  and s = ediv a1 w
  and t = ediv a2 w
  and u = ediv g w in

  let  k_temp, constant_factor = solve_mod (t * u) (h * u + s * c1) (s * t) in

  let  n, _ = solve_mod  (t * constant_factor) (h - t * k_temp) s in

  let  k = k_temp + constant_factor * n in
  let  l = ediv (t * k - h) s in

  let m = ediv (t * u * k - h * u - s * c1) (s * t) in
  let a3 = s * t - r * u
  and b3 = (j * u + m * r) - (k * t + l * s)
  and c3 = (k * l - j * m)

  in reduce (a3, b3, c3)

let pp (a, b, c) =
  (to_string a) ^ ", " ^ (to_string b) ^ ", " ^ (to_string c)

let square x =
  let a1, b1, c1 = reduce x in
  let g = b1 in
  let h = zero in
  let w = gcd_with_zero a1 g in
  let j = w in
  let r = zero in
  let s = ediv a1 w in
  let t = s in
  let u = ediv g w in
  let k_temp, constant_factor = solve_mod (t * u) (h * u + s * c1) (s * t) in
  let n, _ = solve_mod (t * constant_factor) (h - t * k_temp) s in
  let k = k_temp + constant_factor * n in
  let m = ediv (t * u * k - h * u - s * c1) (s * t) in
  assert (m * s * t = t * u * k - h * u - s * c1) ;
  let l = ediv (t * m + c1) u in
  assert (u * l = t * m + c1) ;
  assert (erem (t * u * k - h * u - s * c1) (s * t) = zero) ;
  let a3 = s * t - r * u
  and b3 = (j * u + m * r) - (k * t + l * s)
  and c3 = k * l - j * m in
  reduce (a3, b3, c3)

let rec pow_ x n =
  if n = zero then
    identity_for_discriminant (discriminant x)
  else begin
    let y = square (pow_ x (ediv n two)) in
    if n mod two = zero then
      y
    else
      multiply y x
  end

let (^) = pow_
let ( * ) = multiply

let (=) x y  =
  (reduce x) = (reduce y)

type t = quadratic

(*
Copyright 2018 Chia Network Inc
Modifications Copyright 2018 Arthur Breitman
 - ported Python code to OCaml


Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)



let classgroup x y z = Classgroup.of_coefs (Z.of_int x) (Z.of_int y) (Z.of_int z)

let assert_cg_equal x y = assert Classgroup.(x = y)

let test_new_failure () =
  let t2_1_6 = classgroup 2 1 6
  and t4_1_3 = classgroup 4 1 3 in
  let p = Classgroup.(t2_1_6  * t4_1_3) in
  assert_cg_equal p (classgroup 3 1 4)

let test_new_failure1 () =
  let t6_5_7 = classgroup 6 5 7
  and t2_1_18 = classgroup 2 1 18 in
  let p = Classgroup.(t6_5_7 * t2_1_18) in
  assert_cg_equal p (classgroup 4 (-1) 9)

let test_paper_check () =
  let t12_11_3 = classgroup 12 11 3
  and t93_109_32 = classgroup 93 109 32
  and d = Z.of_int (-23) in

  assert (Classgroup.discriminant t12_11_3 = d);
  assert (Classgroup.discriminant t93_109_32 = d) ;

  let t = Classgroup.(t12_11_3 * t93_109_32) in
  assert_cg_equal t (classgroup 1 (-15) 62) ;
  assert (Classgroup.discriminant t = d) ;

  let t1 = Classgroup.(t93_109_32 * t12_11_3) in
  assert_cg_equal t t1;

  let f = classgroup 195751 1212121 1876411 in
  assert_cg_equal (Classgroup.normalize f) (classgroup 195751 37615 1807) ;
  assert_cg_equal (Classgroup.reduce f) (classgroup 1 1 1)

let test_generator_element () =
  let d = Z.of_int (-103) in
  let e_id = Classgroup.identity_for_discriminant d in
  assert (Classgroup.discriminant e_id = d) ;
  assert_cg_equal e_id (classgroup 1 1 26) ;
  let e = Classgroup.from_ab_discriminant (Z.of_int 2) Z.one d in
  assert (Classgroup.discriminant e = d) ;
  assert_cg_equal e (classgroup 2 1 13) ;
  let e_inv = Classgroup.inverse e in
  assert (Classgroup.discriminant e_inv = d) ;
  assert_cg_equal e_inv (classgroup 2 (-1) 13) ;
  assert_cg_equal Classgroup.(e * e_inv) e_id ;
  let e2 = Classgroup.square e in
  assert_cg_equal e2 (classgroup 4 (-3) 7) ;
  assert_cg_equal e2 Classgroup.(e * e) ;
  let e4 = Classgroup.square e2 in
  assert_cg_equal e4 (classgroup 13 1 2) ;
  assert_cg_equal e4 Classgroup.(e2 * e2)


let test_many_generators () =

  let all_powers e =
    let d = Classgroup.discriminant e in
    let e0 = ref e in
    let items = ref [] in
    while not (List.exists (fun x -> Classgroup.(x = !e0)) !items) do
      items := !e0::!items ;
      assert_cg_equal Classgroup.(!e0 * !e0) Classgroup.(square !e0) ;
      e0 := Classgroup.(!e0 * e) ;
      e0 := Classgroup.normalize !e0 ;
      assert (Classgroup.discriminant !e0 = d);
    done ;
    !items
  in
  for i = 0 to 999 do
    let d = Z.of_int (-7 - i * 8) in
    let e_id = Classgroup.identity_for_discriminant d in
    assert_cg_equal e_id (classgroup 1 1 (2 + 2 * i)) ;
    let e_id_inv = Classgroup.inverse e_id in
    assert_cg_equal e_id e_id_inv ;
    let e0 = Classgroup.from_ab_discriminant (Z.of_int 2) Z.one d in
    let e1 = Classgroup.inverse e0 in
    let p0 = all_powers e0 in
    let p1 = all_powers e1 in
    assert ((List.length p0) = (List.length p1)) ;
    assert_cg_equal e_id (List.hd p0) ;
    assert_cg_equal e_id (List.hd p1) ;
    for j = 0 to (List.length p0)-1 do
      let q = Classgroup.((List.nth p0 j) * (List.nth p1 j)) in
      assert_cg_equal q e_id ;
    done ;
    Printf.printf "discriminant = %d; group order = %d" (Z.to_int d) (List.length p0)
  done

let test_identity () =
  for i = 0 to 999 do
    let d = Z.of_int (-7 - i * 8) in
    let e_id = Classgroup.identity_for_discriminant d in
    let e_prod = Classgroup.(e_id * e_id) in
    assert_cg_equal e_prod e_id
  done

let test_inverse () =
  for i = 0 to 999 do
    let d = Z.of_int (-7 - i * 8) in
    let e_id = Classgroup.identity_for_discriminant d in
    let e_gen = Classgroup.from_ab_discriminant (Z.of_int 2) Z.one d in
    let e_gen_inv = Classgroup.inverse e_gen in
    let e_prod =Classgroup.(e_gen * e_gen_inv) in
    assert_cg_equal e_prod e_id
  done

let test_bad_multiply () =
  let n1 = Classgroup.of_coefs
      (Z.of_string "2243390248") (Z.of_string "-565721959") (Z.of_string "35664920")
  and n2 = classgroup 2 1 370 in
  let n = Classgroup.(n1 * n2) in
  assert (Classgroup.discriminant n = Classgroup.discriminant n1) ;
  assert (Classgroup.discriminant n = Classgroup.discriminant n2)

let test_pow () =
  for i = 0 to 99 do
    let d = Z.of_int ( -7 - i * 8 ) in
    let e_gen = Classgroup.from_ab_discriminant (Z.of_int 2) Z.one d in
    let p = ref (Classgroup.identity_for_discriminant d)in
    for j = 0 to 9 do
      assert_cg_equal Classgroup.(e_gen ^ (Z.of_int j)) !p ;
      p := Classgroup.(!p * e_gen);
    done
  done

let tests = [
  "test_new_failure", `Quick, test_new_failure ;
  "test_new_failure1", `Quick, test_new_failure1 ;
  "test_paper_check", `Quick, test_paper_check ;
  "test_generator_element", `Quick, test_generator_element ;
  "test_many_generators", `Quick, test_many_generators ;
  "test_inverse", `Quick, test_inverse ;
  "test_identity", `Quick, test_identity ;
  "test_bad_multiply", `Quick, test_bad_multiply ;
  "test_pow", `Quick, test_pow ;
]

let () =
  Alcotest.run "test-vdf" [
    "classgroup", tests
  ]


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

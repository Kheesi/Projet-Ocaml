type element = int

let random_element = Random.int

let string_of_element = string_of_int

let comp = compare

let ( $=$ ) x y = comp x y = 0

let ( $>$ ) x y = comp x y > 0

let ( $>=$ ) x y = comp x y >= 0

let ( $<$ ) x y = comp x y < 0

let ( $<=$ ) x y = comp x y <= 0

type inter = Vide | Inter of (element * element * bool * bool)

let inter x y b_x b_y = if x $>$ y then None else Some (Inter (x, y, b_x, b_y))

let inter_to_string = function
  | Inter (e1, e2, b1, b2) when b1 && b2 ->
      "[" ^ string_of_element e1 ^ ", " ^ string_of_element e2 ^ "]"
  | Inter (e1, e2, b1, b2) when b1 && not b2 ->
      "[" ^ string_of_element e1 ^ ", " ^ string_of_element e2 ^ "["
  | Inter (e1, e2, b1, b2) when (not b1) && b2 ->
      "]" ^ string_of_element e1 ^ ", " ^ string_of_element e2 ^ "]"
  | Inter (e1, e2, b1, b2) ->
      "[" ^ string_of_element e1 ^ ", " ^ string_of_element e2 ^ "]"
  | Vide -> "Intervalle vide"

let interEither_to_string = function
  | Either.Right (Inter (e1, e2, b1, b2)) ->
      "(" ^ string_of_element e1 ^ ", " ^ string_of_element e2 ^ ", "
      ^ string_of_bool b1 ^ ", " ^ string_of_bool b2 ^ ")"
  | Either.Left (Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')) ->
      "(" ^ string_of_element e1 ^ ", " ^ string_of_element e2 ^ ", "
      ^ string_of_bool b1 ^ ", " ^ string_of_bool b2 ^ ") U " ^ "("
      ^ string_of_element e1' ^ ", " ^ string_of_element e2' ^ ", "
      ^ string_of_bool b1' ^ ", " ^ string_of_bool b2' ^ ")"
  | Either.Right Vide -> "Vide"
  | _ -> failwith ""

let est_dans_intervalle e = function
  | Inter (x, y, b_x, b_y) when b_x && b_y && e $>=$ x && e $<=$ y -> true
  | Inter (x, y, b_x, b_y) when b_x && (not b_y) && e $>=$ x && e $<$ y -> true
  | Inter (x, y, b_x, b_y) when (not b_x) && b_y && e $>$ x && e $<=$ y -> true
  | Inter (x, y, b_x, b_y) when (not b_x) && (not b_y) && e $>$ x && e $<$ y ->
      true
  | _ -> false

let comp_inter i i' =
  match (i, i') with
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when Inter (e1, e2, b1, b2) = Inter (e1', e2', b1', b2') ->
      0
  | Inter (e1, _, _, _), Inter (e1', _, _, _) when e1 $<$ e1' -> -1
  | Inter (e1, _, b1, _), Inter (e1', _, b1', _)
    when e1 $=$ e1' && b1 && not b1' ->
      -1
  | Inter (e1, e2, b1, _), Inter (e1', e2', b1', _)
    when (e1, b1) = (e1', b1') && e2 $<$ e2' ->
      -1
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when (e1, b1, e2) = (e1', b1', e2') && (not b2') && b2 ->
      -1
  | _ -> 1

let sont_disjoints i i' =
  match (i, i') with
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when b1 && b2
         && ((e1' $>=$ e1 && e1' $<=$ e2) || (e2' $<=$ e2 && e2' $>=$ e1)) ->
      false
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when (not b1) && (not b2)
         && ((e1' $>$ e1 && e1' $<$ e2) || (e2' $<$ e2 && e2' $>$ e1)) ->
      false
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when (not b1) && b2
         && ((e1' $>$ e1 && e1' $<=$ e2) || (e2' $<=$ e2 && e2' $>$ e1)) ->
      false
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when b1 && (not b2)
         && ((e1' $>=$ e1 && e1' $<$ e2) || (e2' $<$ e2 && e2' $>=$ e1)) ->
      false
  | _ -> true

let inter_union i i' =
  match (i, i') with
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when not (sont_disjoints i i') ->
      Either.Right
        (Inter
           ( min e1 e1',
             max e2 e2',
             (if min e1 e1' = e1 then b1 else b1'),
             if max e2 e2' = e2 then b2 else b2' ))
  | _ -> Either.Left (if comp_inter i i' < 0 then (i, i') else (i', i))

let inter_difference i i' =
  match (i, i') with
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2') when sont_disjoints i i'
    ->
      Either.Right i
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when e1' $>$ e1 && e1' $<$ e2 ->
      if e2' $>$ e2 then Either.Right (Inter (e1, e1', b1, false))
      else Either.Left (Inter (e1, e1', b1, false), Inter (e2', e2, false, b2))
  | Inter (e1, e2, b1, b2), Inter (e1', e2', b1', b2')
    when e2' $>$ e1 && e2' $<$ e2 ->
      Either.Right (Inter (e2', e2, false, b2))
  | _ -> Either.Right Vide

let inter_to_list = function
  | Inter (e1, e2, true, true) -> List.init (e2 - e1 + 1) (( + ) e1)
  | Inter (e1, e2, true, false) -> List.init (e2 - e1) (( + ) e1)
  | Inter (e1, e2, false, true) -> List.init (e2 - e1) (( + ) (e1 + 1))
  | Inter (e1, e2, false, false) -> List.init (e2 - e1 - 1) (( + ) (e1 + 1))
  | _ -> failwith ""

let inclusion_inter i1 i2 =
  List.for_all (Fun.flip est_dans_intervalle i1) (inter_to_list i1)

let valide_inter () =
  let e1 = random_element 100
  and e2 = random_element 100
  and e1' = random_element 100
  and e2' = random_element 100 in
  let i1 = inter (min e1 e2) (max e1 e2) (Random.bool ()) (Random.bool ())
  and i2 =
    inter (min e1' e2') (max e1' e2') (Random.bool ()) (Random.bool ())
  in
  match (i1, i2) with
  | Some _, None -> failwith ""
  | None, Some _ -> failwith ""
  | None, None -> failwith ""
  | Some i1, Some i2 ->
      Printf.printf "i1 = %s\n" (inter_to_string i1);
      Printf.printf "i2 = %s\n" (inter_to_string i2);
      let i3 = inter_union i1 i2 in
      Printf.printf "i3 = %s\n" (interEither_to_string i3);
      let i4 = inter_difference i1 i2 in
      Printf.printf "i4 = %s\n" (interEither_to_string i4);
      Printf.printf "i1 et i2 inclus dans i3 : %s\n"
        (string_of_bool (inclusion_inter i1 i3 && inclusion_inter i2 i3));
      Printf.printf "i1 est inclus dans i2 ou dans i4 : %s"
        (string_of_bool
           ((inclusion_inter i2 i1 && not (inclusion_intereither i4 i1))
           || (not  (inclusion_inter i2 i1)) && inclusion_intereither i4 i1)))

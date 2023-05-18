(** Foundational definitions *)
type currency = USD | GBP | JPY | SGD;;
type day = Int;;
type month = Int;;
type year = Int;;
type date = Date of year * month * day;;


(** Observables *)
type 'a observable = Obs of 'a;;
let konst value = Obs value;;
(** Contract primitives *)
type contract = Zero
    | One of currency
    | Give of contract
    | And of contract * contract
    | Or of contract * contract
    | Truncate of date * contract
    | Then of contract * contract
    | Scale of float observable * contract
    | Get of contract
    | Anytime of contract;;

(** Contract primitive constructors *)
let zero = Zero;;
let one currency = One currency;;
let give contract = Give contract;;
let andc first second = And (first, second);;
let orc first second = Or (first, second);;
let truncate date contract = Truncate (date, contract);;
let thenc first second = Then (first, second);;
let scale amount contract = Scale (amount, contract);;
let get contract = Get contract;;
let anytime contract = Anytime contract;;


(** Example composed contracts *)
(** Zero-coupon discount bond *)
let zcb t x c = scale (konst x) (get (truncate t (one c)));;

(** European Option *)
let euro t u = get (truncate t (orc u zero));;

(** String to char list*)
let str_to_lchar s =
    let rec expl i l =
        if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) [];;

(** Char list to string*)
let lchar_to_string cl = String.concat "" (List.map (String.make 1) cl)

(** Split*)
let split s = 
    s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "");;

(** String to Currency*)
let string_to_currency str =
    match str with
    | "USD" -> Some USD
    | "GBP" -> Some GBP
    | "JPY" -> Some JPY
    | "SGD" -> Some SGD
    | _ -> None

(** Parser to check list is numbers*)
let rec parse_numbers char_list =
    match char_list with
    | '1' :: rest -> parse_numbers rest
    | '2' :: rest -> parse_numbers rest
    | '3' :: rest -> parse_numbers rest
    | '4' :: rest -> parse_numbers rest
    | '5' :: rest -> parse_numbers rest
    | '6' :: rest -> parse_numbers rest
    | '7' :: rest -> parse_numbers rest
    | '8' :: rest -> parse_numbers rest
    | '9' :: rest -> parse_numbers rest
    | '0' :: rest -> parse_numbers rest
    | [] -> true
    | _ -> false;;

(** Parser to parse contract of string*)
let rec parse_contract_string str =
    let string_list = (split str) in
    match string_list with
    | "zcb" :: _ :: x :: c :: [] ->  if ((parse_numbers (str_to_lchar x)) && ((string_to_currency c) != None)) then true else false
    | "euro" :: _ :: u :: [] -> if (parse_contract_string u) then true else false
    | [] -> true
    | _ -> false;;

(** Parser to parse command of string*)
let rec parse_command_string str =
    let str_list = (split str) in
    match str_list with
    | "LC" :: [] -> true
    | "OC" :: rest :: [] -> (parse_contract_string rest)
    | "BC" :: rest :: [] -> (parse_numbers (str_to_lchar rest))
    | "EC" ::  rest :: [] -> (parse_numbers (str_to_lchar rest))
    | [] -> false
    | _ -> false;;
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
let string_to_clist s =
    let rec expl i l =
        if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) [];;

(** Char list to string*)
let clist_to_str cl = String.concat "" (List.map (String.make 1) cl)

(** Split*)
let split s c = 
    s |> String.split_on_char c |> List.filter (fun s -> s <> "");;

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

let t = Unix.localtime (Unix.time ());;
let (cday, cmonth, cyear) = (t.tm_mday, t.tm_mon + 1, t.tm_year + 1900) ;;
(* Printf.printf "The current date is %04d-%02d-%02d\n"
  (1900 + year) (month + 1) day ;; *)

(** Check year is valid*)
let parse_year year = if parse_numbers (string_to_clist year)
    then 
        if ((int_of_string year) >= cyear) then true else false
    else 
        false;;

(** Check if month and day is valid*)
let parse_month_day month day =
    if parse_numbers (string_to_clist month) && parse_numbers (string_to_clist day)
        then
            match month with
            | "01" -> true
            | "02" -> true
            | "03" -> true
            | "04" -> true
            | "05" -> true
            | "06" -> true
            | "07" -> true
            | "08" -> true
            | "09" -> true
            | "10" -> true
            | "11" -> true
            | "12" -> true
            | _ -> false
        else
            false;;


(** Make sure date is of the form xxxx-xx-xx (year-month-day)*)
let rec parse_date date =
    let date_list = (split date '-') in
    if (List.length date_list) != 3 then false
    else
        if (parse_year (List.nth date_list 0))
            then
                parse_month_day (List.nth date_list 1) (List.nth date_list 2) 
            else
                false;;

(** Parser to parse contract of string*)
let rec parse_contract_string string_list =
    match string_list with
    | "zcb" :: _ :: x :: c :: [] ->  if ((parse_numbers (string_to_clist x)) && ((string_to_currency c) != None)) then true else false
    | "euro" :: _ :: rest -> if (parse_contract_string rest) then true else false
    | [] -> false
    | _ -> false;;

(** Parser to parse command of string*)
let rec parse_command_string str =
    let str_list = (split str ' ') in
    match str_list with
    | "LC" :: [] -> true
    | "OC" :: rest -> (parse_contract_string rest)
    | "BC" :: rest :: [] -> (parse_numbers (string_to_clist rest))
    | "EC" ::  rest :: [] -> (parse_numbers (string_to_clist rest))
    | [] -> false
    | _ -> false;;

(* Printf.printf "%b\n" (parse_command_string "LC");;
Printf.printf "%b\n" (parse_command_string "OC zcb date 80 USD");;
Printf.printf "%b\n" (parse_command_string "BC 12344");;
Printf.printf "%b\n" (parse_command_string "OC euro date euro date zcb date 80 USD");; *)


(* let rec main = *)
let main_loop = ref false in
    while not !main_loop do
        print_string "> ";
        let str = read_line () in
            if str = "quit" then main_loop := true
            else
                Printf.printf "%b\n" (parse_command_string str)
done
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

(** Count '.' in list clist*)
let rec check_str_float clist =
    match clist with
    |[] -> 0
    |'.' :: rest -> 1 + check_str_float rest
    |_ :: rest -> check_str_float rest;;

(** Parser to check list is numbers*)
let rec parse_numbers_helper char_list count = 
    if count < 2 then
        match char_list with
        | '1' :: rest -> parse_numbers_helper rest count
        | '2' :: rest -> parse_numbers_helper rest count
        | '3' :: rest -> parse_numbers_helper rest count
        | '4' :: rest -> parse_numbers_helper rest count
        | '5' :: rest -> parse_numbers_helper rest count
        | '6' :: rest -> parse_numbers_helper rest count
        | '7' :: rest -> parse_numbers_helper rest count
        | '8' :: rest -> parse_numbers_helper rest count
        | '9' :: rest -> parse_numbers_helper rest count
        | '0' :: rest -> parse_numbers_helper rest count
        | '.' :: rest -> parse_numbers_helper rest (count+1)
        | [] -> true
        | _ -> false
    else
        false;;

let parse_numbers char_list =
    parse_numbers_helper char_list 0;;

(* let t = Unix.localtime (Unix.time ());;
let (cday, cmonth, cyear) = (t.tm_mday, t.tm_mon + 1, t.tm_year + 1900) ;; *)

(** Check year is valid*)
let parse_year year = 
    let t = Unix.localtime (Unix.time ()) in
    let cyear = t.tm_year + 1900 in
    if parse_numbers (string_to_clist year)
    then 
        if ((int_of_string year) >= cyear) then true else false
    else 
        false;;

(** Check if month and day is valid*)
let parse_month_day month day islyear =
    if parse_numbers (string_to_clist month) && parse_numbers (string_to_clist day)
        then
            let day = int_of_string day in
            if day >= 1
                then
                    match month with
                    | "01" -> if day <= 31 then true else false
                    | "02" -> if islyear
                                then
                                    if day <= 29 then true else false
                                else 
                                    if day <= 28 then true else false   
                    | "03" -> if day <= 31 then true else false
                    | "04" -> if day <= 30 then true else false
                    | "05" -> if day <= 31 then true else false
                    | "06" -> if day <= 30 then true else false
                    | "07" -> if day <= 31 then true else false
                    | "08" -> if day <= 31 then true else false
                    | "09" -> if day <= 30 then true else false
                    | "10" -> if day <= 31 then true else false
                    | "11" -> if day <= 30 then true else false
                    | "12" -> if day <= 31 then true else false
                    | _ -> false
                else
                    false
        else
            false;;

(** Make sure date is of the form xxxx-xx-xx (year-month-day)*)
let rec parse_date date =
    let date_list = (split date '-') in
    if (List.length date_list) != 3 then false
    else
        if (parse_year (List.nth date_list 0))
            then
                let islyear = ((int_of_string (List.nth date_list 0)) mod 4) == 0 in
                parse_month_day (List.nth date_list 1) (List.nth date_list 2) islyear
            else
                false;;

(** Parser to parse contract of string*)
let rec parse_contract_string string_list =
    match string_list with
    | "zcb" :: d :: x :: c :: [] ->  if (parse_date d) && (parse_numbers (string_to_clist x)) && ((string_to_currency c) != None) then true else false
    | "euro" :: d :: rest -> if (parse_date d) && (parse_contract_string rest) then true else false
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
Printf.printf "%b\n" (parse_command_string "OC euro 2024-09-16 euro 2024-10-30 zcb 2025-01-31 80 USD");;
Printf.printf "%b\n" (parse_command_string "OC zcb 2022-10-30 80.00 USD");;
Printf.printf "%b\n" (parse_command_string "OC zcb 2024-09-16 80.00 USD");;
Printf.printf "%b\n" (parse_command_string "OC zcb 2024-09-16 80.00. USD");;
Printf.printf "%b\n" (parse_command_string "OC zcb 2024-09-16 .08 USD");; *)

(* let rec main = *)
let main_loop = ref false in
    while not !main_loop do
        print_string "> ";
        let str = read_line () in
            if str = "quit" then main_loop := true
            else
                Printf.printf "%b\n" (parse_command_string str)
done
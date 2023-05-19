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

(** Example composed contracts *)

(** Zero-coupon discount bond *)
let zcb t x c = Scale (Obs x, Get (Truncate (t, One c)))

(** European Option *)
let euro t u = Get (Truncate (t, Or (u, Zero)));;

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

let currency_option_to_currency cur =
    match cur with
    | Some x -> x
    | None -> USD;;

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

(** Check if year, month, and day is valid*)
let parse_year_month_day year month day islyear =
    if parse_numbers (string_to_clist month) && parse_numbers (string_to_clist day) && parse_numbers (string_to_clist year)
        then
            let t = Unix.localtime (Unix.time ()) in
            let (cyear, cmonth, cday, year, month, day) = t.tm_year + 1900, t.tm_mon + 1, t.tm_mday, int_of_string year, int_of_string month, int_of_string day in
            if (year >= cyear || month >= cmonth || day >= cday) && (year > 0 && month > 0 && day > 0)
                then
                    match month with
                    | 1 -> if day <= 31 then true else false
                    | 2 -> if islyear
                                then
                                    if day <= 29 then true else false
                                else 
                                    if day <= 28 then true else false   
                    | 3 -> if day <= 31 then true else false
                    | 4 -> if day <= 30 then true else false
                    | 5 -> if day <= 31 then true else false
                    | 6 -> if day <= 30 then true else false
                    | 7 -> if day <= 31 then true else false
                    | 8 -> if day <= 31 then true else false
                    | 9 -> if day <= 30 then true else false
                    | 10 -> if day <= 31 then true else false
                    | 11 -> if day <= 30 then true else false
                    | 12 -> if day <= 31 then true else false
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
        let islyear = ((int_of_string (List.nth date_list 0)) mod 4) == 0 in
        parse_year_month_day (List.nth date_list 0) (List.nth date_list 1) (List.nth date_list 2) islyear;;

(** Parser to parse contract of string*)
let rec check_composed string_list =
    match string_list with
    | "zcb" :: d :: x :: c :: [] ->  if (parse_date d) && (parse_numbers (string_to_clist x)) && ((string_to_currency c) != None) then true else false
    | "euro" :: d :: rest -> if (parse_date d) && (check_composed rest) then true else false
    | [] -> false
    | _ -> false;;

(** Parser to parse command of string*)
let rec parse_command_string str =
    let str_list = (split str ' ') in
    check_composed str_list;;


let rec get_args sps counter current_args =
    if counter = 0
        then
            match sps with
            | "," :: rest -> (current_args :: (get_args rest 0 []))
            | "(" :: rest -> get_args rest (counter+1) (List.append current_args ["("])
            | ")" :: [] -> [current_args]
            | _ as arg :: rest -> get_args rest counter (List.append current_args [arg])
            | [] -> []
        else match sps with 
            | "(" :: rest -> get_args rest (counter+1) (List.append current_args ["("])
            | ")" :: rest -> get_args rest (counter-1) (List.append current_args [")"])
            | _ as arg :: rest -> get_args rest counter (List.append current_args [arg])
            | [] -> [];;
            

let rec join = function
  | [] -> ""
  | ""::tail -> join tail
  | hd::tail -> hd ^ join tail;;
let ll_to_string ll = 
    join (List.map join ll);;

Printf.printf "%s\n" (ll_to_string (get_args (split "Give ( One ( USD ) ) , Give ( One ( JPY ) ) )" ' ') 0 []));;
Printf.printf "%s\n" (join (List.hd (get_args (split "Give ( One ( USD ) ) , Give ( One ( JPY ) ) )" ' ') 0 [])));;
(** ( Give ( Give One ) )*)
let rec parse_list str_list = 
    match str_list with
    | "Zero" :: [] -> Zero
    | "One" :: "(" :: u :: ")" :: [] -> One (currency_option_to_currency (string_to_currency u))
    | "Give" :: "(" :: rest -> Give (parse_list (List.hd (get_args rest 0 [])))
    | "And" :: "(" :: rest -> let c1c2 = (get_args rest 0 []) in And ((parse_list (List.hd c1c2)), (parse_list (List.hd (List.tl c1c2))))
    | "Or" :: "(" :: rest -> let c1c2 = (get_args rest 0 []) in And ((parse_list (List.hd c1c2)), (parse_list (List.hd (List.tl c1c2))))
    | "Truncate" :: "(" :: rest -> 
    | _ -> Zero ;;
    (* | "And" :: rest -> let c1c2 = (get_args rest) in And (parse_list (first c1c2), (parse_list (List.c))) *)

let rec parse string =
    let str_list = (split string ' ') in
        (parse_list str_list);;

(* ;; *)
Printf.printf "%b\n" (parse_command_string "zcb date 80 USD");;
Printf.printf "%b\n" (parse_command_string "euro 2024-09-16 euro 2024-10-30 zcb 2025-01-31 80 USD");;
Printf.printf "%b\n" (parse_command_string "zcb 2022-10-30 80.00 USD");;
Printf.printf "%b\n" (parse_command_string "zcb 2024-09-16 80.00 USD");;
Printf.printf "%b\n" (parse_command_string "zcb 2024-09-16 80.00. USD");;
Printf.printf "%b\n" (parse_command_string "zcb 2025-01-00 .08 USD");;

Printf.printf "%b\n" ((parse "And ( Give ( One ( USD ) ) , Give ( One ( JPY ) ) )") = And (Give (One USD), Give (One JPY)));;

(* let rec main = *)
let main_loop = ref false in
    while not !main_loop do
        print_string "> ";
        let str = read_line () in
            if str = "quit" then main_loop := true
            else
                Printf.printf "%b\n" (parse_command_string str)
done
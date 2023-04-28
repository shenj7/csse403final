type currency = USD | GBP | JPY | SGD
type day = Int
type month = Int
type year = Int
type date = Date of year * month * day
type 'a observable = Obs of 'a
type contract =
    Zero
  | One of currency
  | Give of contract
  | And of contract * contract
  | Or of contract * contract
  | Truncate of float * contract
  | Then of contract * contract
  | Scale of float * contract
  | Get of contract
  | Anytime of contract
val zero : contract
val one : currency -> contract
val give : contract -> contract
val andc : contract -> contract -> contract
val orc : contract -> contract -> contract
val truncate : float -> contract -> contract
val thenc : contract -> contract -> contract
val scale : float -> contract -> contract
val get : contract -> contract
val anytime : contract -> contract

type cotation_letter = A | B | C [@@deriving yojson]
type cotation_sign = Plus [@@deriving yojson]
type cotation_bloc = int * cotation_letter * (cotation_sign option) [@@deriving yojson]
type cotation_voie = int * cotation_letter * (cotation_sign option) [@@deriving yojson]
type cotation_arkose = Jaune | Vert | Bleu | Rouge | Noir | Violet [@@deriving yojson]
type cotation_mroc = Jaune | Vert | Bleu | Violet | Rouge | Noir  [@@deriving yojson]
type t = Bloc of cotation_bloc | Voie of cotation_voie | Arkose of cotation_arkose | Mroc of cotation_mroc [@@deriving yojson]

let string_of_letter letter = match letter with
    A -> "A"
   | B -> "B"
   | C -> "C"

let string_of_sign sign = match sign with
    None -> ""
  | Some Plus -> "+"

let string_of_cotation_bloc (i, letter, sign) = (string_of_int i)^(string_of_letter letter)^(string_of_sign sign)

let string_of_cotation_voie (i, letter, sign) = (string_of_int i)^(string_of_letter letter)^(string_of_sign sign)

let string_of_cotation_arkose (color:cotation_arkose) = match color with
    Jaune -> "Jaune"
  | Vert -> "Vert"
  | Bleu -> "Bleu"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Violet -> "Violet"

let string_of_cotation_mroc (color:cotation_mroc) = match color with
    Jaune -> "Jaune"
  | Vert -> "Vert"
  | Bleu -> "Bleu"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Violet -> "Violet"

let string_of_cotation cot = match cot with
    Bloc c -> string_of_cotation_bloc c
  | Voie c -> string_of_cotation_voie c
  | Arkose c -> string_of_cotation_arkose c
  | Mroc c -> string_of_cotation_mroc c

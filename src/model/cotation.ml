type cotation_letter = A | B | C [@@deriving yojson]
type cotation_sign = Plus [@@deriving yojson]
type cotation_bloc = int * cotation_letter * (cotation_sign option) [@@deriving yojson]
type cotation_voie = int * cotation_letter * (cotation_sign option) [@@deriving yojson]
type cotation_arkose = Jaune | Vert | Bleu | Rouge | Noir | Violet [@@deriving yojson]
type t = Bloc of cotation_bloc | Voie of cotation_voie | Arkose of cotation_arkose [@@deriving yojson]



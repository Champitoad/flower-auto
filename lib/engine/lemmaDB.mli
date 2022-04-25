type t

exception LemmaNotFound of string

val empty : Fo.env -> t

val find : t -> string -> Fo.form

val add : t -> name:string -> form:string -> t

val load : t -> (string * string) list -> t

val all : t -> (string * Fo.form) list
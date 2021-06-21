type input = {text:string; pos:int}

type 'a result =
  | Ok of 'a * input
  | Err of string * input

type 'a parser = input -> 'a result

type syntax = 
  | App of syntax * syntax
  | Token of string

let wrap_input (s:string): input =
  { text = s; pos = 0}

let unwrap_input (input:input): string =
  let text = input.text in
  let pos = input.pos in
  let len = String.length text in
  if pos <= len then
    String.sub text pos (len-pos)
  else "out of range!"

let read_input (n:int): string parser = fun input ->
  let max_len = String.length input.text in
  let text = input.text in
  let pos = input.pos in
  if pos + n <= max_len
    then let (rest:input) = {text = text; pos = pos + n} in 
      Ok (String.sub text pos n, rest)
    else Err ("index out of range!", input)



let fail (s:string) =
  fun input -> Err (s,input)
let wrap (x: 'a): 'a parser =
  fun input -> Ok (x,input)

let satisfy (p: bool): unit parser =
  if p then wrap ()
  else fail "not satisfy"

let map (f:'a -> 'b) (p:'a parser): 'b parser =
  fun input ->
    match p input with
    | Ok (x,rest) -> Ok (f x, rest)
    | Err (msg,rest) -> Err (msg,rest)

let bind (p: 'a parser) (f: 'a -> 'b parser) : 'b parser =
  fun input -> 
    match p input with
    | Ok (x,rest) -> (f x) rest
    | Err (msg,rest) -> Err (msg,rest)

let bind_drop (p: 'a parser) (f: 'b parser) : 'b parser =
  fun input -> 
    match p input with
    | Ok (x,rest) -> f rest
    | Err (msg,rest) -> Err (msg,rest)


let branch (p1: 'a parser) (p2: 'a parser): 'a parser =
  fun input -> 
    match p1 input with
    | Ok (x,rest) -> Ok (x,rest)
    | Err (e,rest) -> p2 input

let (>>=) = bind
let (>>) = bind_drop
let (<|>) = branch


let parse (s:string): string parser =
  let len = String.length s in
  read_input len >>= fun s1 ->
  satisfy (String.equal s1 s) >>
  wrap s1

let effect (p:'a parser) (eff:unit->unit): 'a parser =
  fun input ->
    match p input with
    | Ok (x,rest) ->
      let _ = eff () in Ok (x, rest)
    | Err (e,rest) -> Err (e,rest)
let (>>!) = effect

let par_hello_world = 
  parse "hello" >>
  parse " " >>
  parse "world" >>! (fun () -> print_endline "reached") >>
  wrap "hello world"


let run (p:'a parser) (s:string): 'a result =
  p (wrap_input s)


let main = 
  let res = run par_hello_world "hello worl" in
  match res with
  | Ok (x,rest) ->
    print_endline "ok:";
    print_endline x;
    print_endline "rest:";
    print_endline (unwrap_input rest);
  | Err (e,rest) -> 
    print_endline "error:";
    print_endline e;
    print_endline "rest:";
    print_endline (unwrap_input rest);













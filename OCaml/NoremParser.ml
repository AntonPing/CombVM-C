type input = {text:string; pos:int}

type error = {msg:string; pos:int}

type 'a parser = {run: input -> ('a * input, error) result}

type syntax = 
  | App of syntax * syntax
  | Token of string

let make_input (s:string): input =
  { text = s; pos = 0}

let input_ptr_move (input:input) (n:int): input =
  { text = input.text; pos = input.pos + n}

let read_file (path: string): input =
  let ch = open_in path in
  let len = in_channel_length ch in
  let str = really_input_string ch len in
    close_in ch; make_input str


let _ = print_endline "hello"


let main_parse =
  read_file "test.nrm"


let fail (s:string) = 
  { run = fun input ->
    Error {msg = s; pos = input.pos}
  }

let map (f:'a -> 'b) (p:'a parser): 'b parser =
  { run = fun input ->
    match p.run input with
    | Ok (x,rest) -> Ok (f x, rest)
    | Error err -> Error err
  }

let bind (f: 'a -> 'b parser) (p: 'a parser): 'b parser =
  { run = fun input -> 
    match p.run input with
    | Ok (x,rest) -> (f x).run rest
    | Error err -> Error err
  }



let parChar (c:char) (k:char -> 'a):

module Parse = struct 

  let get_pos: int parser =
  { run = fun input -> Ok (input.pos,input)}


  let sat ()

  let char (c:char): char parser =
  { run = fun input ->
      let peek = String.get input.text input.pos in
      let rest = input_ptr_move input 1 in 
      if peek = c
        then Ok (c,rest)
        else Error {msg = "can't match"; pos = input.pos}     
  }

  let (-|) (p1:'a parser) (p2:'a parser): 'a parser =
  { run = fun input ->
      match p1.run input with
      | Ok (x,rest) -> Ok (x,rest)
      | Error _ -> p2.run input 

      }

  let ()



end

let word (s:string) : 





















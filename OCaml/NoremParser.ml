type input = {text:string; pos:int}

type error = {msg:string; pos:int}

type 'a parser = {run: input -> (input * 'a, error) result}


let make_input (s:string): input =
  { text = s; pos = 0}

let read_file (path: string): input =
  let ch = open_in path in
  let len = in_channel_length ch in
  let str = really_input_string ch len in
    close_in ch; make_input str


let _ = print_endline "hello"
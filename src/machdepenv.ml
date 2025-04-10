open Machdep
module R = Str
module L = List
module H = Hashtbl

let preparse (s:string) : (string, string list) H.t =
  let specTable = H.create 32 in
  let commaRegexp = R.regexp "," in
  let spaceRegexp = R.regexp "[ \t]+" in
  let specRegexp = R.regexp "^\\([a-zA-Z_0-9]+\\)[ \t]*=\\(.*\\)$" in
  let specs = R.split spaceRegexp s in
  let addSpec spec = 
    if R.string_match specRegexp spec 0 then begin
      let name = R.matched_group 1 spec in
      let value = R.matched_group 2 spec in
      H.add specTable name (R.split commaRegexp value)
    end
    else
      raise (Failure ("invalid specification string " ^ spec))
  in
  L.iter addSpec specs;
  specTable

let errorWrap name f =
  try 
    f name
  with Not_found -> raise (Failure (name ^ " not specified"))
  | _ -> raise (Failure ("invalid format for " ^ name))

let getNthString n specTable name = 
  let l = H.find specTable name in
  L.nth l n

let getNthInt n specTable name =
  errorWrap name (fun name -> int_of_string (getNthString n specTable name))

let getNthBool n specTable name = 
  errorWrap name (fun name -> bool_of_string (getNthString n specTable name))

let getBool = getNthBool 0
let getInt = getNthInt 0
let getSizeof = getNthInt 0
let getAlignof = getNthInt 1

let respace = Str.global_replace (Str.regexp "_") " "

let modelParse (s:string) : mach = 
  let entries =
    try
      preparse s 
    with Failure msg -> raise (Failure msg)
    | _ -> raise (Failure "invalid machine specification")
  in let lookup getter typename zeroval = if H.find_opt entries typename == None then zeroval else getter entries typename
  in let _ = H.iter (fun x y -> Printf.printf "%s -> %s\n" x (String.concat ", " y)) entries
  in
  {
    version_major = 0;
    version_minor = 0;
    version = "machine model " ^ s;
    underscore_name = lookup getBool "underscore_name" false;
    sizeof_short = lookup getSizeof "short" 0;
    alignof_short = lookup getAlignof "short" 0;
    sizeof_bool = lookup getSizeof "bool" 0;
    alignof_bool = lookup getAlignof "bool" 0;
    sizeof_int = lookup getSizeof "int" 0;
    alignof_int = lookup getAlignof "int" 0;
    sizeof_long = lookup getSizeof "long" 0;
    alignof_long = lookup getAlignof "long" 0;
    sizeof_longlong = lookup getSizeof "long_long" 0;
    alignof_longlong = lookup getAlignof "long_long" 0;
    sizeof_ptr = lookup getSizeof "pointer" 0;
    alignof_ptr = lookup getAlignof "pointer" 0;
    alignof_enum = lookup getInt "alignof_enum" 0;
    sizeof_float = lookup getSizeof "float" 0;
    alignof_float = lookup getAlignof "float" 0;
    sizeof_shortfloat = lookup getSizeof "short_float" 0;
    alignof_shortfloat = lookup getAlignof "short_float" 0;
    sizeof_double = lookup getSizeof "double" 0;
    alignof_double = lookup getAlignof "double" 0;
    sizeof_longdouble = lookup getSizeof "long_double" 0;
    alignof_longdouble = lookup getAlignof "long_double" 0;
    alignof___float128 = lookup getAlignof "__float128" 0;
    alignof_float128 = lookup getAlignof "_Float128" 0;
    alignof_float128x = lookup getAlignof "_Float128x" 0;
    alignof_float64x = lookup getAlignof "_Float64x" 0;
    alignof_float32x = lookup getAlignof "_Float32x" 0;
    alignof_float16x = lookup getAlignof "_Float16x" 0;
    alignof_float64 = lookup getAlignof "_Float64" 0;
    alignof_float32 =  lookup getAlignof "_Float32" 0;
    alignof_float16 = lookup getAlignof "_Float16" 0;
    sizeof_complex_shortfloat = lookup getSizeof "short_float__Complex" 0;
    alignof_complex_shortfloat = lookup getAlignof "short_float__Complex" 0;
    sizeof_complex_float = lookup getSizeof "float__Complex" 0;
    alignof_complex_float = lookup getAlignof "float__Complex" 0;
    sizeof_complex_double = lookup getSizeof "double__Complex" 0;
    alignof_complex_double = lookup getAlignof "double__Complex" 0;
    sizeof_complex_longdouble = lookup getSizeof "long_double__Complex" 0;
    alignof_complex_longdouble = lookup getAlignof "long_double__Complex" 0;
    alignof_complex_float128 = lookup getAlignof "_Float128__Complex" 0;
    alignof_complex_float128x = lookup getAlignof "_Float128x__Complex" 0;
    alignof_complex_float64x = lookup getAlignof "_Float64x__Complex" 0;
    alignof_complex_float32x = lookup getAlignof "_Float32x__Complex" 0;
    alignof_complex_float16x = lookup getAlignof "_Float16x__Complex" 0;
    sizeof_void = lookup getSizeof "void" 0;
    sizeof_fun = lookup getSizeof "fun" 0;
    alignof_fun = lookup getAlignof "fun" 0;
    alignof_str = lookup getInt "alignof_string" 0;
    alignof_aligned = lookup getInt "max_alignment" 0;
    size_t = respace (getNthString 0 entries "size_t");
    wchar_t = respace (getNthString 0 entries "wchar_t");
    char_is_unsigned = not (getBool entries "char_signed");
    const_string_literals = lookup getBool "const_string_literals" false;
    little_endian = not (getBool entries "big_endian");
    __thread_is_keyword = lookup getBool "__thread_is_keyword" false;
    __builtin_va_list = lookup getBool "__builtin_va_list" false;
  }

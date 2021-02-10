open Options
open Utils

type html_node =
  | Img of string
  | Code of string
  | Paragraph of string
  | List of html_node list

let rec print_html oc = function
    Img s -> Printf.fprintf oc "<img src=\"%s\" />\n" s
  | Code s -> Printf.fprintf oc "<pre>%s</pre>\n" s
  | Paragraph s -> Printf.fprintf oc "<div>%s</div>\n" s
  | List l -> List.iter (print_html oc) l

type report_section = { sect_title: string;
                        sect_id: string;
                        sect_content: html_node
                      }

let report = ref ([]: report_section list)

let add_to_report id title content =
  report := !report @ [{ sect_id = id; sect_title = title; sect_content = content }]

let make_report filename report () =
  let html = open_out (filename ^ ".html") in
  Printf.fprintf html "<ul id=\"top\">";
  List.iter
    (fun { sect_id; sect_title  } ->
       Printf.fprintf html "<li><a href=\"#%s\">%s</a></li>\n" sect_id sect_title
    )
    !report;
  Printf.fprintf html "</ul>";
  List.iter
    (fun { sect_id; sect_title; sect_content } ->
       Printf.fprintf html "<fieldset><h3 id=\"%s\"><a href=\"#top\">&uarr;</a> %s</h3>%a</fieldset>\n" sect_id sect_title print_html sect_content
    )
    !report;
  close_out html;
  ()

let call_dot report_sectid report_secttitle file () : unit =
  if not !Options.no_dot
  then begin
    let r = Sys.command (Format.sprintf "dot -Tsvg %s -o %s.svg" file file) in
    add_to_report report_sectid report_secttitle (Img (Filename.basename file^".svg"));
    ignore r
  end

(*  *)


type run_result = {
  step: string;
  retval: int option;
  output: string;
  error: string option;
  time: float;
}

type compile_result = {
  step: string;
  error: string option;
  data: Yojson.t
}

type result = RunRes of run_result
            | CompRes of compile_result


let results : result list ref = ref []


let record_compile_result ?error:(error=None) ?data:(data=[]) step =
  let data = if not !Options.nostats then `List data else `Null in
  results := !results @ [CompRes { step; error; data}]


let run step flag eval p =
  if flag then begin
    let starttime = Unix.gettimeofday () in
    let res = match eval Format.str_formatter p !heapsize !params with
      | exception e ->
        Error (Printexc.to_string e)
      | e -> e in
    let timerun = Unix.gettimeofday () -. starttime in
    let output = Format.flush_str_formatter () in
    let rres = { step ; retval = None; output; error = None; time = timerun} in
    let rres =
    begin match res with
      | OK v ->  { rres with retval = v }
      | Error msg -> { rres with error = Some msg }
    end in
    results := !results @ [RunRes rres];
    add_to_report step ("Run " ^ step) (
      Paragraph
        (
          Printf.sprintf "With parameters : [%s]<br>\n" (String.concat"," (List.map string_of_int !params))
          ^ Printf.sprintf "Mem size : %d bytes<br>\n" !heapsize
          ^ Printf.sprintf "Return value : %s<br>\n" (match rres.retval with | Some v -> string_of_int v | _ -> "none")
          ^ Printf.sprintf "Output : <pre style=\"padding: 1em; background-color: #ccc;\">%s</pre>\n" output
          ^
          (match rres.error with
           | Some msg -> Printf.sprintf "Error : <pre style=\"padding: 1em; background-color: #fcc;\">\n%s</pre>\n" msg
           | _ -> "")
          ^ Printf.sprintf "Time : %f seconds<br>\n" timerun
        )
    )
  end


let json_output_string () =
  let open Yojson in
  let jstring_of_ostring o =
    match o with
    | None -> `Null
    | Some s -> `String s
  in
  let j = `List (List.map (function
      | RunRes { step; retval; output; error; time } ->
        `Assoc [("runstep",`String step);
                ("retval", match retval with Some r -> `Int r | None -> `Null);
                ("output", `String output);
                ("error", jstring_of_ostring error);
                ("time", `Float time)
               ]
      | CompRes { step; error; data } ->
        `Assoc [("compstep",`String step);
                ("error", jstring_of_ostring error);
                ("data", data)
               ]
    ) !results) in
  (Yojson.pretty_to_string j)

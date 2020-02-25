
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

module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module G = Graphics_js

let js = Js.string

let doc = Html.window##.document

let doc_ = Html.document

let canvas_width = 300.
let canvas_height = 150.

let create_canvas () =
  let r = Html.createCanvas doc_ in
  r##.width := int_of_float canvas_width;
  r##.height := int_of_float canvas_height;
  r


let rec draw_things c (counter:int) = 
  let ( >>= ) = Lwt.bind  in
  c##.font := js "50px serif";
  c##fillText (js "Hello World") 20. 90.;
  c##strokeRect 0. 0. canvas_width canvas_height;
  c##.font := js "20px serif";
  c##fillText (js ("This page has been open for:")) 20. 110.;
  c##fillText (js (Printf.sprintf "%i seconds" counter)) 20. 130.;
  (Js_of_ocaml_lwt.Lwt_js.sleep 1.0)  >>= fun () ->
    c##clearRect 0. 0. canvas_width canvas_height;
    draw_things c (counter + 1)


(* Generic button with text on input and 
  an event gotten via onclick*)
let button name callback = 
  let res = doc##createDocumentFragment in
  let input = Html.createInput  ~_type:(js "button") doc in
  input##.value := js name;
  input##.onclick := Html.handler callback;
  Js_of_ocaml.Dom.appendChild res input;
  res



let onload _ =
  let main = Js.Opt.get (doc##getElementById (js "main")) (fun () -> assert false) in
  let canvas = create_canvas () in
  G.open_canvas canvas;
  Js_of_ocaml.Dom.appendChild doc##.body canvas;
  let c = canvas##getContext Html._2d_ in
  let promise = ref @@ draw_things c (-15) in
  Js_of_ocaml.Dom.appendChild main
    (button "Reset" (fun _ ->
      let div = Html.createDiv doc in
      Js_of_ocaml.Dom.appendChild main div;
      Lwt.cancel !promise;
      c##clearRect 0. 0. canvas_width canvas_height;
      promise := draw_things c 0;
      Js._false));
  Js._false

let _ =
  Html.window##.onload := Html.handler onload
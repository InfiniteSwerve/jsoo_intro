module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
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


let rec draw_things context (counter:int) = 
  let open Lwt.Syntax  in
  context##.font := js "50px serif";
  context##fillText (js "Hello World") 20. 90.;
  context##strokeRect 0. 0. canvas_width canvas_height;
  context##.font := js "20px serif";
  context##fillText (js ("This page has been open for:")) 20. 110.;
  context##fillText (js (Printf.sprintf "%i seconds" counter)) 20. 130.;
  let* () = (Js_of_ocaml_lwt.Lwt_js.sleep 1.0) in
  context##clearRect 0. 0. canvas_width canvas_height;
  draw_things context (counter + 1)


(* Generic button with text on input and 
  an event gotten via onclick*)
let button name callback = 
  let res = doc##createDocumentFragment in
  let input = Html.createInput  ~_type:(js "button") doc in
  input##.value := js name;
  input##.onclick := Html.handler callback;
  Dom.appendChild res input;
  res



let onload _ =
  let main = Js.Opt.get (doc##getElementById (js "main")) (fun () -> assert false) in
  let canvas = create_canvas () in
  G.open_canvas canvas;
  Dom.appendChild doc##.body canvas;
  let context = canvas##getContext Html._2d_ in
  let promise = ref @@ draw_things context (0) in
  Dom.appendChild main
    (button "Reset" (fun _ ->
      let div = Html.createDiv doc in
      Dom.appendChild main div;
      Lwt.cancel !promise;
      context##clearRect 0. 0. canvas_width canvas_height;
      promise := draw_things context 0;
      Js._false));
  Js._false

let _ =
  Html.window##.onload := Html.handler onload
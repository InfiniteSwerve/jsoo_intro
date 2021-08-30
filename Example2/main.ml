module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js
module G = Graphics_js

let js = Js.string

let doc = Html.document

let canvas_width = 300.
let canvas_height = 150.

let create_canvas () =
  let r = Html.createCanvas doc in
  r##.width := int_of_float canvas_width;
  r##.height := int_of_float canvas_height;
  r



let rec draw_things context (counter:int) = 
  let open Lwt.Syntax in
  context##.font := js "50px serif";
  context##fillText (js "Hello World") 20. 90.;
  context##strokeRect 0. 0. canvas_width canvas_height;
  context##.font := js "20px serif";
  context##fillText (js ("This page has been open for:")) 20. 110.;
  context##fillText (js (Printf.sprintf "%i seconds" counter)) 20. 130.;
  let* () = (Js_of_ocaml_lwt.Lwt_js.sleep 1.0) in
  context##clearRect 0. 0. canvas_width canvas_height;
  draw_things context (counter + 1)

let main _ =
  let canvas = create_canvas () in
  G.open_canvas canvas;
  Dom.appendChild doc##.body canvas;
  let context = canvas##getContext Html._2d_ in
  draw_things context 0 |> ignore;
  Js._true

let _ =
  Html.window##.onload := Html.handler main
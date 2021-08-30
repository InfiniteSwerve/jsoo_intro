module Html = Js_of_ocaml.Dom_html
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

let draw_things context = 
  context##.font := js "50px serif";
  context##fillText (js "Hello World") 20. 90.;
  context##strokeRect 0. 0. canvas_width canvas_height
  

let main _ =
  let canvas = create_canvas () in
  G.open_canvas canvas;
  Js_of_ocaml.Dom.appendChild doc##.body canvas;
  let context = canvas##getContext Html._2d_ in
  draw_things context |> ignore;
  Js._true

let _ =
  Html.window##.onload := Html.handler main
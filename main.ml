
(* Dom_html is the data representation of structure and content that make up a 
  document on the web. It's a tree structure, and is how programming languages
  interact with web content. 
  https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction
  *)
module Html = Js_of_ocaml.Dom_html
 
module Js = Js_of_ocaml.Js
 
module G = Graphics_js

(* utility functions *)
let fmt = Printf.sprintf
let js = Js.string
let doc = Html.document

let canvas_width = 640. 
let canvas_height = 480.

let r = 3.

let x0 = 0.

and y0 = 0.

type ball = {
  x_position: float;
  y_position: float;
  x_velocity: float;
  y_velocity: float;
}


let objects = [
  {x_position = 50.;
  y_position = 50.;
  x_velocity = 10.;
  y_velocity = 10.;}
]

let draw_ball context (b:ball) =
  begin
  context##beginPath;
  context##arc b.x_position b.y_position r 0. 6.283185 Js._false;
  context##fill;
  end

let update_ball (b:ball) = 
  let new_x_position = b.x_position +. b.x_velocity 
  and new_y_position = b.y_position +. b.y_velocity in
  let new_x_velocity = if new_x_position -. r <= x0|| new_x_position +. r >= canvas_width then -.b.x_velocity else b.x_velocity
  and new_y_velocity = if new_y_position -. r <= y0 || new_y_position +. r >= canvas_height then -.b.y_velocity else b.y_velocity in
  {
    x_position = new_x_position;
    y_position = new_y_position;
    x_velocity = new_x_velocity;
    y_velocity = new_y_velocity;
  }

let ( >>= ) = Lwt.bind 


let rec loop (canvas:Html.canvasElement Js.t) objects =
  let c = canvas##getContext Html._2d_ in
  c##strokeRect x0 y0 canvas_width canvas_height;
  List.iter (draw_ball c) objects;
  (Js_of_ocaml_lwt.Lwt_js.sleep (1. /. 60.)) >>= fun () -> 
    c##beginPath;
    c##clearRect 0. 0. canvas_width canvas_height;
    loop canvas (List.map update_ball objects)


let create_canvas () =
  let r = Html.createCanvas doc in
  r##.width := int_of_float canvas_width;
  r##.height := int_of_float canvas_height;
  r

let start _ =
  let canvas = create_canvas () in
  G.open_canvas canvas;
  Js_of_ocaml.Dom.appendChild doc##.body canvas;
  loop canvas objects|> ignore;
  Js._false

let _ =
  Html.window##.onload := Html.handler start
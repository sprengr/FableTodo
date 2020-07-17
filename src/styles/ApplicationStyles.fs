[<AutoOpen>]
module ApplicationStyles

open Zanaptak.TypedCssClasses

type Tw = CssClasses<"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css", Naming.Underscores>

type ButtonStyle =
    | Small of string
    | Normal of string

let btn buttonStyle =
    match buttonStyle with
    | Small color ->
        [ Tw.w_6
          Tw.inline_flex
          Tw.items_center
          Tw.mr_1
          Tw.px_1
          Tw.py_1
          Tw.text_xs
          Tw.font_thin
          Tw.border
          Tw.border_transparent
          Tw.rounded_md
          Tw.text_white
          Tw.focus_outline_none
          Tw.transition
          Tw.duration_150
          Tw.ease_in_out
          "bg-" + color + "-600"
          "hover:bg-" + color + "-500"
          " focus:border-" + color + "-700"
          "active:bg-" + color + "-700"
          "focus:shadow-outline-" + color ]

    | Normal color ->
        [ Tw.inline_flex
          Tw.items_center
          Tw.px_4
          Tw.py_2
          Tw.text_sm
          Tw.leading_5
          Tw.font_medium
          Tw.border
          Tw.border_transparent
          Tw.rounded_md
          Tw.text_white
          Tw.focus_outline_none
          Tw.transition
          Tw.duration_150
          Tw.ease_in_out
          "bg-" + color + "-600"
          "hover:bg-" + color + "-500"
          "focus:border-" + color + "-700"
          "active:bg-" + color + "-700"
          "focus:shadow-outline-" + color ]

let editInputStyle =
    [ Tw.shadow
      Tw.appearance_none
      Tw.border
      Tw.rounded
      Tw.w_full
      Tw.py_2
      Tw.px_3
      Tw.text_gray_700
      Tw.leading_tight 
      Tw.focus_outline_none
      Tw.focus_shadow_outline ]

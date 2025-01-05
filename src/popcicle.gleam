import gleam/bool
import gleam/dynamic
import gleam/int
import gleam/list
import gleam/result
import lustre/attribute.{attribute}
import lustre/element.{type Element} as _
import lustre/element/html.{div, style}
import lustre/event
import plinth/browser/document
import plinth/browser/element.{get_attribute, set_attribute} as pl_element
import plinth/browser/event as pl_event

pub type PrefferedPosition {
  TopLeft
  TopCenter
  TopRight
  RightTop
  RightCenter
  RightBottom
  BottomLeft
  BottomCenter
  BottomRight
  LeftTop
  LeftCenter
  LeftBottom
}

pub type PopcicleConfig {
  PopcicleConfig(css: String, click_gap: Int, hover_gap: Int)
}

pub type ShowType {
  Click
  Hover
}

@external(javascript, "./popcicle_ffi.mjs", "initPopcicle")
fn init_popcicle(config: PopcicleConfig) -> Nil

@external(javascript, "./popcicle_ffi.mjs", "getPopcicleConfig")
fn get_popcicle_config() -> Result(PopcicleConfig, Nil)

@external(javascript, "./popcicle_ffi.mjs", "registerPopcicle")
fn register_popcicle(elements: Element(a)) -> Int

@external(javascript, "./popcicle_ffi.mjs", "getPopcicles")
fn get_popcicles() -> List(#(Int, Element(a)))

@external(javascript, "./popcicle_ffi.mjs", "getElementLeft")
fn get_element_left(elem: pl_element.Element) -> Result(Int, Nil)

@external(javascript, "./popcicle_ffi.mjs", "getElementRight")
fn get_element_right(el fdem: pl_element.Element) -> Result(Int, Nil)

@external(javascript, "./popcicle_ffi.mjs", "getElementTop")
fn get_element_top(elem: pl_element.Element) -> Result(Int, Nil)

@external(javascript, "./popcicle_ffi.mjs", "getElementBottom")
fn get_element_bottom(elem: pl_element.Element) -> Result(Int, Nil)

@external(javascript, "./popcicle_ffi.mjs", "getElementWidth")
fn get_element_width(elem: pl_element.Element) -> Result(Int, Nil)

@external(javascript, "./popcicle_ffi.mjs", "getElementHeight")
fn get_element_height(elem: pl_element.Element) -> Result(Int, Nil)

@external(javascript, "./popcicle_ffi.mjs", "isMouseOver")
fn is_mouse_over(event: pl_event.Event(a), elem: pl_element.Element) -> Bool

@external(javascript, "./popcicle_ffi.mjs", "contains")
pub fn contains(element: pl_element.Element, other: pl_element.Element) -> Bool

@external(javascript, "./popcicle_ffi.mjs", "getFirstParentWithAttrib")
fn get_first_parent_with_attrib(
  target: dynamic.Dynamic,
  attrib: String,
) -> Result(dynamic.Dynamic, Nil)

pub fn default_config() {
  PopcicleConfig(
    css: "
div[data-popcicle-open] {
  position: absolute;
  transition: opacity 200ms;
}

div[data-popcicle-open=\"0\"] {
  opacity: 0;
  pointer-events: none;
}

div[data-popcicle-open=\"1\"] {
  opacity: 1;
}",
    click_gap: 4,
    hover_gap: -2,
  )
}

pub fn initialize(config: PopcicleConfig, cb: fn() -> Element(a)) {
  let _ = init_popcicle(config)

  document.add_event_listener("mousedown", fn(event) {
    let _ = {
      use target <- result.try(
        dynamic.field("target", dynamic.dynamic)(dynamic.from(event)),
      )
      list.map(get_popcicles(), fn(popcicle) {
        use popcicle <- result.try(document.query_selector(
          "div[data-popcicle-id=\"" <> int.to_string(popcicle.0) <> "\"]",
        ))
        use target <- result.try(
          pl_element.cast(target) |> result.replace_error(Nil),
        )
        use <- bool.guard(contains(popcicle, target), Error(Nil))
        Ok(set_attribute(popcicle, "data-popcicle-open", "0"))
      })
      Error([])
    }
    Nil
  })

  document.add_event_listener("mousemove", fn(event) {
    let _ = {
      list.map(get_popcicles(), fn(popcicle) {
        use target <- result.try(document.query_selector(
          "div[data-popcicle-target-id=\"" <> int.to_string(popcicle.0) <> "\"]",
        ))

        use popcicle <- result.try(document.query_selector(
          "div[data-popcicle-id=\""
          <> int.to_string(popcicle.0)
          <> "\"], data[data-popcicle-show-on=\"hover\"]",
        ))

        use show_on <- result.try(pl_element.get_attribute(
          popcicle,
          "data-popcicle-show-on",
        ))
        use <- bool.guard(show_on != "hover", Error(Nil))

        use <- bool.guard(
          is_mouse_over(event, popcicle) || is_mouse_over(event, target),
          Error(Nil),
        )

        set_attribute(popcicle, "data-popcicle-open", "0")
        Error(Nil)
      })
      Error([])
    }
    Nil
  })

  html.div(
    [],
    list.flatten([
      [style([], config.css), cb()],
      get_popcicles()
        |> list.map(fn(popcicle) {
          div(
            [
              attribute("data-popcicle-id", int.to_string(popcicle.0)),
              attribute("data-popcicle-open", "0"),
              attribute("style", "left:0;top:0;"),
            ],
            [popcicle.1],
          )
        }),
    ]),
  )
}

/// A simple popcicle will add the element in the child parameter where the
/// function is called. Then when that element is pressed (show_on=popcicle.Click) or
/// hovered (show_on=popcicle.Hover), it will show the popcicle as a popover
/// positioned by the position parameter. By default the popcicle and it's
/// button comes completely unstyled and you *can* use it that way however, it
/// is highly recommened you create your own styled functions. If you don't
/// want to do that then there is a prestyled component based on shadcn/ui that
/// you can simply copy into your project: [Dropdown Menu](https://github.com/dinkelspiel/popcicle/blob/master/examples/dropdown_menu.gleam)
///
pub fn popcicle(
  child: Element(a),
  position: PrefferedPosition,
  popcicle: Element(a),
  show_on: ShowType,
) {
  let open_popcicle = fn(event) -> Result(a, List(dynamic.DecodeError)) {
    use button <- result.try(dynamic.field("target", dynamic.dynamic)(event))
    use button <- result.try(
      get_first_parent_with_attrib(button, "data-popcicle-target-id")
      |> result.replace_error([]),
    )
    use button <- result.try(
      pl_element.cast(button) |> result.replace_error([]),
    )
    use popcicle_target_id <- result.try(
      get_attribute(button, "data-popcicle-target-id")
      |> result.replace_error([]),
    )
    use popcicle <- result.try(
      document.query_selector(
        "div[data-popcicle-id='" <> popcicle_target_id <> "']",
      )
      |> result.replace_error([]),
    )

    use styles <- result.try(get_position_values(
      position,
      button,
      popcicle,
      show_on,
    ))

    set_attribute(popcicle, "data-popcicle-open", "1")
    let assert Ok(id) = int.parse(popcicle_target_id)
    set_attribute(
      popcicle,
      "style",
      styles <> "; z-index: " <> int.to_string(9999 - id),
    )
    set_attribute(popcicle, "data-popcicle-show-on", case show_on {
      Click -> "click"
      Hover -> "hover"
    })

    Error([])
  }

  let id = register_popcicle(popcicle)
  div(
    [
      attribute("data-popcicle-target-id", int.to_string(id)),
      case show_on {
        Click -> event.on("click", open_popcicle)
        Hover -> event.on("mouseover", open_popcicle)
      },
    ],
    [child],
  )
}

fn get_position_values(
  position: PrefferedPosition,
  relative: pl_element.Element,
  popcicle: pl_element.Element,
  show_on: ShowType,
) {
  use right <- result.try(
    get_element_right(relative) |> result.replace_error([]),
  )
  use top <- result.try(get_element_top(relative) |> result.replace_error([]))
  use left <- result.try(get_element_left(relative) |> result.replace_error([]))
  use bottom <- result.try(
    get_element_bottom(relative) |> result.replace_error([]),
  )
  use popcicle_width <- result.try(
    get_element_width(popcicle) |> result.replace_error([]),
  )
  use popcicle_height <- result.try(
    get_element_height(popcicle) |> result.replace_error([]),
  )
  use target_width <- result.try(
    get_element_width(relative) |> result.replace_error([]),
  )
  use target_height <- result.try(
    get_element_height(relative) |> result.replace_error([]),
  )
  use config <- result.try(get_popcicle_config() |> result.replace_error([]))

  let gap = case show_on {
    Click -> config.click_gap
    Hover -> config.hover_gap
  }

  let res = case position {
    TopLeft -> [
      #("top", top - popcicle_height - gap |> int.to_string),
      #("left", left - popcicle_width + target_width |> int.to_string),
    ]
    TopCenter -> [
      #("top", top - popcicle_height - gap |> int.to_string),
      #("left", left - { popcicle_width - target_width } / 2 |> int.to_string),
    ]
    TopRight -> [
      #("top", top - popcicle_height - gap |> int.to_string),
      #("left", left |> int.to_string),
    ]
    RightBottom -> [
      #("top", top |> int.to_string),
      #("left", right + gap |> int.to_string),
    ]
    RightCenter -> [
      #("top", top - { popcicle_height - target_height } / 2 |> int.to_string),
      #("left", right + gap |> int.to_string),
    ]
    RightTop -> [
      #("top", top - popcicle_height + target_height |> int.to_string),
      #("left", right + gap |> int.to_string),
    ]
    BottomLeft -> [
      #("top", bottom + gap |> int.to_string),
      #("left", left - popcicle_width + target_width |> int.to_string),
    ]
    BottomCenter -> [
      #("top", bottom + gap |> int.to_string),
      #("left", left - { popcicle_width - target_width } / 2 |> int.to_string),
    ]
    BottomRight -> [
      #("top", bottom + gap |> int.to_string),
      #("left", left |> int.to_string),
    ]
    LeftBottom -> [
      #("top", top |> int.to_string),
      #("left", left - popcicle_width - gap |> int.to_string),
    ]
    LeftCenter -> [
      #("top", top - { popcicle_height - target_height } / 2 |> int.to_string),
      #("left", left - popcicle_width - gap |> int.to_string),
    ]
    LeftTop -> [
      #("top", top - popcicle_height + target_height |> int.to_string),
      #("left", left - popcicle_width - gap |> int.to_string),
    ]
  }

  Ok(
    res
    |> list.map(fn(a) {
      #(a.0, case a.0 {
        "left" | "top" -> a.1 <> "px"
        _ -> a.1
      })
    })
    |> tuples_to_style(""),
  )
}

fn tuples_to_style(in: List(#(String, String)), acc: String) {
  case in {
    [item, ..rest] ->
      tuples_to_style(rest, acc <> item.0 <> ": " <> item.1 <> "; ")
    [] -> acc
  }
}

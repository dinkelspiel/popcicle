import gleam/list
import lustre/attribute.{type Attribute, class}
import lustre/element.{type Element}
import lustre/element/html.{div, span}
import popcicle.{type PrefferedPosition}

pub fn dropdown_menu(
  child: Element(a),
  position: PrefferedPosition,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
  show_on: popcicle.ShowType,
) {
  popcicle.popcicle(
    child,
    position,
    div(
      list.flatten([
        [
          class(
            "p-1 min-w-[8rem] outline-none shadow-md bg-white rounded-md border border-zinc-200 overflow-hidden w-56",
          ),
        ],
        attributes,
      ]),
      children,
    ),
    show_on,
  )
}

pub fn item(
  attributes: List(attribute.Attribute(a)),
  children: List(Element(a)),
) {
  div(
    list.flatten([
      [
        class(
          "relative flex cursor-pointer hover:bg-zinc-100 text-zinc-900 select-none items-center gap-2 rounded-sm px-2 py-1.5 text-sm outline-none transition-colors focus:bg-zinc-50 focus:text-zinc-800 data-[disabled]:pointer-events-none data-[disabled]:opacity-50 [&>svg]:size-4 [&>svg]:shrink-0",
        ),
      ],
      attributes,
    ]),
    children,
  )
}

pub fn label(
  attributes: List(attribute.Attribute(a)),
  children: List(Element(a)),
) {
  div(
    list.flatten([[class("px-2 py-1.5 text-sm font-semibold")], attributes]),
    children,
  )
}

pub fn separator(attributes: List(attribute.Attribute(a))) {
  div(list.flatten([[class("-mx-1 my-1 h-px bg-zinc-100")], attributes]), [])
}

pub fn shortcut(
  attributes: List(attribute.Attribute(a)),
  children: List(Element(a)),
) {
  span(
    list.flatten([
      [class("ml-auto text-xs tracking-widest opacity-60")],
      attributes,
    ]),
    children,
  )
}

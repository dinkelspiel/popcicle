import gleam/int
import lustre
import lustre/attribute.{class, href}
import lustre/element.{type Element}
import lustre/element/html.{button, div, h1, p, text, a, span}
import lustre/event
import popcicle
import popcicle/components/dropdown_menu
import lucide_lustre

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", 0)

  Nil
}

type Model =
  Int

fn init(initial_count: Int) -> Model {
  case initial_count < 0 {
    True -> 0
    False -> initial_count
  }
}

pub opaque type Msg {
  Incr
  Decr
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}

fn button_styles() {
  "px-[30px] h-[40px] rounded-md text-[13px] font-semibold shadow-md bg-gradient-to-r transition-all "
}

fn light_button_styles() {
  button_styles()
  <> "from-zinc-100 to-zinc-50 hover:from-zinc-200 hover:to-zinc-100"
}

fn dark_button_styles() {
  button_styles()
  <> "from-zinc-800 to-zinc-900 hover:from-zinc-700 hover:to-zinc-800 text-white"
}

fn view(model: Model) -> Element(Msg) {
  use <- popcicle.initialize(popcicle.default_config())

  div([class("text-zinc-900 w-[60ch] mx-auto mt-48")], [
    div([class("flex flex-col gap-3")], [
      h1([class("text-5xl font-semibold text-center")], [text("Popcicle")]),
      p([class("text-center text-lg mb-3")], [
        text("Simple to use, styleable popovers for "),
        span([], [
          a([class("text-center underline text-violet-400 cursor-pointer"), href("https://github.com/lustre-labs/lustre")], [text("Lustre")])
        ])
      ]),
      div([class("flex gap-2 justify-center")], [
        dropdown_menu.dropdown_menu(
          button([class(dark_button_styles())], [text("Open popover")]),
          popcicle.BottomCenter,
          [],
          [
            dropdown_menu.label([], [text("My Account")]),
            dropdown_menu.separator([]),
            dropdown_menu.item([], [
              text("Profile"),
              dropdown_menu.shortcut([], [text("⇧⌘P")]),
            ]),
            dropdown_menu.item([], [
              text("Billing"),
              dropdown_menu.shortcut([], [text("⌘B")]),
            ]),
            dropdown_menu.item([], [
              text("Settings"),
              dropdown_menu.shortcut([], [text("⌘S")]),
            ]),
            dropdown_menu.item([], [
              text("Keyboard shortcuts"),
              dropdown_menu.shortcut([], [text("⌘K")]),
            ]),
            dropdown_menu.separator([]),
            dropdown_menu.item([], [text("Team")]),
            dropdown_menu.dropdown_menu(
              dropdown_menu.item([class("justify-between")], [
                text("Invite users"),
                lucide_lustre.chevron_right([]),
              ]),
              popcicle.RightBottom,
              [class("!w-28")],
              [
                dropdown_menu.item([], [text("Email")]),
                dropdown_menu.item([], [text("Message")]),
                dropdown_menu.separator([]),
                dropdown_menu.item([], [text("More...")]),
              ],
              popcicle.Hover,
            ),
            dropdown_menu.item([], [
              text("New Team"),
              dropdown_menu.shortcut([], [text("⌘+T")]),
            ]),
            dropdown_menu.item([], [text("GitHub")]),
            dropdown_menu.item([], [text("Support")]),
            dropdown_menu.item([attribute.disabled(True)], [text("API")]),
            dropdown_menu.separator([]),
            dropdown_menu.item([], [
              text("Log out"),
              dropdown_menu.shortcut([], [text("⇧⌘Q")]),
            ]),
          ],
          popcicle.Click,
        ),
        a([href("https://github.com/dinkelspiel/popcicle")], [
          button([class(light_button_styles())], [text("GitHub")]),
        ])
      ]),
      a([class("text-center underline text-zinc-400 text-sm cursor-pointer"), href("https://hexdocs.pm/popcicle/")], [
        text("Documentation")
      ])
    ]),
  ])
}

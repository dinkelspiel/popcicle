import lustre/attribute.{type Attribute, attribute}
import lustre/element/svg

pub fn chevron_right(attributes: List(Attribute(a))) {
  svg.svg(
    [
      attribute("stroke-linejoin", "round"),
      attribute("stroke-linecap", "round"),
      attribute("stroke-width", "2"),
      attribute("stroke", "currentColor"),
      attribute("fill", "none"),
      attribute("viewBox", "0 0 24 24"),
      attribute("height", "24"),
      attribute("width", "24"),
      ..attributes
    ],
    [svg.path([attribute("d", "m9 18 6-6-6-6")])],
  )
}

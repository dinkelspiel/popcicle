import { toList, Ok, Error } from "./gleam.mjs"
export function initPopcicle(config)
{
  if(globalThis.popcicle)
    return;

  globalThis.popcicle = {
    popups: [],
    config
  }
}

export function getPopcicleConfig()
{
  try {
    if(globalThis.popcicle.config)
    {
      return new Ok(globalThis.popcicle.config)
    } else {
      return new Error(void 0)
    }
  } catch {
    return new Error(void 0)
  }

}

export function getPopcicles()
{
  return toList(globalThis.popcicle.popups)
}

export function registerPopcicle(a)
{
  if(!globalThis.popcicle)
    throw new Error("Popcicle must be initialized before calling register")

  const id = globalThis.popcicle.popups.length
  globalThis.popcicle.popups.push([id, a])

  return id
}

export function getFirstParentWithAttrib(elem, attrib)
{
  let currentElement = elem
  let depth = 0

  while (currentElement && depth < 20)
  {
    if(currentElement.hasAttribute(attrib)) {
      return new Ok(currentElement) 
    }

    currentElement = currentElement.parentElement
    depth++
  }

  return new Error(void 0)
}

function getElementBoundingBox(elem, index) {
  try {
    const value = elem.getBoundingClientRect()[index]
    if(value !== undefined)
    {
      return new Ok(value)
    } else {
      return new Error(void 0)
    }
  } catch {
    return new Error(void 0)
  }
}

export function getElementLeft(elem) {
  return getElementBoundingBox(elem, "left")
}
export function getElementRight(elem) {
  return getElementBoundingBox(elem, "right")
}
export function getElementTop(elem) {
  return getElementBoundingBox(elem, "top")
}
export function getElementBottom(elem) {
  return getElementBoundingBox(elem, "bottom")
}
export function getElementWidth(elem) {
  return getElementBoundingBox(elem, "width")
}
export function getElementHeight(elem) {
  return getElementBoundingBox(elem, "height")
}

export function isMouseOver(event, element)
{
  const rect = element.getBoundingClientRect();
  return event.clientX >= rect.left && event.clientX <= rect.right &&
    event.clientY >= rect.top && event.clientY <= rect.bottom
}

export function contains(element, other) {
  return element.contains(other);
}


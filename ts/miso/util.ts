/* various utilities */
export function callFocus(id: string, delay: number): void {
  var setFocus = function () {
    var e = document.getElementById(id);
    if (e && e.focus) e.focus();
  };
  delay > 0 ? setTimeout(setFocus, delay) : setFocus();
}

export function callBlur(id: string, delay: number): void {
  var setBlur = function () {
    var e = document.getElementById(id);
    if (e && e.blur) e.blur();
  };
  delay > 0 ? setTimeout(setBlur, delay) : setBlur();
}

export function setBodyComponent(componentId: string): void {
  document.body.setAttribute('data-component-id', componentId);
}

export function hasPrototypeTagged(name: string, value: object): boolean {
  if (typeof window !== 'undefined') {
    const type = window[name];

    if (type && (value instanceof type)) {
      return true;
    }
  }

  let target = value;

  while (target) {
    const proto = Object.getPrototypeOf(target);
    const constructorName = proto.constructor.name;
    
    if (constructorName === name) {
      return true;
    } else if (constructorName === 'Object') {
      return false;
    }

    target = proto;
  }

  return false;
}

export const version : string = '1.9.0.0';

import React from 'react';
import { ReactComponent as SvgArrowDown } from './icons/keyboard_arrow_down.svg';
import { ReactComponent as SvgArrowRight } from './icons/keyboard_arrow_right.svg';
import { ReactComponent as SvgFullscreen } from './icons/fullscreen.svg';
import { ReactComponent as SvgHSplit } from './icons/horizontal_split.svg';
import { ReactComponent as SvgVSplit } from './icons/vertical_split.svg';
import { ReactComponent as SvgRefresh } from './icons/refresh.svg';

/// Do not use material icon packages. The maintainers haven't updated
/// for a few years; it doesn't have latest icons.

function makeComponent(Comp) {
  return (props) => {
    let className = "icons";
    if (props.className) {
      className += ` ${props.className}`;
    }
    return <Comp {...props} className={className}/>;
  };
}

export const IconArrowDown = makeComponent(SvgArrowDown);

export const IconArrowRight = makeComponent(SvgArrowRight);

export const IconHSplit = makeComponent(SvgHSplit);

export const IconVSplit = makeComponent(SvgVSplit);

export const IconFullscreen = makeComponent(SvgFullscreen);

export const IconRefresh = makeComponent(SvgRefresh);

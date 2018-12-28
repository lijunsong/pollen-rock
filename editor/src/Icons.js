import React from 'react';
import 'material-design-icons/iconfont/material-icons.css';
import hsplit from './icons/horizontal_split.svg';

/// This file contains Icons used in the editor
function iconGen(name) {
  return (props) => (
    <span key={name} id={name}
          className="icons clickable"
          onClick={props.onClick}></span>
  );
}

export const arrowRight = (
  <i className="material-icons md18">keyboard_arrow_right</i>
);

export const arrowDown = (
  <i className="material-icons md18">keyboard_arrow_down</i>
);

export const HorizontalSplitIcon = iconGen("HorizontalSplitIcon");

export const VerticalSplitIcon = iconGen("VerticalSplitIcon");

export const FullscreenIcon = iconGen("FullscreenIcon");

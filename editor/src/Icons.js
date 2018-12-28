import React from 'react';
import 'material-design-icons/iconfont/material-icons.css';
import hsplit from './icons/horizontal_split.svg';

/// This file contains Icons used in the editor

export const arrowRight = (
  <i className="material-icons md18">keyboard_arrow_right</i>
);

export const arrowDown = (
  <i className="material-icons md18">keyboard_arrow_down</i>
);

export const horizontalSplit = (
  <svg className="icons">
    <use href={hsplit}/>
  </svg>
);

export const verticalSplit = (
  <i className="material-icons md18">vertical_split</i>
);

export const fullscreen = (
  <i className="material-icons md18">fullscreen</i>
);

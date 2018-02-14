'use strict';

/* Notify provides info and error two functions to popup messages */
class Notify {
  constructor() {
    this._ID = 'pollenNotify';
    this.$container = document.getElementById(self._ID);
    if (! this.$container) {
      this.$container = document.createElement('div');
      this.$container.id = this._ID;
      this.$container.className = 'notifyContainer';

      document.body.appendChild(this.$container);
    }
  }

  _show(msg, type, time) {
    this.$container.innerHTML = '';

    if (time == null) {
      time = 4000;
    }

    let $inner = document.createElement('div');
    $inner.innerHTML = msg;
    $inner.className = `notify-${type}`;
    this.$container.appendChild($inner);
    this.$container.style.display = 'block';
    this._countDown(time);
  }

  _countDown(time) {
    setTimeout(() => {
      this.$container.style.display = 'none';
      this.$container.innerHTML = '';
    }, time);
  }

  info(msg, time) {
    this._show(msg, 'info', time);
  }

  error(msg, time) {
    this._show(msg, 'error', time);
  }

}

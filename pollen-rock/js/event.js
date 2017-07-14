class Event {
  constructor(sender) {
    this.sender = sender;
    this.listeners = [];
  }

  attach(listener) {
    this.listeners.push(listener);
  }

  notify(args) {
    for (let listener of this.listeners) {
      listener(this.sender, args);
    }
  }
}

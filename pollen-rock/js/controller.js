// Rule of thumbs on Event subscription: 1. View directly subscribes
// to model's event.  2. View's events  to which model should
// response will be set up by Controller

class Controller {
  constructor(model, view) {
    this.model = model;
    this.view = view;

    this.setupHandlers()
      .attach();
  }

  setupHandlers() {
    return this;
  }

  attach() {
    return this;
  }

  init() {
    this.model.init();
  }
}

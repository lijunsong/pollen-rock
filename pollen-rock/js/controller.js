class Controller {
  constructor(model, view) {
    this.model = model;
    this.view = view;

    this.setupHandlers()
      .attach();
  }

  setupHandlers() {
    this.editorChangeHandler = this.autoSave();
    return this;
  }

  attach() {
    this.model.editorChangeEvent.attach(this.editorChangeHandler);
    return this;
  }

  autoSave() {
    var scheduledSave;
    return () => {
      if (scheduledSave)
        clearTimeout(scheduledSave);

      scheduledSave = setTimeout(() => {
        this.model.saveStatusChangeEvent.notify();
        this.model.save().then(v => 'saved').then(v => {
          this.model.saveStatusChangeEvent.notify(v)
        });
      }, 2000);
    }
  }

}

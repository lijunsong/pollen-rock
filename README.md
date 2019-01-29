# Pollen-Rock

Pollen-Rock provides an improved rendering server and an in-browser editor for
[pollen](http://docs.racket-lang.org/pollen/).

![pollen-rock editor](./pollen-rock/scribblings/images/side-by-side.png?raw=true "pollen-rockk editor")


## Note

This project is still at its early development stage. Feedback and
suggestions are welcome.

## Documentation

See [pollen-rock](https://docs.racket-lang.org/pollen-rock/) for documentation.


# ChangeLog
## 0.7.0
Features
 - Server provides mtime on reads and writes of a file to detect modifictions from multiple clients
 - A new design and rewrite of the front end editor. The editor provides a lot of handy features
   - Sidebar for project navigation
   - Multi-client detection to avoid silent overwrite
   - Cursor position tracking in the editor to provide tag path and tag signature
   - Enhanced editor to work better with pollen command boundaries; inserting and deleting braces become smart
   - Editor focus mode to remove UI cluster
   - Resizable vertical and horizontal preview split
   - Syntax highlight for almost all languages (auto detected by file extensions)
 - The editor can highlight and jump to the corresponding position when a piece of searchable text in preview is selected

## 0.6.0
Features
 - Server provides RESTful APIs
 - Docs are available. Yea!

## 0.5.0
Features
 - Support create/rename/delete project files on index page
 - Make editor fonts available locally on the server (network font dependencies are removed)
 - Add server side logging
 - Add `--local` option for secure access

Bug Fixes
 - fixed a bug that could cause data lose when user closes browser immediately after typing.
 - fixed a bug that TAB replaces select region with a TAB; TAB now indents a selected region

## 0.4.1
Features

 - Editor can edit Racket and HTML files
 - Editor recognizes Racket code in Pollen file (supports mixed pollen and Racket code in pm files)

Enhancement

 - Editor parser respects the command char defined in pollen.rkt and pollen/setup.


## 0.4.0
Features

 - Implement a simple autocomplete that can autocomplete tag names defined and exported in pollen.rkt

Enhancement

 - Editor now shows server errors
 - UI supports updating editor settings
 - CodeMirror upgrades to 5.27
 - Implement JSON RPC in racket to provide a manageable way to extend server features
 - Completely rewrite the editor using ES6; implement the editor in event-based MVC

Break Compatibility

 - Removed shell command panel (keep the app simple; not sure how useful it is to others)

## 0.3.2

Features

 - add theme (rename Help to Settings)

Enhancement

 - upgrade CodeMirror to 5.24

## 0.3.1

Features

 - add option (--no-shell) to disable the terminal
 - add checks on unsaved document before browser is closed

## 0.3.0

Features

 - add protocol to watch file changes
 - Implement auto-reload rendered pages

## 0.2.0

Features

- add the ability to run shell command in editor
- add a placeholder in editor for an empty article body
- add buttons "shell" and "help"

Enhancement and Bugfix

- Improve autosave: it takes place only when users stop typing
- Improve Preview: Preview rendering interacts nicely with autosave.
- Redefine layout to remove two adjacent scrollbars on Windows
- Redefine layout to display a clean layout on mobile
- Improve scrolling: scolling in Preview doesn't affect other
  components now.

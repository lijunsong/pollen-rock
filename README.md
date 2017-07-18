# Pollen-Rock

A Pollen Server and an In-Browser Editor For Pollen Publishing System.

Pollen-Rock aims to be a simple yet powerful tool to manage your
pollen projects. Essentially, it offers a front-end editor and a
back-end server to improve UX of pollen editing.

## Note

This project is still at its early development stage. Feedback and
suggestions are welcome.

## Editor

Pollen-Rock aims to offer a powerful editor in browser to help you
focus when you compose, and to find runtime racket errors when you
typeset.

To help you compose, the editor

- [x] provides a distraction-free mode
- [x] saves the file automatically for you
- [x] inserts CommandChar using `@`
- [x] provides useful syntax highlight for pollen files

To help you typeset, the editor

- [x] detects project settings (e.g. CommandChar)
- [x] warns unbalanced braces
- [x] provides document preview
- [x] reloads preview (only when pollen syntax is correct) during editing
- [x] auto-completes your tag functions
- [ ] warns runtime errors in your document
- [ ] warns undefined tag functions
- [ ] inlines Racket document

Also, the server

- [x] watches file changes and auto-reloads your pages.

*Note:*
- [x] means the feature has been implemented.
- Only Chrome and Safari are supported.

These useful features are made possible because the back-end of
Pollen-Rock is written in Racket. It understands your configuration
file `pollen.rkt`, and serves you better than tranditional editors
(like Vim, Emacs, Sublime Text, etc.).

## Project Management

Pollen-Rock is also going to provide a simple interface to add, rename,
delete files in your pollen projects.

## Installation
```
raco pkg install pollen-rock
```

## Usage

1. Run `raco pollen-rock` in your pollen project root directory.
2. Start your work in your browser

`raco pollen-rock -h` shows available options.

# ChangeLog
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

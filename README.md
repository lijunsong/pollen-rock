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
- [ ] auto-completes your tag functions
- [ ] warns runtime errors in your document
- [ ] warns undefined tag functions
- [ ] inlines Racket document

Also, the server

- [x] watches file changes and auto-reloads your pages.

*Note:*
- [x] means the feature has been implemented.

These useful features are made possible because the back-end of
Pollen-Rock is written in Racket. It understands your configuration
file `pollen.rkt`, and serves you better than tranditional editors
(like Vim, Emacs, Sublime Text, etc.).

## Project Management

In the editor, you can run shell command. This means that you can
manage your Pollen projects directly from the editor, which makes
Pollen-Rock suitable to run as a service on your home server (if you
have any).

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
## 0.3.1 (03/07/2017)

Features

 - add option (--no-shell) to disable the terminal
 - add checks on unsaved document before browser is closed

## 0.3.0 (03/07/2017)

Features

 - add protocol to watch file changes
 - Implement auto-reload rendered pages

## 0.2.0 (03/06/2017)

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

# Pollen-Rock

Pollen-Rock provides an improved rendering server and an in-browser editor for [pollen](http://docs.racket-lang.org/pollen/).

## Note

This project is still at its early development stage. Feedback and
suggestions are welcome.

## Pollen Server

The main contribution of Pollen-Rock is its rendering server: the
server can respond to various HTTP POST requests in [JSON RPC](http://json-rpc.org/);
your program can dynamically fetch useful project specific information
from the server.

Why is this useful? Take the built-in editor as an example. The editor talks
to the server before opening a pm file. It
asks the server to gether project info (e.g. list all functions exported from
pollen.rkt) and uses these info when you typing (e.g. auto-complete known function names).

## Editor

Pollen-Rock also comes with a built-in editor that makes it easy to edit
text mixed with racket code.

To help you compose your prose, the editor

- [x] provides a distraction-free mode
- [x] saves the file automatically for you
- [x] inserts CommandChar using `@`
- [x] provides useful syntax highlight for pollen files

To help typeset, the editor

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
- The built-in editor supports only Chrome and Safari (Other browsers are not tested)

These useful features are made possible because the back-end of
Pollen-Rock is written in Racket. It understands your configuration
file `pollen.rkt`, and serves you better than tranditional editors
(like Vim, Emacs, Sublime Text, etc.).

## Project Management

(Not yet implemented)

Pollen-Rock is also going to provide a simple interface to add, rename,
delete files in your pollen projects.

## Installation
```
raco pkg install pollen-rock
```

## Usage

`raco pollen-rock -h` shows available options.

### Start Server

Run `raco pollen-rock` in your pollen project root directory.

```
$ raco pollen-rock
Your Web application is running at http://localhost:8000.
Stop this program at any time to terminate the Web Server.
```

Open http://localhost:8000 in your browser. Your browser will display an index page that lists all files in your project.

### Use built-in editor

The built-in editor supports only Racket (.rkt), HTML (.html), pollen files (.pm, .pp, .p). When you click a file name on the index page, server opens the editor only when the opening file is editable (supported). Otherwise your browser will display a page showing simple plain text.

The editor comes with preview and auto reload preview when you modifies the file. You can turn off auto reload preview in settings.

You can also see what key bindings are provided in settings. For example, autocomplete by default is `Ctrl-Space` (only when the cursor is after a command char). `@` is used to insert either a command char or a `@`.

(Key binding customization is not implemented so far)

### Watch file changes

When the built-in editor is not sufficient for you, you can always switch back to your favorite editor. Pollen-Rock can watch changes made to pollen files, and refresh the rendered HTML in your browser.

On the index page, pollen files will have a **watch** icon on the right. Clicking the icon will open a rendered page. When you make changes to the watching file, the rendered page will refresh automatically.

## Known Issues

 - Opening multiple built-in editors to edit the same file will result in data loss.
 - Editor settings are ephemeral; closing browser will reset all settings.

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

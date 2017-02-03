# pollen-rock

A Project Management Tool For Pollen Publishing System.

Pollen Rock aims to be a simple yet powerful tool to manage your
pollen projects. Essentially, it offers a front-end editor and a
back-end server to improve UX of pollen editing.

## Note

This project is still in its prototyping phase. It's not ready for
serious use yet.

## Editor

Pollen-Rock aims to offer a powerful editor in browser to help you
focus when you compose, and to find runtime racket errors when you
typeset.

To help you compose, the editor

- [x] provides a distraction-free mode (Cmd-Enter)
- [x] saves the file automatically for you
- [x] inserts CommandChar using `@` (and two CmdChar inserts `@`)

To help you typeset, the editor

- [x] provides useful syntax highlight for pollen files
- [ ] auto-completes your tag functions
- [x] warns unbalanced braces
- [ ] warns runtime errors in your document
- [ ] warns undefined tag functions
- [ ] Inlines Racket document
- [ ] provides file preview in Real-time

*Note:*
- [x] means the feature has been implemented.

These useful warnings are possible because the back-end of Pollen-Rock
is written in Racket. It understands your configuration file
`pollen.rkt`, and serves you better than tranditional editors (like
Vim, Emacs, Sublime Text, etc.).

## Project Management

Pollen-Rock is also going to provide a simple interface to add, rename,
delete files in your pollen projects

# Usage

1. Run `racket /path/to/pollen-rock/main.rkt` in your pollen project root
directory.
2. Start your work in your browser

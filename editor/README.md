## Workflow

Make sure you're in the editor/ directory before running these commands.

1. Install Node.js packages: `npm install`
2. Install Elm as a global node package: `npm install -g elm`
3. install Elm packages: `elm package install`
4. Start development environment (start the server, build the target files, and use elm-live to listen to file change to rebuild the target files): `npm start`
5. Build Elm files: `npm run build`
6. visit localhost:8000/dashboard to get started

## Directory

src/
    Elm source files
public/
    Static files and the application generated from Elm

## NOTES

To debug routes `export DEBUG=express-urlrewrite` before running api.


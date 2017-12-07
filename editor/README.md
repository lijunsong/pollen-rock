## Workflow

1. Install Node.js packages: `npm install`
2. install Elm packages: `elm package install`
3. Start development environment: `npm start`
4. Build Elm files: `npm run build`

## Directory

mock/
    A JSON database which contains a fake API that responds to GET/POST requests for development and testing purposes
src/
    Elm source files
public/
    Static files and the application generated from Elm

## NOTES

To debug routes `export DEBUG=express-urlrewrite` before running api.


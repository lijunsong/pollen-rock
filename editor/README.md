Pollen-rock Editor was bootstrapped with [Create React App](https://github.com/facebook/create-react-app).

To work on the editor, you'll need

- nodejs
- pollen-rock installed and running.

To install all npm packages, run the following command in editor's
root director:

```
npm install
```

## Available Scripts

In the project directory, you can run:

### `npm start`

Before running this script, you'll need pollen-rock running at port 8000

```
raco pollen-rock --local -p 8000
```

`npm start` runs the app in the development mode, talking to
pollen-rock at point 8000.

If pollen-rock is running in a different server (running WITHOUT
`--local` option), you can use `npm run starteditor` with
`REACT_APP_DEV_REMOTE` env var to let the editor
connects to a different remote. Take a look at package.json file.

```
REACT_APP_DEV_REMOTE=http://myweb.com npm run starteditor
```

Open [http://localhost:3000](http://localhost:3000) to view it in the
browser.

The page will reload if you make edits. You will also see any lint
errors in the console.

### `npm test`

Launches the test runner in the interactive watch mode.<br>
See the section about [running tests](https://facebook.github.io/create-react-app/docs/running-tests) for more information.

### `npm run build`

Builds the app for production to the `build` folder.<br>
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.<br>
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.

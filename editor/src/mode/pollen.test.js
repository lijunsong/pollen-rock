import CodeMirror from 'codemirror';
import './pollen';

/// NOTE for developer: in each test, you can console.log the state
/// object or token to pretty print the object

/// Inject necessary method so codemirror can run successfully
global.navigator = 'gecko';

document.body.createTextRange = function() {
  return {
    setEnd: function(){},
    setStart: function(){},
    getClientRects: function(){
      return {right: 0};
    },
    getBoundingClientRect: function(){
      return {right: 0};
    }

  };
};

function getCM(text) {
  let div = document.createElement('div');
  let cm = CodeMirror(div,{
    value: text,
    mode: 'pollen',
  });
  return cm;
}

test('Test match brace', () => {
  let cm = getCM('◊tag{data}');
  let state = cm.getStateAfter(0);
  expect(state).toEqual({braceStack: []});
});

test('Test token', () => {
  let cm = getCM('◊tag{data}');
  let token = cm.getTokenAt({line: 0, ch: 1});
  expect(token.type).toEqual("keyword");
  expect(token.string).toEqual("◊tag");
});

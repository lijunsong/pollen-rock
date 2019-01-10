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


test('stack tracks {. syntax error.', () => {
  let cm = getCM('◊func{data');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(1);
});


test('stack tracks }. syntax okay', () => {
  let cm = getCM('◊func{data}');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
});


test('{ as normal text. syntax okay', () => {
  let cm = getCM('◊func {data');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
});

test('{ in context. syntax error', () => {
  let cm = getCM('◊func{data { }');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(1);
});

test('{ matches only with }. syntax okay', () => {
  let cm = getCM('◊func{data}|');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
  let token = cm.getTokenAt({line: 0, ch:11});
  expect(token.string).toEqual("|");
});

test('get tag', () => {
  let cm = getCM('◊func{data}');
  let token = cm.getTokenAt({line: 0, ch: 1});
  expect(token.type).toEqual("keyword");
  expect(token.string).toEqual("◊func");
});

test('eat ; as a tag', () => {
  let cm = getCM('◊;{data}');
  let token = cm.getTokenAt({line: 0, ch: 1});
  expect(token.string).toEqual("◊;");
});

test('eat the char in partial stream', () => {
  let cm = getCM('a ◊');
  let token = cm.getTokenAt({line: 0, ch: 3});
  expect(token.type).toEqual(null);
  expect(token.string).toEqual("◊");
});

test('eat partial stream', () => {
  let cm = getCM('a ◊partial');
  let token = cm.getTokenAt({line: 0, ch: 3});
  expect(token.type).toEqual("keyword");
  expect(token.string).toEqual("◊partial");
});

/// Now test |{

test('stack tracks |{. syntax error.', () => {
  let cm = getCM('◊func|{data');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(1);
});

test('simple { in |{ is not a nest context', () => {
  let cm = getCM('◊func|{data {text');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(1);
});

test('simple { in |{ is not a nest context', () => {
  let cm = getCM('◊func|{data {text }|');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
});

test('|{ in { is not a block-brace, part1', () => {
  let cm = getCM('◊func{ |{ eh');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(2);

});

test('|{ in { is not a block-brace, part2', () => {
  let cm = getCM('◊func{ |{ eh }');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(1);

});

test('|{ in { is not a block-brace, part3', () => {
  let cm = getCM('◊func{ |{ eh } }');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);

});

test('stack tracks }|. syntax okay', () => {
  let cm = getCM('◊func|{data}|');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
});


test('|{ as normal text. syntax okay', () => {
  let cm = getCM('◊func |{data');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
});

test('|{ can contain {. syntax okay', () => {
  let cm = getCM('◊func|{data { }|');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
});


test('get tag', () => {
  let cm = getCM('◊func|{data}|');
  let token = cm.getTokenAt({line: 0, ch: 1});
  expect(token.type).toEqual("keyword");
  expect(token.string).toEqual("◊func");
});

/// Test comments

test('; is comment style', () => {
  let cm = getCM('◊;|{data}|');
  let token = cm.getTokenAt({line: 0, ch: 1});
  expect(token.type).toEqual("comment");
});

test('; followed by |{ allows {', () => {
  let cm = getCM('◊;|{data { { { }|');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
});

test('; followed by |{ allows }', () => {
  let cm = getCM('◊;|{data  } } } }|');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(0);
});


test('; without brace', () => {
  let cm = getCM('◊; data  } } } }|');
  let token = cm.getTokenAt({line: 0, ch: 3});
  expect(token.type).toEqual("comment");
});

test('; without brace shouldnot affect other lines', () => {
  let cm = getCM(`◊; data  } } } }|
line2`);
  let token = cm.getTokenAt({line: 0, ch: 2});
  expect(token.type).toEqual("comment");

  token = cm.getTokenAt({line: 1, ch: 2});
  expect(token.string).toEqual("i");
  expect(token.type).toEqual(null);
});

test('; followed by |{ is comment', () => {
  let cm = getCM('◊;|{data  } } } }|');
  let token = cm.getTokenAt({line: 0, ch: 4})
  expect(token.type).toEqual("comment");
});

test('; followed by { is comment', () => {
  let cm = getCM('◊;{data |{');
  let state = cm.getStateAfter(0)
  expect(state.braceStack.length).toEqual(2);
});

test('; followed by { is comment', () => {
  let cm = getCM('◊;{data |{ } }| good');
  let token = cm.getTokenAt({line: 0, ch: 14})
  expect(token.string).toEqual('|');
  expect(token.type).toEqual(null);
});

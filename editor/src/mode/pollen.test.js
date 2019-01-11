import CodeMirror from 'codemirror';
import './pollen';

/// NOTE for developer: in each test, you can console.log the state
/// object or token to pretty print the object. Also print out all
/// tokens in a line can help debugging: cm.getLineTokens(1, true)


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

function debugTokens(cm, line) {
  let tokens = cm.getLineTokens(line);
  for (let t of tokens) {
    console.log(t);
    console.log(t.state.braceStack);
  }
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
  let token = cm.getTokenAt({line: 0, ch:12});
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
  expect(state.braceStack.topBrace()).toEqual("{");
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

test('} does not close |{', () => {
  let cm = getCM('◊func|{data} ');
  let state = cm.getStateAfter(0);
  expect(state.braceStack.length).toBe(1);
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

  let state = cm.getStateAfter(1);
  expect(state.braceStack.length).toBe(0);

  let token = cm.getTokenAt({line: 0, ch: 2});
  expect(token.type).toEqual("comment");

  token = cm.getTokenAt({line: 1, ch: 2});
  expect(token.type).toEqual(null);
});


test('; followed by |{ starts comment', () => {
  let cm = getCM('◊;|{data  } } } }|');
  let token = cm.getTokenAt({line: 0, ch: 5});
  expect(token.type).toEqual("comment");
});

test('; followed by { starts comment', () => {
  let cm = getCM('◊;{data |{');
  let state = cm.getStateAfter(0)
  expect(state.braceStack.length).toEqual(2);

  let token = cm.getTokenAt({line: 0, ch: 9});
  expect(token.string).toEqual('|');
  expect(token.type).toEqual("comment");
});

test('|{ in ; is just simple left brace', () => {
  let cm = getCM('◊;{data |{ } }| good');
  let token = cm.getTokenAt({line: 0, ch: 15})
  // the | in }| should be normal text
  expect(token.string).toEqual('|');
  expect(token.type).toEqual(null);

  token = cm.getTokenAt({line: 0, ch: 12})
  // the } in the middle should be a comment
  expect(token.string).toEqual('}');
  expect(token.type).toEqual("comment");
});

test('tag in comments should not be a keyword', () => {
  let cm = getCM('◊;{◊foo{hello}}');
  let token = cm.getTokenAt({line: 0, ch: 5})
  // racket runtime completely ignores the inside tag
  expect(token.string).toEqual('foo');
  expect(token.type).toEqual("comment");
});


test('regression: left brace in comments', () => {
  let cm = getCM('◊;|{}}|');
  let token = cm.getTokenAt({line: 0, ch: 3})
  expect(token.string).toEqual('|{');
  expect(token.type).toEqual("comment");
});

//◊;{h |{ }  }

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

function last(arr) {
  return arr[arr.length - 1] || null;
}

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
    console.log(t.state.context);
    console.log(JSON.stringify(t.state));
  }
}

test('check after tag position', () => {
  let cm = getCM('abc ◊p{◊tag2[datum2]{◊i{i} def} ghi} jkl');
  let token = cm.getTokenAt({line: 0, ch: 1});
  expect(token.state.tagStack.length).toBe(0);

  // check def tag stack
  token = cm.getTokenAt({line: 0, ch: 28});
  expect(last(token.state.tagStack)).toEqual('tag2');

  // check ghi
  token = cm.getTokenAt({line: 0, ch: 34});
  expect(last(token.state.tagStack)).toEqual('p');

  // check jkl
  token = cm.getTokenAt({line: 0, ch: 40});
  expect(token.state.tagStack.length).toEqual(0);

});


test('stack tracks {. syntax error.', () => {
  let cm = getCM('◊func{data data2');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(1);
});

test('stack tracks }. syntax okay', () => {
  let cm = getCM('◊func{data}');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(0);
});


test('{ as normal text. syntax okay', () => {
  let cm = getCM('◊func {data');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(0);
});

test('{ in context. syntax error', () => {
  let cm = getCM('◊func{data { }');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(1);
});

test('{ matches only with }. syntax okay', () => {
  let cm = getCM('◊func{data}|');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(0);
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
  expect(state.braceCount).toBe(0);
  expect(state.blockBraceCount).toBe(1);
});

test('simple { in |{ is not a nest context', () => {
  let cm = getCM('◊func|{data {text');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(0);
  expect(state.blockBraceCount).toBe(1);
});

test('simple { in |{ is not a nest context', () => {
  let cm = getCM('◊func|{data {text }|');
  let state = cm.getStateAfter(0);
  expect(state.blockBraceCount).toBe(0);
  expect(state.braceCount).toBe(0);
});

test('|{ in { is not a block-brace, part1', () => {
  let cm = getCM('◊func{ |{ eh');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(2);
  expect(state.blockBraceCount).toBe(0);
});

test('|{ in { is not a block-brace, part2', () => {
  let cm = getCM('◊func{ |{ eh }');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(1);

});

test('|{ in { is not a block-brace, part3', () => {
  let cm = getCM('◊func{ |{ eh } }');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(0);

});

test('stack tracks }|. syntax okay', () => {
  let cm = getCM('◊func|{data}|');
  let state = cm.getStateAfter(0);
  expect(state.blockBraceCount).toBe(0);
  expect(state.braceCount).toBe(0);
});


test('|{ as normal text. syntax okay', () => {
  let cm = getCM('◊func |{data');
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toBe(0);
  expect(state.blockBraceCount).toBe(0);
});

test('|{ can contain {. syntax okay', () => {
  let cm = getCM('◊func|{data { }|');
  let state = cm.getStateAfter(0);
  expect(state.blockBraceCount).toBe(0);
  expect(state.braceCount).toBe(0);
});

test('} does not close |{', () => {
  let cm = getCM('◊func|{data} ');
  let state = cm.getStateAfter(0);
  expect(state.blockBraceCount).toBe(1);
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
  expect(state.braceCount).toBe(0);
  expect(state.blockBraceCount).toBe(0);
});

test('; followed by |{ allows }', () => {
  let cm = getCM('◊;|{data  } } } }|');
  let state = cm.getStateAfter(0);
  expect(state.blockBraceCount).toBe(0);
  expect(state.braceCount).toBe(0);
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
  expect(state.blockBraceCount).toBe(0);

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
  let state = cm.getStateAfter(0);
  expect(state.braceCount).toEqual(2);

  let token = cm.getTokenAt({line: 0, ch: 9});
  expect(token.string).toEqual('|');
  expect(token.type).toEqual("comment");
});

test('|{ in ; is just simple left brace', () => {
  let cm = getCM('◊;{data |{ } }| good');
  let token = cm.getTokenAt({line: 0, ch: 15});
  // the | in }| should be normal text
  expect(token.string.startsWith("|")).toBe(true);
  expect(token.type).toEqual(null);

  token = cm.getTokenAt({line: 0, ch: 12});
  // the } in the middle should be a comment
  expect(token.string).toEqual('}');
  expect(token.type).toEqual("comment");

  token = cm.getTokenAt({line: 0, ch: 14});
  // the } at the last should still be a comment
  expect(token.string).toEqual('}');
  expect(token.type).toEqual("comment");
});

test('tag in comments should be in tagStack', () => {
  let cm = getCM('◊;{◊foo{hello}}');
  let token = cm.getTokenAt({line: 0, ch: 5});
  // should be comment
  expect(token.type).toEqual("comment");
  // the tagStack should contain foo
  expect(token.state.tagStack.length).toBe(2);
});


test('regression: left brace in comments', () => {
  let cm = getCM('◊;|{}}|');
  let token = cm.getTokenAt({line: 0, ch: 3});
  expect(token.string).toEqual('|{');
  expect(token.type).toEqual("comment");
});

test('nested comments', () => {
  let cm = getCM('◊;{abc ◊;{def} ghi}');
  let token = cm.getTokenAt({line: 0, ch: 5});
  expect(token.type).toEqual('comment');

  token = cm.getTokenAt({line: 0, ch: 11});
  expect(token.type).toEqual('comment');

  token = cm.getTokenAt({line: 0, ch: 17});
  expect(token.type).toEqual('comment');
});


test('cmd has [] form', () => {
  let cm = getCM('◊tag1[datum]{test1}');
  let token = cm.getTokenAt({line: 0, ch: 15});
  expect(token.state.braceCount).toBe(1);
});

test('nested cmd in [] form', () => {
  let cm = getCM('◊tag1[◊tag2{data1}]{test1}');
  let token = cm.getTokenAt({line: 0, ch: 5});
  expect(last(token.state.tagStack)).toEqual('tag1');

  token = cm.getTokenAt({line: 0, ch: 14});
  expect(last(token.state.tagStack)).toEqual('tag2');

  token = cm.getTokenAt({line: 0, ch: 23});
  expect(last(token.state.tagStack)).toEqual('tag1');

});

test('nested cmd in {} form', () => {
  let cm = getCM('◊tag1[datum1]{◊tag2[datum2]{text2}}');
  let token = cm.getTokenAt({line: 0, ch: 9});
  expect(last(token.state.tagStack)).toEqual('tag1');

  token = cm.getTokenAt({line: 0, ch: 17});
  expect(last(token.state.tagStack)).toEqual('tag2');

  token = cm.getTokenAt({line: 0, ch: 22});
  expect(last(token.state.tagStack)).toEqual('tag2');

});

test('check after tag position', () => {
  let cm = getCM('abc ◊p{◊tag2[datum2]{◊i{i} def} ghi} jkl');
  let token = cm.getTokenAt({line: 0, ch: 1});
  expect(token.state.tagStack.length).toBe(0);

  // check def tag stack
  token = cm.getTokenAt({line: 0, ch: 28});
  expect(last(token.state.tagStack)).toEqual('tag2');

  // check ghi
  token = cm.getTokenAt({line: 0, ch: 34});
  expect(last(token.state.tagStack)).toEqual('p');

  // check jkl
  token = cm.getTokenAt({line: 0, ch: 40});
  expect(token.state.tagStack.length).toEqual(0);

});

#lang scribble/manual

@(require (for-label racket))
@(require scribble/decode)
@(require scribble/core)
@(require racket/runtime-path)
@(require scribble/html-properties)

@(define paramlist itemlist)
@(define (param
           #:name name
           #:type type
           #:optional [optional #f]
           . pre-content)
   (define optional-text (if optional " (optional)" ""))
   (define lead (tt (format "~a: ~a~a. "
                            name type optional-text)))
   (apply item
    `(,lead " " ,@pre-content)))

@(define (param-errno . str)
   (apply param str #:name "errno" #:type "int"))

@(define-runtime-path scribble-root ".")

@(define (use-image name scale)
  (let ((path (build-path scribble-root "images" name)))
    (image path #:scale scale)))

@title{Pollen-rock: an improved Pollen server}

Pollen-rock improves Pollen web server by providing an interface for querying project configuration and file information. It also comes with a convenient in-browser editor that helps you compose and typeset your prose.

@;other-doc['(lib "pollen/scribblings/pollen.scrbl") #:indirect "Pollen"]

Pollen-rock is simply an add-on of Pollen rendering server. This new rendering server does not change Pollen language's semantics. Hate Pollen being silent on your undefined tag functions? You'll find the same behavior in Pollen-rock as well, because Pollen-rock uses Pollen to render your file. But Pollen-rock's built-in editor has the ability to inform you about that undefined tag, and you can make the decision what to do about it.

Internally, Pollen-rock provides a RESTful web API for querying various information of a Pollen project and source files. If you're interested in writing your tools using the RESTful API, continue to @secref["server-spec"]. If you're interested in Pollen-rock built-in editor, continue to @secref["installation"].

@local-table-of-contents[]

@section[#:tag "installation"]{Installation}

Pollen-rock is an add-on of Pollen. Installing pollen-rock is as simple as running the following command in your terminal

@codeblock{
 raco pkg install pollen-rock
}

Racket will handle package dependencies for you.

For those who haven't installed Pollen before, you can follow the complete guide @secref["How_to_install" #:doc '(lib "pollen/scribblings/pollen.scrbl")] of Pollen to install Racket and Pollen, and then follow the above step.

@section[#:tag "use-server"]{Use Pollen-rock}

Working with Pollen-rock is similar to Pollen's @secref["The_project_server" #:doc '(lib "pollen/scribblings/pollen.scrbl")].

@subsection{Start the Server}

To start, issue the following command in your terminal.

@codeblock{
$ raco pollen-rock --start-dir ~/workspace/blog
}

And you'll see the following output

@verbatim{
Welcome to Pollen Rock 0.6.0 (Racket 6.8)
Project root is /home/user/workspace/blog/
Pollen Editor is running at http://localhost:8000/dashboard (accessible by all machines on your network)
Ctrl-C at any time to terminate Pollen Rock.
}

If @code{--start-dir} argument is not specified, the project root will be current working directory.

@margin-note{All machines on your network will be accessible to your project directory. This is dangerous; other people who know your IP can issue HTTP POST request remotely to remove your project data. To limit the access scope to your own machine, specify @code{--local} in the argument.}

@subsection{Dashboard}

The dashboard is at @code{http://localhost:8000/dashboard}.

On the dashboard, you can

@itemlist[
@item{Navigate in directories}
@item{Render Pollen source files}
@item{Open an in-browser editor to edit files}
@item{Change editor settings}
]

@subsection{Render}

On the dashboard, you can click Render to render a Pollen source file.

When Pollen-rock starts to render a file, it actually does two things: It renders and shows the rendered page in the browser, and it watches the source code changes and reloads the rendered page.


This is convenient when you open your editor and the browser side by side

@(use-image "side-by-side.png" 0.4)

There are a few things you should be aware of:

@itemlist[
@item{Pollen-rock doesn't know the dependencies of your rendered result; if Pollen-rock watches source file @tt{file1.html.pm}, who specifies link src @tt{style.css}. Your change to @tt{style.css} is not going to trigger the reload.}
@item{Pollen-rock watches the file content as well as file attributes.}
]


@subsection{Use the Editor}

On the dashboard, you can also click "Edit" to edit a file.

Pollen-rock editor currently supports syntax highlight of only a few common file types. The dashboard shows "Edit" only for supported files.

The editor saves your change automatically, and the header area shows the saving status. Once the saving status has become @emph{saved}, it's safe to close the browser. It will warn you when you close the browser if there are unsaved contents.

In the header area, you can also see

@itemlist[
@item{@tt{Render}: split the view. It can cycle through horizontal and vertical split view, and automatically render the file and reload the rendered page when the source code syntax is correct}
@item{@tt{Settings}: open settings page. Any changes to the setting will take effect immediately}
]

@;(image (build-path scribble-root "images" "auto-render.gif") #:style "width:100%; height:100%")
@(use-image "auto-render.gif" 0.8)


@section[#:tag "server-spec"]{Pollen-rock Server Specification}

Pollen-rock server provides
a set of RESTful APIs for the front end to query project information.

@margin-note{Note: these APIs and this doc are still under construction. They are subject to change at this point.}

@subsection[#:tag "working-example"]{Query the Server}

All API resource starts with @code{/rest/}. Let's take the following project structure as an working example.

@verbatim|{
/tmp/project/
  |- pollen.rkt
  |- file1.html.pm
  |- file2.html.pm
  |- dir1/
     |- file3.html.pm
  |- dir2/
}|

@margin-note{All JSON output in this section are additionally rendered by pretty print. The command output is generated by passing the JSON into @code{python -m json.tool}, as in @tt{curl http://localhost:8000/rest/fs/ | python -m json.tool}}

@code{file1.html.pm} and @code{file2.html.pm} are two pollen source files. pollen.rkt has the following contents

@verbatim|{
#lang racket

(provide (all-defined-out))

(define x 1)

(define (tag0 x . y)
  `(mytag ,x ,@y))
}|

Now open a terminal, and start pollen-rock

@verbatim{
$ raco pollen-rock
}

In another terminal, we can start querying the server. Let's list the project root directory

@verbatim|{
$ curl http://localhost:8000/rest/fs
{
  "errno": 0,
  "items": [
    "compiled/",
    "dir1/",
    "dir2/",
    "file1.html.pm",
    "file2.html.pm",
    "pollen.rkt"
  ]
}
}|

What about the @code{dir1} directory

@verbatim|{
$ curl http://localhost:8000/rest/fs/dir1
{
  "errno": 0,
  "items": [
    "file3.html.pm"
  ]
}
}|

What about listing some directory called @code{non-exists}?

@verbatim|{
$ curl http://localhost:8000/rest/fs/non-exists
{
  "errno": 1
}
}|

Let's read the contents of @code{file3.html.pm}

@verbatim|{
$ curl http://localhost:8000/rest/fs/dir1/file3.html.pm
{
  "errno": 0,
  "contents": "#lang pollen\n\nfile3 contents\n"
}
}|

Let's get all tags visible in @code{file3.html.pm}.

@verbatim|{
$ curl http://localhost:8000/rest/tags/dir1/file3.html.pm
{
  "errno": 0,
  "tags": [
    {
      "arity-at-least": true,
      "kind": "procedure",
      "arity": 1,
      "required-keywords": [

      ],
      "name": "tag0",
      "all-keywords": [

      ]
    },
    {
      "kind": "variable",
      "name": "x",
      "type": "number",
      "value": 1
    }
  ]
}
}|

You can tell Pollen-rock can do quite a lot here. Let's dive into the API specification in next section.

@subsection[#:tag "api"]{RESTful API}

Pollen-rock supports requests that are form-url-encoded, i.e. requests whose @code{Content-Type} is @code{application/x-www-form-urlencoded}.

For each API, we first list its designated url (the resource), and then the query parameter, the request parameter, and the response parameter. It's necessary to make it clear what and where these parameters are in the request and response:

@itemlist[
 @item{@emph{query parameter} (@emph{GET} HTTP request only): It's the @code|{{6}}| and @code|{{7}}| of the following diagram (borrowed from
 @secref["URL_Structure" #:doc '(lib "net/scribblings/net.scrbl")])
  @verbatim|{
  http://sky@www:801/cgi-bin/finger;xyz?name=shriram;host=nw#top
                                    {6} {----7-------------}
            }|
  }

 @item{@emph{request parameter} (@emph{POST} HTTP request only): @emph{request parameter} is in the payload of an HTTP POST request. Pollen-rock currently supports only @code{application/x-www-form-urlencoded}.
 }

 @item{@emph{response parameter}: Response from the server. Pollen-rock now returns only JSON. So all the parameters are keys (properties) of JSON object.}
 ]

@subsubsection{POST /rest/fs/$path}

This API is for file system operations.

Query parameter: None

Request parameter:

@paramlist[
 @param[#:name "op" #:type "string"]{
   It can be one of
   @itemlist[@item{@tt{"mv"}: rename @tt{$path} into the string given in @tt{data}}
             @item{@tt{"mkdir"}: create new directory @tt{$path}}
             @item{@tt{"rm"}: remove @tt{$path}}
             @item{@tt{"write"}: write @tt{data} into file @tt{$path}}
             ]
 }
 @param[#:name "data" #:type "string" #:optional #t]{
   extra data
 }
 ]

Response parameter:
@paramlist[
 @param[#:name "errno" #:type "int"]{0 means no error}
 @param[#:name "message" #:type "string"]{Extra message from server. Usually this contains error messages if @tt{errno} is not 0}
 ]

Examples: create directory @code{dir3}, and move @code{dir3} to @code{dir4}.

@verbatim|{
$ curl -X POST -d "op=mkdir" http://localhost:8000/rest/fs/dir3
{
    "errno": 0,
    "message": ""
}
$ curl http://localhost:8000/rest/fs/
{
    "errno": 0,
    "items": [
        "compiled/",
        "dir1/",
        "dir2/",
        "dir3/",
        "file1.html.pm",
        "file2.html.pm",
        "pollen.rkt"
    ]
}
$ curl -X POST -d "op=mv&data=dir4" http://localhost:8000/rest/fs/dir3
{
    "errno": 0,
    "message": ""
}
}|

@subsubsection{GET /rest/fs/$path}

This API is for list directory or reading files.

Query parameter: None

Response parameter:
@paramlist[
 @param-errno{0 means success. non-zero means the operation has failed}
 @param[#:name "items" #:type "string array"]{If @tt{$path} is a directory, @tt{items} contains the directory contents. All subdirectory names have @tt{/} suffix.}
 @param[#:name "contents" #:type "string"]{If @tt{$path} is a regular text file, @tt{contents} is the contents of that file.}
 ]

Examples: get the contents of @tt{file3.html.pm}

@verbatim|{
$ curl http://localhost:8000/rest/fs/dir1/file3.html.pm
{
    "contents": "#lang pollen\n\nfile3 contents\n",
    "errno": 0
}
}|

List the project root
@verbatim|{
{
    "errno": 0,
    "items": [
        "compiled/",
        "dir1/",
        "dir2/",
        "dir3/",
        "dir4/",
        "file1.html",
        "file1.html.pm",
        "file2.html.pm",
        "pollen.rkt"
    ]
}
}|

@subsubsection[#:tag "api-tags"]{GET /rest/tags/$file}

Fetch defined tags of a racket module @code{$file}. Note that @code{$file} must be a racket module.

Query parameter: None
@margin-note{TODO: It would be useful to accept a query parameter to return a specific tag}


response parameter:
@paramlist[
 @param-errno{0 means success. 1 means that error has occurred.}
 @param[#:name "tags" #:type "objects array"]{
  array of json objects containing procedure or variable information.
  @paramlist[
 @param[#:name "name" #:type "string"]{Tag name}
 @param[#:name "kind" #:type "string"]{Either @tt{"variable"} or @tt{"procedure"}.}
 @param[#:name "type" #:type "string or null"]{(Variable only) Valid values are @tt{"boolean"}, @tt{"number"}, @tt{"string"}, @tt{"char"}, @tt{"symbol"}, @tt{null}. Also see @tt{value}.}
 @param[#:name "value" #:type "any"]{(Variable only) The value of the tag. The type of the value is indicated by the @tt{type} property.}
 @param[#:name "arity" #:type "number"]{(Procedure only) This is the arity of the procedure. If @tt{arity-at-least} is @tt{true}, this value is the minimum arity that this procedure requires.}
 @param[#:name "arity-at-least" #:type "boolean"]{(Procedure only) See @tt{arity}.}
 @param[#:name "all-keywords" #:type "string array or false"]{(Procedure only) All keywords of a procedure, including optional and required keywords. If this is @tt{false}, it can accept any keywords.}
 @param[#:name "required-keywords" #:type "string array"]{(Procedure only) Required keywords.}
 ]
 }
 ]

Examples: to fetch all tags exported to @code{file1.html.pm}
@verbatim|{
$ curl http://localhost:8000/rest/tags/file1.html.pm
{
    "errno": 0,
    "tags": [
        {
            "all-keywords": [],
            "arity": 1,
            "arity-at-least": true,
            "kind": "procedure",
            "name": "tag0",
            "required-keywords": []
        },
        {
            "kind": "variable",
            "name": "x",
            "type": "number",
            "value": 1
        }
    ]
}
}|

@subsubsection{GET /rest/config/$file}

Get project configuration of the given @tt{$file}. If @tt{pollen.rkt} doesn't exist, this API fetches pre-defined tags from @secref["Setup" #:doc '(lib "pollen/scribblings/pollen.scrbl")].

Query parameter: None
@margin-note{It would be useful to accept a query parameter to return a specific config}

Response parameter: same as @secref{api-tags}. However, this API returns only variables of the setup module.

@subsubsection{GET /rest/watch/$file}
Do HTTP long polling on the given @code{$file}. The HTTP request returns only when the modified time of @tt{$file} has changed.

Query parameter:
@paramlist[
 @param[#:name "mtime" #:type "int" #:optional #t]{Return immediately if the last modified time of the file is greater than this mtime. This is parameter is not provided, the return occurs only when the modified time of the file has changed.}
 ]

Response parameter:
@paramlist[
 @param-errno{0 means file changed. 1 means no such file.}
 @param[#:name "mtime" #:type "int"]{Last modified time when errno is 0. When errno is 1, mtime can be anything}
 ]

Example: the following command would block until @tt{touch file1.html.pm} runs.

@verbatim|{
$ (sleep 10 && touch file1.html.pm) &

$ time curl http://localhost:8000/rest/watch/file1.html.pm
{"mtime":1514347321,"errno":0}

real	0m9.445s
user	0m0.012s
sys	0m0.021s
}|

@subsubsection{GET /rest/search/$file}

Search source and output file.

If the given file is a pollen source file, i.e. @tt{pp}, @tt{pm},
@tt{p}, etc., Pollen-rock always returns @tt{source} and @tt{output}
paths, and @tt{source-exists} indicates whether the source file
exists. If the given file is not a pollen source, it's treated as an
output file, and returns non-zero errno if no source files on the file
system can generate the output, and returns 0 errno and set
@tt{source} and @tt{output} accordingly, in which case,
@tt{source-exists} is always @tt{true}.

Query parameter: None

Response parameter:
@paramlist[
 @param-errno{0 means no error, non-zero means some error has occurred}
 @param[#:name "source-exists" #:type "bool"]{Whether the source exists}
 @param[#:name "source" #:type "string"]{The source path relative to project root. The value is undefined when @tt{errno} is non-zero.}
 @param[#:name "output" #:type "string"]{The output path relative to project root. The value is undefined when @tt{errno} is non-zero.}
]

Example: the following command gets the output path of one nonexistent pollen source

@verbatim|{
$ curl http://localhost:8000/rest/search/nonexist.html.pm
{
    "errno": 0,
    "output": "nonexist.html",
    "source": "nonexist.html.pm",
    "source-exists": false
}
}|

The following command demonstrates querying an output path that doesn't exist.

@verbatim|{
$ curl localhost:8000/rest/search/nonexist-dir/
{
    "errno": 1,
    "output": "",
    "source": "",
    "source-exists": false
}
}|

@subsubsection{GET /rest/render/$file}
Render the given pollen source @tt{$file}

Query parameter: None

Response parameter:
@paramlist[
 @param-errno{0 means no error, 1 means no such file, 2 means render failed.}
 @param[#:name "location" #:type "string"]{Rendered file name. The location will be always present even if render fails.}
 ]

Example: render @tt{file3.html.pm}.

@verbatim|{
$ curl http://localhost:8000/rest/render/dir1/file3.html.pm
{
    "errno": 0,
    "location": "dir1/file3.html"
}
}|

@section[#:tag "dev"]{Development}
Pollen-rock's development is on @hyperlink["http://github.com/lijunsong/pollen-rock" "github"].

Pollen-rock contains server code written in Racket and editor code written in Elm.

Once you've cloned the Pollen-rock source, you're ready to work on the server. If you're going to work on the server code, make sure you can pass all tests at any time.

@verbatim|{
raco test pollen-rock/pollen-rock
}|

@subsection{Install}

The following command will clone the repo and install (link) the repo
as a local package for development. The installation is a soft link,
so any source change can take effect immediately.

@verbatim|{
$ git clone https://github.com/lijunsong/pollen-rock.git
$ cd pollen-rock
$ raco pkg install -l
}|

If you encounter cache problem, you can run

@verbatim|{
$ raco setup -l pollen-rock
}|

to rebuild the package.

@subsection{RESTful API}

The documentation of RESTful API is available locally in Racket
documentation. The above installation will also build it. Run the
following command to open Racket Documentation:

@verbatim|{
$ raco docs
}|

Then search Pollen-rock. You won't miss it.

@subsection{Rebuild the Docs}

Pollen-rock documentation lives in @tt{pollen-rock/scribblings/}. To rebuild
the document when you make changes, run the following command

@verbatim|{
raco setup --doc-index -l pollen-rock
}|

@subsection{The Editor}

Elm development envionrment needs a bit setup. Follow steps described
in @tt{README.md} in the @tt{editor} folder.


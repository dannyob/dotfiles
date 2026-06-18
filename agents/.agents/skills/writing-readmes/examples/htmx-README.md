<!--
WHAT TO STUDY HERE (annotation by writing-readmes skill, not part of upstream README):

- **Voice over polish** - lowercase section headings ("introduction",
  "motivation", "quick start", "haiku") are a distinctive choice that
  signals a real human author, not a template. The README ends with an
  actual haiku.
- **Motivation as four specific questions** - "Why should only <a> and <form>
  be able to make HTTP requests?" - each question is concrete. Compare with
  the AI-default "modern web frameworks need to evolve" framing.
- **Quickstart is one paste-and-run snippet** - 8 lines of HTML, then a quoted
  plain-English explanation of what htmx will do when the button is clicked.
  Time-to-first-understanding is roughly 20 seconds.
- **Honest aside** - "Note there is an old broken package called htmx. This
  is htmx.org." Admits a real-world annoyance instead of pretending things
  are smoother than they are.
- **Dry and small** - "They are all OK." about the test stack. Almost no AI
  README would write that sentence.
- **Communicates philosophy without manifesto** - links to HATEOAS and
  Fielding's REST dissertation as one-word references; doesn't lecture.
- Source: https://github.com/bigskysoftware/htmx (BSD-2-Clause Zero)
-->

[![</> htmx](https://raw.githubusercontent.com/bigskysoftware/htmx/master/www/static/img/htmx_logo.1.png "high power tools for HTML")](https://htmx.org)

*high power tools for HTML*

[![Discord](https://img.shields.io/discord/725789699527933952)](https://htmx.org/discord)
[![Netlify](https://img.shields.io/netlify/dba3fc85-d9c9-476a-a35a-e52a632cef78)](https://app.netlify.com/sites/htmx/deploys)
[![Bundlephobia](https://badgen.net/bundlephobia/dependency-count/htmx.org)](https://bundlephobia.com/result?p=htmx.org)
[![Bundlephobia](https://badgen.net/bundlephobia/minzip/htmx.org)](https://bundlephobia.com/result?p=htmx.org)

## introduction

htmx allows you to access  [AJAX](https://htmx.org/docs#ajax), [CSS Transitions](https://htmx.org/docs#css_transitions),
[WebSockets](https://htmx.org/extensions/ws/) and [Server Sent Events](https://htmx.org/extensions/sse/)
directly in HTML, using [attributes](https://htmx.org/reference#attributes), so you can build
[modern user interfaces](https://htmx.org/examples) with the [simplicity](https://en.wikipedia.org/wiki/HATEOAS) and
[power](https://www.ics.uci.edu/~fielding/pubs/dissertation/rest_arch_style.htm) of hypertext

htmx is small ([~14k min.gz'd](https://cdn.jsdelivr.net/npm/htmx.org/dist/)),
[dependency-free](https://github.com/bigskysoftware/htmx/blob/master/package.json) &
[extendable](https://htmx.org/extensions)

## motivation

* Why should only `<a>` and `<form>` be able to make HTTP requests?
* Why should only `click` & `submit` events trigger them?
* Why should only GET & POST be available?
* Why should you only be able to replace the *entire* screen?

By removing these arbitrary constraints htmx completes HTML as a
[hypertext](https://en.wikipedia.org/wiki/Hypertext)

## quick start

```html
  <script src="https://cdn.jsdelivr.net/npm/htmx.org@2.0.10/dist/htmx.min.js"    
          integrity="sha384-H5SrcfygHmAuTDZphMHqBJLc3FhssKjG7w/CeCpFReSfwBWDTKpkzPP8c+cLsK+V" 
          crossorigin="anonymous"></script>
  <!-- have a button POST a click via AJAX -->
  <button hx-post="/clicked" hx-swap="outerHTML">
    Click Me
  </button>
```

The [`hx-post`](https://htmx.org/attributes/hx-post) and [`hx-swap`](https://htmx.org/attributes/hx-swap) attributes tell htmx:

> "When a user clicks on this button, issue an AJAX request to /clicked, and replace the entire button with the response"

htmx is the successor to [intercooler.js](http://intercoolerjs.org)

### installing as a node package

To install using npm:

```
npm install htmx.org --save
```

Note there is an old broken package called `htmx`.  This is `htmx.org`.

## website & docs

* <https://htmx.org>
* <https://htmx.org/docs>

## contributing
Want to contribute? Check out our [contribution guidelines](CONTRIBUTING.md)

No time? Then [become a sponsor](https://github.com/sponsors/bigskysoftware#sponsors)

### hacking guide

To develop htmx locally, you will need to install the development dependencies.

Run:

```
npm install
```

Then, run a web server in the root.

This is easiest with:

```
npx serve
```

You can then run the test suite by navigating to:

<http://0.0.0.0:3000/test/>

At this point you can modify `/src/htmx.js` to add features, and then add tests in the appropriate area under `/test`.

* `/test/index.html` - the root test page from which all other tests are included
* `/test/attributes` - attribute specific tests
* `/test/core` - core functionality tests
* `/test/core/regressions.js` - regression tests
* `/test/ext` - extension tests
* `/test/manual` - manual tests that cannot be automated

htmx uses the [mocha](https://mochajs.org/) testing framework, the [chai](https://www.chaijs.com/) assertion framework
and [sinon](https://sinonjs.org/releases/v9/fake-xhr-and-server/) to mock out AJAX requests.  They are all OK.

## haiku

*javascript fatigue:<br/>
longing for a hypertext<br/>
already in hand*

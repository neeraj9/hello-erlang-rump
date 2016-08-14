# uWiki

This project hosts an Erlang/OTP Wiki service, which exposes api for querying
wiki information around the world. This project is one of the
service running Erlang microkernel powered by the Rumprun unikernel.

The project is under active development and hence still very early stage
and not completely tested.

## Dependencies and Development Environment

This is documented in detail at [project readme](../readme.md).

## Motivation

This service provides HTTP/2.0 API for querying wiki information.
The following list of sources exist in the current codebase:

* [wikipedia](https://en.wikipedia.org)

## Build

After you are done setting up the development environment the build is
pretty straight-forward (see below).

    git clone https://github.com/neeraj9/hello-erlang-rump
    cd hello-erlang-rump
    make uwiki

## REST API

This service supports REST over HTTP/1.1 for maximum compatibility.
Additionally, the response is always json. The following endpoints
are provided within the service:

* /wiki/textsearch - Full text search for wiki.

### Wiki Text Search

This endpoint (/wiki/textsearch) can be used to search wiki for information.

* **URL**

  `/wiki/textsearch?txt=what+is+your+name`

* **Method**

  `GET`

* **URL Params**

  **Required:**

  `txt=[string]`

* **Authentication**

  Not required.

* **Data Params**

  There is no request body for this API.

* **Success Response**

  * **Code:** 200 <br />
    **Content:** `{"batchcomplete":"",
                   "continue":{"sroffset":1,"continue":"-||"},
                   "query":{"searchinfo":{"totalhits":65052},
                   "search":[{"ns":0,"title":"What's Your Name (album)",
                   "snippet":"For the Adam Sandler album, see <span class=\"searchmatch\">What</span>'s <span class=\"searchmatch\">Your</span> <span class=\"searchmatch\">Name</span>? <span class=\"searchmatch\">What</span>'s <span class=\"searchmatch\">Your</span> <span class=\"searchmatch\">Name</span> <span class=\"searchmatch\">is</span> a compilation album by American rock group Lynyrd Skynyrd. It was certified Gold",
                   "size":2141,
                   "wordcount":172,
                   "timestamp":"2016-02-16T20:56:19Z"}]}}`

* **Error Response**

  * **Code:** 500 Internal Server Error <br />
    **Content:** <EMPTY>

* **Sample Call**

  `curl -v "http://localhost:9595/wiki/textsearch?txt=what+is+your+name"`

* **Notes**

  The error cases typically return HTTP 500 but they should be as per
  the real source of error. This is something to be worked upon
  in the future.

## Thanks

Thanks for evaluating this project and hope you find it useful.
Feel free to create issues for bugs or new features.

## Authors

* Neeraj Sharma {[github: neeraj9](https://github.com/neeraj9)}

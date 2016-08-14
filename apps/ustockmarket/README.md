# ustockmarket

This project hosts an Erlang/OTP stockmarket service, which exposes api for querying
stockmarket information around the world. This project is one of the
service running Erlang microkernel powered by the Rumprun unikernel.

The project is under active development and hence still very early stage
and not completely tested.

## Dependencies and Development Environment

This is documented in detail at [project readme](../../readme.md).

## Motivation

This service provides HTTP/2.0 API for querying stockmarket information.
The following list of sources exist in the current codebase:

* [Quandl](https://www.quandl.com)

## Build

After you are done setting up the development environment the build is
pretty straight-forward (see below).

    git clone https://github.com/neeraj9/hello-erlang-rump
    cd hello-erlang-rump
    make ustockmarket

## REST API

This service supports REST over HTTP/1.1 for maximum compatibility.
Additionally, the response is always json. The following endpoints
are provided within the service:

* /stockmarket/code/NSE - Get list of stock quotes available.
* /stockmarket/quote/NSE?q=<STOCK_CODE> - Get history stock information for a
  specific NSE stock (which is one of the available stock quotes). This
  API returns all the complete history till date for that stock quote.

### stockmarket NSE Code

This endpoint (/stockmarket/code/NSE) can be used to get all the stock
codes in National Stock Exchange (NSE) India.

* **URL**

  `/stockmarket/code/NSE`

* **Method**

  `GET`

* **URL Params**

  Not required.

* **Authentication**

  Not required.

* **Data Params**

  There is no request body for this API.

* **Success Response**

  * **Code:** 200 <br />
    **Content:** `{"datasets": ["20MICRONS", "3IINFOTECH", ...]
                   }`

* **Error Response**

  * **Code:** 500 Internal Server Error <br />
    **Content:** <EMPTY>

* **Sample Call**

  `curl -v "http://localhost:9595/stockmarket/code/NSE"`

* **Notes**

  The error cases typically return HTTP 500 but they should be as per
  the real source of error. This is something to be worked upon
  in the future.

### stockmarket NSE Stock Quote

This endpoint (/stockmarket/quote/NSE) can be used to get complete stock
quotes for a specific NSE stock symbol (or code).

* **URL**

  `/stockmarket/quote/NSE?q=<STOCK_CODE>`

* **Method**

  `GET`

* **URL Params**

  **Required:**

  `q=[string]`

* **Authentication**

  Not required.

* **Data Params**

  There is no request body for this API.

* **Success Response**

  * **Code:** 200 <br />
    **Content:** `{"dataset":{"id":2366895,"dataset_code":"ZENITHEXPO",
                              "database_code":"NSE",
                              "name":"Zenith Exports Limited",
                              "description":"Historical prices...",
                              "refreshed_at":"2016-08-03T13:45:54.668Z",
                              "newest_available_date":"2016-08-02",
                              "oldest_available_date":"1998-03-12",
                              "column_names":["Date","Open","High","Low",
                                              "Last","Close",
                                              "Total Trade Quantity",
                                              "Turnover (Lacs)"],
                              "frequency":"daily",
                              "type":"Time Series",
                              "premium":false,
                              "limit":null,
                              "transform":null,
                              "column_index":null,
                              "start_date":"1998-03-12",
                              "end_date":"2016-08-02",
                              "data":[["2016-08-02",41.45,41.5,41.45,41.5,41.5,21.0,0.01],
                                      ["2016-08-01",39.25,39.55,39.2,39.55,39.55,35.0,0.01],...
                                      ["1998-03-12",37.4,37.4,37.4,null,null,null,null]],
                              "collapse":null,
                              "order":"desc",
                              "database_id":33}}`

* **Error Response**

  * **Code:** 500 Internal Server Error <br />
    **Content:** <EMPTY>

* **Sample Call**

  `curl -v "http://localhost:9595/stockmarket/quote/NSE?q=ZENITHEXPO"`

* **Notes**

  The error cases typically return HTTP 500 but they should be as per
  the real source of error. This is something to be worked upon
  in the future.

## Thanks

Thanks for evaluating this project and hope you find it useful.
Feel free to create issues for bugs or new features.

## Authors

* Neeraj Sharma {[github: neeraj9](https://github.com/neeraj9)}

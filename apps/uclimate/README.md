# uClimate

This application hosts an Erlang/OTP Climate service, which exposes api for
querying climate information around the world. This project is one of the
service running Erlang microkernel powered by the Rumprun unikernel.

The project is under active development and hence still very early stage
and not completely tested.

## Dependencies and Development Environment

This is documented in detail at [project readme](../../readme.md).

## Motivation

This service provides HTTP/2.0 API for querying world climate information.
The following list of sources exist in the current codebase:

* [openweathermap](http://openweathermap.org/)

## Build

After you are done setting up the development environment the build is
pretty straight-forward (see below).

    git clone https://github.com/neeraj9/hello-erlang-rump
    cd hello-erlang-rump
    make uclimate

## REST API

This service supports REST over HTTP/1.1 for maximum compatibility.
Additionally, the response is always json. The following endpoints
are provided within the service:

* /climate/geocircle - Query temperature/climate of cities around
  a geolocation specified by a latitude and longitude.

### Climate GeoCircle

This endpoint (/climate/geocircle) can be used to query temperature/climate of
cities around a geolocation specified by a latitude and longitude value. An
additional (though optional) city count can also be provided otherwise a
default value of 10. is used.

* **URL**

  `/climate/geocircle?lat=GeoLatitude&lon=GeoLongitude[&cnt=CityCount]`

> The cnt query string key is optional hence enclosed within the square
> brackets, otherwise square brackets are not part of the url.

* **Method**

  `GET` | `HEAD`

* **URL Params**

  **Required:**

  `lat=[float]`
  `lon=[float]`

  **Optional:**

  `cnt=[int]`

* **Authentication**

  Not required.

* **Data Params**

  There is no request body for this API.

* **Success Response**

  * **Code:** 200 <br />
    **Content:** `{"message":"accurate","cod":"200","count":1,"list":[
                    {"id":2394560,"name":"Djougou",
                     "coord":{"lon":1.66598,"lat":9.70853},
                     "main":{"temp":299.28,"temp_min":299.28,
                             "temp_max":299.28,"pressure":982.52,
                             "sea_level":1024.04,"grnd_level":982.52,
                             "humidity":87},
                     "dt":1469819046,"wind":{"speed":2.36,"deg":248.5},
                     "sys":{"country":""},"clouds":{"all":80},
                     "weather":[{"id":803,"main":"Clouds",
                                 "description":"broken clouds","icon":"04n"}]
                    }]}`

* **Error Response**

  * **Code:** 500 Internal Server Error <br />
    **Content:** <EMPTY>

* **Sample Call**

  `curl -v "http://localhost:9595/climate/geocircle?lat=10&lon=2"`

* **Notes**

  The error cases typically return HTTP 500 but they should be as per
  the real source of error. This is something to be worked upon
  in the future.

## Thanks

Thanks for evaluating this project and hope you find it useful.
Feel free to create issues for bugs or new features.

## Authors

* Neeraj Sharma {[github: neeraj9](https://github.com/neeraj9)}

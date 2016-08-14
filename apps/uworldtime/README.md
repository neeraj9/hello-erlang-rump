# uWorldtime

This project hosts an Erlang/OTP worldtime service, which exposes api for querying
time information around the world. This project is one of the
service running Erlang microkernel powered by the Rumprun unikernel.

The project is under active development and hence still very early stage
and not completely tested.

## Dependencies and Development Environment

This is documented in detail at [project readme](../../readme.md).

## Motivation

This service provides HTTP/2.0 API for querying world time information.
The following list of sources exist in the current codebase:

* [google maps api](https://developers.google.com/maps/documentation/timezone/start)
  * [google maps tz intro](https://developers.google.com/maps/documentation/timezone/intro)
* [Sunset and sunrise times API](http://sunrise-sunset.org/api)


> **TODO** Look at
> http://geoip.nekudo.com/
> https://github.com/nekudo/shiny_geoip
> https://github.com/maxmind
> https://github.com/maxmind/MaxMind-DB/blob/master/MaxMind-DB-spec.md

## Build

After you are done setting up the development environment the build is
pretty straight-forward (see below).

    git clone https://github.com/neeraj9/hello-erlang-rump
    cd hello-erlang-rump
    make uworldtime

## REST API

This service supports REST over HTTP/1.1 for maximum compatibility.
Additionally, the response is always json. The following endpoints
are provided within the service:

* /worldtime/tz - timezone at any given geolocation.
* /worldtime/suntime - today sunset and sunrise information at a given
  geolocation.

### Worldtime Timezone

This endpoint (/worldtime/tz) can be used to query for current timezone
at any given geolocation.

* **URL**

  `/worldtime/tz?lat=GeoLatitude&lon=GeoLongitude`

* **Method**

  `GET`

* **URL Params**

  **Required:**

  `lat=[float]`
  `lon=[float]`

* **Authentication**

  Not required.

* **Data Params**

  There is no request body for this API.

* **Success Response**

  * **Code:** 200 <br />
    **Content:** `{
                     "dstOffset" : 3600,
                     "rawOffset" : -18000,
                     "status" : "OK",
                     "timeZoneId" : "America/New_York",
                     "timeZoneName" : "Eastern Daylight Time"
                  }`

* **Error Response**

  * **Code:** 500 Internal Server Error <br />
    **Content:** <EMPTY>

* **Sample Call**

  `curl -v "http://localhost:9595/worldtime/tz?lat=10.1&lon=12.2"`

* **Notes**

  The error cases typically return HTTP 500 but they should be as per
  the real source of error. This is something to be worked upon
  in the future.

### Worldtime Sunset and Sunrise Time

This endpoint (/worldtime/suntime) can be used to query for sunrise and sunset
at any given geolocation.

* **URL**

  `/worldtime/suntime?lat=GeoLatitude&lon=GeoLongitude`

* **Method**

  `GET`

* **URL Params**

  **Required:**

  `lat=[float]`
  `lon=[float]`

* **Authentication**

  Not required.

* **Data Params**

  There is no request body for this API.

* **Success Response**

  * **Code:** 200 <br />
    **Content:** `{
                    "results":
                    {
                      "sunrise":"2015-05-21T05:05:35+00:00",
                      "sunset":"2015-05-21T19:22:59+00:00",
                      "solar_noon":"2015-05-21T12:14:17+00:00",
                      "day_length":51444,
                      "civil_twilight_begin":"2015-05-21T04:36:17+00:00",
                      "civil_twilight_end":"2015-05-21T19:52:17+00:00",
                      "nautical_twilight_begin":"2015-05-21T04:00:13+00:00",
                      "nautical_twilight_end":"2015-05-21T20:28:21+00:00",
                      "astronomical_twilight_begin":"2015-05-21T03:20:49+00:00",
                      "astronomical_twilight_end":"2015-05-21T21:07:45+00:00"
                    },
                     "status":"OK"
                  }`

The time in the above response is in [ISO 8601](http://en.wikipedia.org/wiki/ISO_8601)
format.

* **Error Response**

  * **Code:** 500 Internal Server Error <br />
    **Content:** <EMPTY>

* **Sample Call**

  `curl -v "http://localhost:9595/worldtime/suntime?lat=36.7201600&lon=-4.4203400"`

* **Notes**

  The error cases typically return HTTP 500 but they should be as per
  the real source of error. This is something to be worked upon
  in the future.

## Thanks

Thanks for evaluating this project and hope you find it useful.
Feel free to create issues for bugs or new features.

## Authors

* Neeraj Sharma {[github: neeraj9](https://github.com/neeraj9)}

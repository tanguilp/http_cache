# http_cache

`http_cache` is a stateless Erlang HTTP caching library that implements the various
HTTP RFCs related to caching.

When caching, it analyses the response along with the associated request to determine whether the
response is cacheable, and caches it if so.

When looking up cached responses, it analyses the request and finds the most suitable response
(that is, response that is the freshest and conforms to the `vary` hear).

It supports invalidating cached responses:
- by URL
- by alternate key

It also supports:
- automatically compressing and decompressing with gzip. It allows saving space when storing
response and reducing transmission time
- replying to range requests

Finally, many telemetry events are emitted. See the documentation of the main module.

## Store backends

Responses have to be stored in a separate store backend (this library being stateless).
A suitable backend store for production use is
[`http_cache_store_native`](https://github.com/tanguilp/http_cache_store_native). It uses
native BEAM features (ETSes...) and is cluster-aware.

## Header normalisation

This library may store different responses for the same URL,
following the directives of the `"vary"` header. For instance, if a response can
be returned in English or in Russian, both versions can be cached as long as the
`"vary"` header is correctly used.

This can unfortunately result in an explosion of stored responses if the headers
are not normalized. For instance, in this scenario where a site handles both these
languages, a response will be stored for any of these requests that include an
`"accept-language"` header:
- `fr-CH, fr;q=0.9, en;q=0.8, de;q=0.7, *;q=0.5`
- `fr-CH, fr;q=0.9, en;q=0.8, de;q=0.7,*;q=0.5`
- `en`
- `ru`
- `en, ru`
- `en, ru, fr`
- `en;q=1, ru`
- `en;q=1, ru;q=0.9`
- `en;q=1, ru;q=0.8`
- `en;q=1, ru;q=0.7`
- `en;q=1, ru;q=0.6`
- `en;q=1, ru;q=0.5`

and so on, so potentially hundreds of stored responses for only 2 available
responses (English or Russian versions).

In this case, you probably want to apply normalization before caching, that is modify the
relevant header to have only the `en` or `fr` value set before using this library.

See [Best practices for using the Vary header](https://www.fastly.com/blog/best-practices-using-vary-header)
for more guidance regarding this issue.

## Support

OTP24+

RFC5861 (`stale-if-error` and `stale-while-revalidate` cache directives) is supported
on latest development branch of `cowlib` (since
[this commit](https://github.com/ninenines/cowlib/commit/ce6798c6b2e95b6a34c6a76d2489eaf159827d80))
or in other words from version `2.12` (not released yet as April, 2022). Manually override
dependency if you need to use it.

## Conformance

RFC9111: [HTTP Caching](https://www.rfc-editor.org/rfc/rfc9111.html):
- [-] 3. Storing Responses in Caches
  - [ ] 3.1. Storing Header and Trailer Fields
  - [x] 3.2. Updating Stored Header Fields
  - [ ] 3.3. Storing Incomplete Responses
  - [ ] 3.4. Combining Partial Content
  - [x] 3.5. Storing Responses to Authenticated Requests
- [x] 4. Constructing Responses from Caches
  - [x] 4.1. Calculating Secondary Keys with Vary
  - [x] 4.2. Freshness
    - [x] 4.2.1. Calculating Freshness Lifetime
    - [x] 4.2.2. Calculating Heuristic Freshness
    - [x] 4.2.3. Calculating Age
    - [x] 4.2.4. Serving Stale Responses
  - [x] 4.3. Validation
    - [x] 4.3.1. Sending a Validation Request
    - [x] 4.3.2. Handling a Received Validation Request
    - [x] 4.3.3. Handling a Validation Response
    - [x] 4.3.4. Freshening Stored Responses upon Validation
    - [x] 4.3.5. Freshening Responses via HEAD
  - [x] 4.4. Invalidating Stored Responses
- [-] 5. Header Field Definitions
  - [x] 5.1. Age
  - [x] 5.2. Cache-Control
    - [x] 5.2.1. Request Cache-Control Directives
      - [x] 5.2.1.1. `max-age`
      - [x] 5.2.1.2. `max-stale`
      - [x] 5.2.1.3. `min-fresh`
      - [x] 5.2.1.4. `no-cache`
      - [x] 5.2.1.5. `no-store`
      - [x] 5.2.1.6. `no-transform`
      - [x] 5.2.1.7. `only-if-cached`
    - [-] 5.2.2. Response Cache-Control Directives
      - [x] 5.2.2.1. `max-age`
      - [x] 5.2.2.2. `must-revalidate`
      - [x] 5.2.2.3. `must-understand`: `http_cache` only caches responses that have a known status
      code
      - [-] 5.2.2.4. `no-cache`: only unqualified form is supported
      - [x] 5.2.2.5. `no-store`
      - [x] 5.2.2.6. `no-transform`
      - [-] 5.2.2.7. `private`" only unqualified form is supported
      - [x] 5.2.2.8. `proxy-revalidate`
      - [x] 5.2.2.9. `public`
      - [x] 5.2.2.10. `s-maxage`
    - [-] 5.2.3. Cache Control Extensions
    - [-] 5.2.4. Cache Directive Registry
  - [x] 5.3. Expires
  - [x] 5.4. Pragma
  - [ ] 5.5. Warning

RFC5861: [HTTP Cache-Control Extensions for Stale Content](https://datatracker.ietf.org/doc/html/rfc5861)
- [x] 3. The stale-while-revalidate Cache-Control Extension
- [x] 4. The stale-if-error Cache-Control Extension

RFC7233: [Hypertext Transfer Protocol (HTTP/1.1): Range Requests](https://datatracker.ietf.org/doc/html/rfc7233)
- [-] 2. Range Units
  - [x] 2.1. Byte Ranges
  - [ ] 2.2. Other Range Units
  - [ ] 2.3. Accept-Ranges
- [x] 3. Range Requests
  - [x] 3.1. Range
  - [x] 3.2. If-Range
- [x] 4. Responses to a Range Request
  - [x] 4.1. 206 Partial Content
  - [x] 4.2. Content-Range
  - [x] 4.3. Combining Ranges
  - [x] 4.4. 416 Range Not Satisfiable

RFC7232: [Hypertext Transfer Protocol (HTTP/1.1): Conditional Requests](https://datatracker.ietf.org/doc/html/rfc7232)
- [x] 2. Validators
  - [x] 2.1. Weak versus Strong
  - [x] 2.2. Last-Modified
  - [x] 2.3. ETag
  - [-] 2.4. When to Use Entity-Tags and Last-Modified Dates
- [-] 3. Precondition Header Fields
  - [ ] 3.1. If-Match
  - [x] 3.2. If-None-Match
  - [x] 3.3. If-Modified-Since
  - [ ] 3.4. If-Unmodified-Since
  - [x] 3.5. If-Range
- [x] 4. Status Code Definitions
  - [x] 4.1. 304 Not Modified
  - [x] 4.2. 412 Precondition Failed
- [x] 5. Evaluation
- [x] 6. Precedence

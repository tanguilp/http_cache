# CHANGELOG

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to Semantic Versioning.

## [0.4.0] - XXXX-XX-XX

### Fixed
- `[http_cache]`: now takes `stale-if-error` and `stale-while-revalidate` directive values into
account when calculating the grace period

### Changed

- `[http_cache]`: handling of the `stale-if-error` directive has changed. Stale responses returned
in this case are now returned by the `cache/3` and `cache/4` functions, whose signature has changed
- `[http_cache]`: the `allow_stale_while_revalidate` option was renamed to
`stale_while_revalidate_supported`

### Removed

- `[http_cache]`: the `stale_if_error` option has been removed
- `[http_cache]`: the `origin_unreachable` option has been removed

## [0.3.2] - 2025-04-07

### Fixed

- `[http_cache]` Fixed bug that made cache/3 and cache/4 return gziped content even
when the request didn't support gzip

## [0.3.1] - 2023-12-20

### Added
- [`http_cache`] Added `prevent_set_cookie` option. Caching a response in a shared
cache with `set-cookie` header now raises

## [0.3.0] - 2023-06-22

### Changed

- [`http_cache`] Use external library `http_cache_store_behaviour`

## [0.2.0] - 2023-04-25

### Added
- [`http_cache`] Export `notifying_downloading/2` for future support of HTTP request
collapsing

### Changed

- [`http_cache`] Options are now a map (was previously a proplist)

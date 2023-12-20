# CHANGELOG

All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to Semantic Versioning.

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

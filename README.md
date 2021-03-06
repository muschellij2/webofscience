
[![Travis build
status](https://travis-ci.com/muschellij2/webofscience.svg?branch=master)](https://travis-ci.com/muschellij2/webofscience)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/webofscience?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/webofscience)
[![Coverage
status](https://codecov.io/gh/muschellij2/webofscience/branch/master/graph/badge.svg)](https://codecov.io/gh/muschellij2/webofscience)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# webofscience Package:

The goal of `webofscience` is to provide interacts with the Web of
Science API, including ‘InCites’ to retrieve citation information.

## API Keys

API keys need to be requested at:
<https://developer.clarivate.com/apis>. Each API has a different API key
and can be set using `ws_set_API_key` where `API` is the API in
question. See `ws_list_apis()` for a listing of the APIs and
`ws_endpoints()` for their endpoints.

## Installation

You can install `webofscience` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("muschellij2/webofscience")
```

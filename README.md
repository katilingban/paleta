
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paleta: Collection of Palettes, Themes, and Theme Components <img src="man/figures/logo.png" width="200" align="right" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/katilingban/paleta/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/katilingban/paleta/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/katilingban/paleta/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/katilingban/paleta/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/katilingban/paleta/branch/main/graph/badge.svg)](https://app.codecov.io/gh/katilingban/paleta?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/katilingban/paleta/badge)](https://www.codefactor.io/repository/github/katilingban/paleta)
<!-- badges: end -->

A collection of palettes, themes, and theme components based on publicly
available branding guidelines of various non-governmental organisations,
government agencies, and United Nations units.

## What does `paleta` do?

`paleta` provides or will be providing utilities for applying the visual
identity and/or branding guidelines of various non-governmental
organisations, government agencies, and United Nations units.

Currently, `paleta` has colours, palettes, and `ggplot2` themes based on
the following organisations’ branding/visual identity guidelines:

- [Africa CDC](https://africacdc.org/) [visual identity
  guidelines](https://africacdc.org/download/visual-identity-africa-cdc-branding-guide/);

- [World Bank](https://www.worldbank.org) [visual identity
  guidelines](https://thedocs.worldbank.org/en/doc/723361567518322252-0060022019/original/WBGBrandingandVisualIdentityGuidelinesFebruary2016.pdf).

- [United Nations Children’s Fund](https://www.unicef.org/) [Brand Book
  and Brand
  Manual](https://www.unicef.org/jordan/media/7166/file/ANNEX_G_-_Brand_book_V3.1.pdf).

Additional colours, palettes, and themes will be provided over time.

## Installation

`paleta` is not yet available on [CRAN](https://cran.r-project.org) but
can be installed through the [Katilingban R
universe](https://katilingban.r-universe.dev) repository as follows:

``` r
install.packages(
  "paleta", 
  repos = c(
    'https://katilingban.r-universe.dev', 
    'https://cloud.r-project.org'
  )
)
```

## Usage

- [Using the Africa CDC colours, palettes, and
  themes](https://katilingban.io/paleta/articles/africa-cdc.html)

- [Using the World Bank colours, palettes, and
  themes](https://katilingban.io/paleta/articles/world-bank.html)

- [Using the UNICEF colours, palettes, and
  themes](https://katilingban.io/paleta/articles/unicef.html)

## Community guidelines

Feedback, bug reports and feature requests are welcome; file issues or
seek support [here](https://github.com/katilingban/paleta/issues). If
you would like to contribute to the package, please see our
[contributing
guidelines](https://katilingban.io/paleta/CONTRIBUTING.html).

This project is released with a [Contributor Code of
Conduct](https://katilingban.io/paleta/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.

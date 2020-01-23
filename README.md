
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openNDB

<!-- badges: start -->

<!-- badges: end -->

[NDBオープンデータ](https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000177182.html)を可視化・分析しやすい形にするパッケージです。

[tidy化したものをcsv/parquet形式で配布しています。](https://github.com/Vintea01/openNDB/tree/master/NDBdata)パッケージはtidy化のコードを公表し、再現性を担保するためのものです。

現在、処方薬数量のtidyデータ版を公開しています。

## Installation

<!--  
You can install the released version of openNDB from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("openNDB")

```
-->

You can install the development version from [GitHub
repository](https://github.com/Vintea01/openNDB) with:

``` r
# install.packages("devtools")
devtools::install_github("Vintea01/openNDB")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(openNDB)
## basic example code
```

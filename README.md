

<div id="badges"><!-- pkgdown markup -->
 <a href="https://github.com/HenrikBengtsson/future.mapreduce/actions?query=workflow%3AR-CMD-check"><img border="0" src="https://github.com/HenrikBengtsson/future.mapreduce/actions/workflows/R-CMD-check.yaml/badge.svg?branch=develop" alt="R CMD check status"/></a>     <a href="https://app.codecov.io/gh/HenrikBengtsson/future.mapreduce"><img border="0" src="https://codecov.io/gh/HenrikBengtsson/future.mapreduce/branch/develop/graph/badge.svg" alt="Coverage Status"/></a> <a href="https://lifecycle.r-lib.org/articles/stages.html"><img border="0" src="man/figures/lifecycle-experimental-orange.svg" alt="Life cycle: experimental"/></a>
</div>

# future.mapreduce: Utility Functions for Future Map-Reduce API Packages 

The **future.mapreduce** package provides utility functions for other packages implementing map-reduce APIs on top of the **[future]** framework.  Specifically, it will provide general functions for "load balancing", that is, methods for partitioning the elements to iterate over into chunks so that each chunk is processed by a single futures.  Load balancing helps lower the overhead in parallel processing that comes from communicating with and orchestrating parallel workers.  It will provide methods for common tasks such as globals handling and critical tasks such as parallel RNG in map-reduce contexts.

_WARNING: This package is currently just a skeleton.  Please stay tuned._

This will benefit existing map-reduce packages **[future.apply]**, **[furrr]**, and **[doFuture]**, but also other similar efforts.  This will further simply the implementing of these existing solutions as well as other future-based map-reduce APIs that might be on the horizon.


[future]: https://cran.r-project.org/package=future
[future.apply]: https://cran.r-project.org/package=future.apply
[furrr]: https://cran.r-project.org/package=furrr
[doFuture]: https://cran.r-project.org/package=doFuture

## Installation
R package future.mapreduce is only available via [GitHub](https://github.com/HenrikBengtsson/future.mapreduce) and can be installed in R as:
```r
remotes::install_github("HenrikBengtsson/future.mapreduce", ref="master")
```


### Pre-release version

To install the pre-release version that is available in Git branch `develop` on GitHub, use:
```r
remotes::install_github("HenrikBengtsson/future.mapreduce", ref="develop")
```
This will install the package from source.  

<!-- pkgdown-drop-below -->


## Contributing

To contribute to this package, please see [CONTRIBUTING.md](CONTRIBUTING.md).


# future.mapreduce: Utility Functions for Future Map-Reduce API Packages

![Life cycle: experimental](vignettes/imgs/lifecycle-experimental-orange.svg)

The **future.mapreduce** package provides utility functions for other packages implementing map-reduce APIs on top of the **[future]** framework.  Specifically, it will provide general functions for "load balancing", that is, methods for partitioning the elements to iterate over into chunks so that each chunk is processed by a single futures.  Load balancing helps lower the overhead in parallel processing that comes from communicating with and orchestrating parallel workers.  It will provide methods for common tasks such as globals handling and critical tasks such as parallel RNG in map-reduce contexts.

This will benefit existing map-reduce packages **[future.apply]**, **[furrr]**, and **[doFuture]**, but also other similar efforts.  This will further simply the implementing of these existing solutions as well as other future-based map-reduce APIs that might be on the horizon.


## Installation

R package future.mapreduce is only available via [GitHub](https://github.com/HenrikBengtsson/future.mapreduce) and can be installed in R as:
```r
remotes::install_github("HenrikBengtsson/future.mapreduce", ref="master")
```


## Contributions

This Git repository uses the [Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model (the [`git flow`](https://github.com/petervanderdoes/gitflow-avh) extension is useful for this).  The [`develop`](https://github.com/HenrikBengtsson/future.mapreduce/tree/develop) branch contains the latest contributions and other code that will appear in the next release, and the [`master`](https://github.com/HenrikBengtsson/future.mapreduce) branch contains the code of the latest release.

Contributing to this package is easy.  Just send a [pull request](https://help.github.com/articles/using-pull-requests/).  When you send your PR, make sure `develop` is the destination branch on the [future.mapreduce repository](https://github.com/HenrikBengtsson/future.mapreduce).  Your PR should pass `R CMD check --as-cran`, which will also be checked by  and  when the PR is submitted.


## Software status

| Resource      | GitHub        | GitHub Actions      | Travis CI       | AppVeyor CI      |
| ------------- | ------------------- | ------------------- | --------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Multiple_          | _Linux & macOS_ | _Windows_        |
| R CMD check   |  | <a href="https://github.com/HenrikBengtsson/future.mapreduce/actions?query=workflow%3AR-CMD-check"><img src="https://github.com/HenrikBengtsson/future.mapreduce/workflows/R-CMD-check/badge.svg?branch=develop" alt="Build status"></a>       |    |  |
| Test coverage |                     |                     |      |                  |

[future]: https://cran.r-project.org/package=future
[future.apply]: https://cran.r-project.org/package=future.apply
[furrr]: https://cran.r-project.org/package=furrr
[doFuture]: https://cran.r-project.org/package=doFuture

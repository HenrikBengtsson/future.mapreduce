The **future.mapreduce** package provides utility functions for other packages implementing map-reduce APIs on top of the **[future]** framework.  Specifically, it will provide general functions for "load balancing", that is, methods for partitioning the elements to iterate over into chunks so that each chunk is processed by a single futures.  Load balancing helps lower the overhead in parallel processing that comes from communicating with and orchestrating parallel workers.  It will provide methods for common tasks such as globals handling and critical tasks such as parallel RNG in map-reduce contexts.

_WARNING: This package is currently just a skeleton.  Please stay tuned._

This will benefit existing map-reduce packages **[future.apply]**, **[furrr]**, and **[doFuture]**, but also other similar efforts.  This will further simply the implementing of these existing solutions as well as other future-based map-reduce APIs that might be on the horizon.


## Roadmap

 * Migrate core functions from **future.apply** and **doFuture** to **future.mapreduce**

 * Only export a minimal API and keep most functions internal for now

 * Update **future.apply** and **doFuture** to import (also internal functions) from **future.mapreduce**

 * Submit **future.mapreduce** to CRAN with a warning that the API is under development and should not be used by other packages than **future.apply** and **doFuture** until further notice

 * Submit updated versions of **future.apply** and **doFuture**

 * Work with **furrr** to make use of **future.mapreduce**

 * When the internal API has stabilized, export it and submit to CRAN


[future]: https://cran.r-project.org/package=future
[future.apply]: https://cran.r-project.org/package=future.apply
[furrr]: https://cran.r-project.org/package=furrr
[doFuture]: https://cran.r-project.org/package=doFuture

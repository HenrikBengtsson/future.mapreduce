# Version (development version)

* Keeping `get_random_seed()`, `set_random_seed()`,
  `next_random_seed()`, `is_valid_random_seed()`,
  `is_lecyer_cmrg_seed()`, and `as_lecyer_cmrg_seed()` as internal
  functions in **future** for now.


# Version 0.0.1 [2021-06-29]

* Add `get_globals_and_packages_xapply()`, which is a generalized
  version of `future.apply:::getGlobalsAndPackagesXApply()`.

* Add `make_chunks()`, which eventually will replace
 `future.apply:::makeChunks()`.
 
* Add `make_rng_seeds()`, which eventually will replace 
 `future.apply:::make_rng_seeds()`.

* Add `get_random_seed()`, `set_random_seed()`, `next_random_seed()`,
  `is_valid_random_seed()`, `is_lecyer_cmrg_seed()`,
  `as_lecyer_cmrg_seed()`, which originates from internal versions in
  **future** and **future.apply**.

* Aiming for a snake-case naming convention for functions and arguments.


# Version 0.0.0-9000 [2021-06-12]

## New Features

 * Created package.

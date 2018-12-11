// Rest can run as is  [** Run under Stata 14.]

cd "$dir"

* our dataset creation--the discrete first, then the continuous
do "dem_cov_small - mstate format--1 - discrete, v1a (France et al).do"
do "dem_cov_small - mstate format--2 - continuous, v1a (France et al).do"


// Done.

concstats 0.1.2 (submission to CRAN)
================================================================
For the submission to CRAN the author fixed the following issues:

- increased version number to v.0.1.2.

- Fixed issues with unit testing regarding floating doubles


concstats 0.1.1 (Resubmission to CRAN)
================================================================
For the resubmission to CRAN the author fixed the following issues:

- increased version number.

- Fixed issues with unit testing.

concstats 0.1.0 (Resubmission to CRAN)
================================================================
For the resubmission to CRAN the author fixed the following issues:

- All references regarding methods were moved from their respective script files
to the DESCRIPTION file.

-  Fixed issue with (unnecessary) instances of install.packages() in vignette.

concstats 0.1.0
================================================================
This version of ``concstats`` has been updated:

- added code coverage for the package.

- added `tests` for various functions and group of functions.

Most of the functions were streamlined with less lines of code.

concstats 0.0.2.999
=================================================
This development version of the ``concstats`` package has been completely
restructured. The one-step function has been recomposed in three main groups and
the following functions (measures) were added:

* the group wrapper for the concentration/competition measures (comp.R) contains
the following functions:
  + `hhi`
  + `hhi_d`
  + `hhi_min`
  + `dom`
  + `sten`
  + `all_comp` 
    - calculates all the measures of the wrapper in a one-step procedure
      
* the inequality wrapper (inequ.R) added the following functions:
  + `entropy`
  + `gini`
  + `berry`
  + `palma`
  + `grs`
  + `all_inequ`
    - calculates all the measures of the wrapper in a one-step procedure
    
* the market structure wrapper (mstruct.R) contains the following functions:
  + `firm`
  + `nrs_eq`
  + `top`
  + `top3`
  + `top5`
  + `all_mstruct`
    - calculates all the measures of the wrapper in a one-step procedure
 
* `concstats`
  + calculates a set of eight preselected concentration and diversity measures
    in a one-step procedure
  
* Updated `Description` and `Readme` file

* Added `vignettes`

* Added `web page`

* Added `creditcoops`, a small data set 

* Added a `NEWS.md` file to track changes to the package.
 
concstats 0.0.1
========================================================================
- initial version 

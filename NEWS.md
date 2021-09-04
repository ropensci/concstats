# concstats 0.0.2.999

This development version of the ``concstats`` package has been completly
restructed. The one-step funtion has been recomposed in three main groups and
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

* Added `creditcoops`, a small data set 

* Added a `NEWS.md` file to track changes to the package.
 


---
# concstats 0.0.1

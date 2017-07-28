# biosensequality
**A Data Quality Report Generator for the NSSP BioSense Platform**

This package can be installed using the `devtools` package. If you do not have this package installed, you can install it with:

`install.packages("devtools")`

After this, you can install the `biosensequality` package using the `install_github` function from `devtools`:

`devtools::install_github("markhwhiteii/biosensequality")`

**Getting Started**

Take a look at the introductory vignette (http://rpubs.com/markhw/bioqual-vignette) for an introduction on how to use the function; also check out the explainer (http://rpubs.com/markhw/bioqual-interpret) on how to interpret the results.


## Updates
**2017-07-28:** Added the function `write_facility`, which is a lightweight alternative to `write_reports`. While `write_reports` generates statewide summaries and separate summary and example workbooks for *each and every facility*, this function takes an extra argument, `facility`, (which will be the C_Biosense_Facility_ID for the facility you want) and generates a summary and example report *for that facility only.* It runs much, much quicker, and can be used when you only want to check-in on one facility.  

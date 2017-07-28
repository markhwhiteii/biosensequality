# biosensequality
**A Data Quality Report Generator for the NSSP BioSense Platform**

This package can be installed using the `devtools` package. If you do not have this package installed, you can install it with:

`install.packages("devtools")`

After this, you can install the `biosensequality` package using the `install_github` function from `devtools`:

`devtools::install_github("markhwhiteii/biosensequality")`

**Getting Started**

Take a look at the introductory vignette (http://rpubs.com/markhw/bioqual-vignette) for an introduction on how to use the function; also check out the explainer (http://rpubs.com/markhw/bioqual-interpret) on how to interpret the results.


## Updates
* **2017-07-28:** Added the function `write_facility`, which is a lightweight alternative to `write_reports`. While `write_reports` generates statewide summaries and separate summary and example workbooks for *each and every facility*, this function takes an extra argument, `facility`, (which will be the C_Biosense_Facility_ID for the facility you want) and generates a summary and example report *for that facility only.* It runs much, much quicker, and can be used when you only want to check-in on one facility.

* **2017-07-28:** Fixed the time zone issue brought up in the ISDS RUG meeting—that C_Visit_Date_Time and Arrived_Date_Time are in different time zones. A new parameter has now been added to the `va_lag`, `write_reports`, and `write_reports_local` functions called `offset`. This asks the user to specify how many hours they are behind UTC, and the timeliness report now adjusts for this difference. Thanks to Sophia Crossen and Aaron Kite Powell for help in pointing this out and remedying the issue.

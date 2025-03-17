## easySdcTable	1.1.1

* Notes added in vignettes
  - Maintenance notice and suggested alternative
    * This package will be maintained to work, but will not be developed further. It is limited to handling problems with two linked frequency tables. For more advanced frequency
       table problems and for magnitude tables, the 
       [GaussSuppression package](https://cran.r-project.org/package=GaussSuppression) is recommended. Also, note that it is important to use easySdcTable’s own method `"Gauss"`
       (default), not sdcTable’s `"GAUSS"`.  See [comparisons at the GitHub Repository](https://github.com/statisticsnorway/ssb-easysdctable/tree/master/comparison).
  - The singletons/zeros vignette
    * The terminology in this vignette is somewhat misleading. According to 
    [Langsrud (2024)](https://doi.org/10.1007/978-3-031-69651-0_6), "ones and zeros" should have been used instead of "singletons and zeros."
* New pkgdown website for the package, with logo  
  - This package now has a documentation site at [https://statisticsnorway.github.io/ssb-easysdctable/](https://statisticsnorway.github.io/ssb-easysdctable/).
* Some technical changes in documentation to comply with standards.    


## easySdcTable	1.0.7

* Improvements when microdata input (`freqVar = NULL`)  combined with "Gauss" 
  - Data is pre-aggregated prior to the Gauss algorithm. Efficiency is improved.  
  - This also improves singleton handling when `protectZeros = FALSE`.


## easySdcTable	1.0.3

* Minor updates of vignettes 
  - Note a comment about "Gauss" and zeros omitted in input in "Introduction to 'easySdcTable'"

## easySdcTable	1.0.1

* Update of the vignette "The problem of singletons and zeros"
  - For all the examples to still be relevant, `"SIMPLEHEURISTIC_OLD"` is used instead of `"SIMPLEHEURISTIC"`.
  
## easySdcTable	1.0.0

* Wrapper methos "SimpleSingle" and "Simple" re-defined to use "SIMPLEHEURISTIC_OLD"
  - Possible after introduction of "SIMPLEHEURISTIC_OLD" in sdcTable 0.32.1
  - Ensures same behavior as sdcTable versions prior to 0.32. 
  - Thus, "SimpleSingle" and "Simple" behave as earlier (see problems in 0.8.0 news below).  
* New wrapper method "SIMPLEHEURISTICSingle"
  - As "SimpleSingle" with "SIMPLEHEURISTIC" instead of "SIMPLEHEURISTIC_OLD"


## easySdcTable	0.9.0

* Method "Gauss" improved when zeros omitted in input data.
  - A potential pitfall when doing secondary suppression by Gaussian elimination is cases with `protectZeros = TRUE` and where zeros are omitted in input data. The underlying function, GaussSuppression, produce a warning in such cases (introduced in  SSBtools version 1.2.2) with text: *"Suppressed cells with empty input will not be protected. Extend input data with zeros?"*.  Cases where this warning occur is now avoided within ProtectTable. Internally data are automatically extended when needed.  


## easySdcTable	0.8.0

* Method "Gauss" made default. 
  - Based on comparison of computing times, errors and the numbers of suppressed and revealed cells,
this is a natural consequence after the release of sdcTable version 0.32.0 
(see `comparison/Comparison of suppression methods while developing easySdcTable ver. 0.8.0.md` on the github page of easySdcTable). 
* Internal changes caused by changes in sdcTable version 0.32.0
  - Now `protect_linked_tables` is used insetad of `protectLinkedTables`
  - Summary output from sdcTable is captured differently (for `info` output)
* New tidy wrapper function, `ProtectTableData`,  which returns a single data frame. 


## easySdcTable	0.7.0

* No changes in functionality. Only technical changes and minor improvements. 


## easySdcTable	0.6.0

* New method, "Gauss", available 
  - An additional method that is not available in sdcTable
  - Secondary suppression by Gaussian elimination
* Tau-argus problem fixed. 
  - Version 0.5.0 resulted in wrong tau-argus input when the useFancyQuotes option was TRUE.  

## easySdcTable	0.5.0

* Method "SimpleSingle" re-defined and made default. 
  - The new parameter, threshold, in sdcTable is used. 
* The shiny functionality is now as two functions  
  - Run PTgui from the R console or use PTguiApp to make a server application
* PTxyz, new function
  - ProtectTable with output ready for SuppressDec in package RegSDC
  
## easySdcTable	0.3.3

* Last version before any news

## easySdcTable	0.8.0

* Method "Gauss" made default. 
  - Based on comparison of computing times, errors and the numbers of suppressed and revealed cells,
this is a natural consequence after the release of sdcTable version 0.32.0 
(see `Comparison of suppression methods while developing easySdcTable ver. 0.8.0.md` on the github page of easySdcTable). 
* Other changes caused by changes in sdcTable version 0.32.0
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

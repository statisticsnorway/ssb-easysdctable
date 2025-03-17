# R package easySdcTable <img src="man/figures/logo.png" align="right" height="150" /> 
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)


| [easySdcTable on CRAN](https://cran.r-project.org/package=easySdcTable) |  | [pkgdown website](https://statisticsnorway.github.io/ssb-easysdctable/) |  | [GitHub Repository](https://github.com/statisticsnorway/ssb-easysdctable) |
|----------------------|---|----------------------|---|----------------------|


***

### Description


**Easy Interface to the Statistical Disclosure Control Package 'sdcTable' Extended with Own Implementation of 'GaussSuppression'**


The main function, 
[ProtectTable()](https://statisticsnorway.github.io/ssb-easysdctable/reference/ProtectTable.html), 
performs table suppression according to a 
 frequency rule with a data set as the only required input. Within this function, 
 protectTable(), protect_linked_tables() or runArgusBatchFile() in package 'sdcTable' is called. 
 Lists of level-hierarchy (parameter 'dimList') and other required input to these functions 
 are created automatically. 
 The suppression method Gauss (default) is implemented independently of 'sdcTable'.
 The function, 
[PTgui()](https://statisticsnorway.github.io/ssb-easysdctable/reference/PTgui.html), 
 starts a graphical user interface based on the 'shiny' package.


***


### Installation

You can install easySdcTable from CRAN with

```r
install.packages("easySdcTable")
```

***

### Maintenance notice and suggested alternative 

This package will be maintained to work, but will not be developed further. It is limited to handling problems with two linked frequency tables. For more advanced frequency table problems and for magnitude tables, the 
[GaussSuppression package](https://cran.r-project.org/package=GaussSuppression) is recommended.

Also, note that it is important to use easySdcTable’s own method `"Gauss"` (default), not sdcTable’s `"GAUSS"`. 
See [comparisons at the GitHub Repository](https://github.com/statisticsnorway/ssb-easysdctable/tree/master/comparison).


***


📌 See the [list of functions](https://statisticsnorway.github.io/ssb-easysdctable/reference/index.html).


***


Official version on CRAN: [https://cran.r-project.org/package=easySdcTable](https://cran.r-project.org/package=easySdcTable)

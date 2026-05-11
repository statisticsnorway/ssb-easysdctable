# Introduction to 'easySdcTable'

## Introduction and background

Below is given an introductory demonstration of the function
ProtectTable() which enables an easy interface to the statistical
disclosure control package ‘sdcTable’
(<https://CRAN.R-project.org/package=sdcTable>). To see the input and
output to functions in sdcTable consult the function ProtectTable1()
which is an underlying function of ProtectTable(). Note that
‘easySdcTable’ is not as general as ‘sdcTable’.

This package was originally developed as a part of the modernization of
the production of the key figures on municipal activities in Norway
(<https://www.ssb.no/en/offentlig-sektor/kostra>). The fictitious
example data is generated to be similar to realistic data from Norwegian
municipalities and the variable names are (unfortunately) in Norwegian.

The demonstration below is based on the data from example 2 in the
package and first we will use the unstacked data.

Before demonstrating ProtectTable() a few words about other
possibilities.

### Maintenance notice and suggested alternative (added in version 1.1.1)

This package will be maintained to work, but will not be developed
further. It is limited to handling problems with two linked frequency
tables. For more advanced frequency table problems and for magnitude
tables, the [GaussSuppression
package](https://cran.r-project.org/package=GaussSuppression) is
recommended.

Also, note that it is important to use easySdcTable’s own method
`"Gauss"` (default), not sdcTable’s `"GAUSS"`. See comparisons at the
[GitHub
Repository](https://github.com/statisticsnorway/ssb-easysdctable).

#### Note after easySdcTable version 0.8.0

Method `"Gauss"` has been made default (See NEWS). This is an additional
method that is not available in sdcTable.

#### News in easySdcTable version 0.9.0

Method “Gauss” improved when zeros omitted in input data.

- A potential pitfall when doing secondary suppression by Gaussian
  elimination is cases with `protectZeros = TRUE` and where zeros are
  omitted in input data. The underlying function, GaussSuppression,
  produce a warning in such cases (introduced in SSBtools version 1.2.2)
  with text: *“Suppressed cells with empty input will not be protected.
  Extend input data with zeros?”*. Cases where this warning occur is now
  avoided within ProtectTable. Internally data are automatically
  extended when needed.

#### Another comment about “Gauss” and zeros omitted in input

When hierarchies are supplied as input (parameter `dimList`) and when
there exist input codes in the hierarchies that are totally missing in
the data, it is still possible to create a situation with warning:
*“Suppressed cells with empty input will not be protected. Extend input
data with zeros?”*. This behavior will not be changed. Ignore the
warning if such codes represent structural zeros. If not, extend data
with zero frequencies (see parameter freqVar) so that these code are
represented in data.

## Graphical user interface and $`\tau`$-ARGUS

A graphical user interface based on ‘shiny’ can be started by:

``` r

PTgui()
```

To start the gui with example data and catch output:

``` r

out <- PTgui(data=EasyData("z1w"))
```

To start the gui with possibility to run tau-argus:

``` r

exeArgus <- "C:/Tau/TauArgus4.1.4/TauArgus.exe" # Tau-argus executable 
pathArgus <- "C:/Users/nnn/Documents" # Folder for (temporary) tau-argus files
PTgui(exeArgus=exeArgus, pathArgus=pathArgus) 
```

The interface to tau-argus make use of functionality in ‘sdcTable’. See
the documentation of ProtectTable() for more information.

## Unstacked data

#### The input data

The function EasyData() in ‘easySdcTable’ returns example data.

``` r

z2w <- EasyData("z2w") 
print(z2w, row.names=FALSE)
```

     region fylke kostragr annet arbeid soshjelp trygd
          A     1      300    11     11       55    36
          B     4      300     7      1       29    18
          C     5      300     5      8       35    25
          D     5      300    13      2       17    13
          E     6      300     9     14       63    52
          F     6      300    12      9       24    22
          G     8      300     6      4       22     8
          H     8      300     9      3       38    15
          I     1      400     3      0        9     2
          J    10      400     9      0       32    20
          K    10      400     4      2       18    11

By unstacked data we mean that counts (cell frequencies) are in more
than a single column.

#### Running ProtectTable

In this case we have counts in columns four to seven. Using the
dimensional variable in the first column we can run ProtectTable by:

``` r

ex2w <- ProtectTable(z2w,1,4:7) 
```

#### The data with computed totals

The output element *freq* contains the data with computed totals.

``` r

print(ex2w$freq, row.names=FALSE)
```

     region annet arbeid soshjelp trygd Total
          A    11     11       55    36   113
          B     7      1       29    18    55
          C     5      8       35    25    73
          D    13      2       17    13    45
          E     9     14       63    52   138
          F    12      9       24    22    67
          G     6      4       22     8    40
          H     9      3       38    15    65
          I     3      0        9     2    14
          J     9      0       32    20    61
          K     4      2       18    11    35
      Total    88     54      342   222   706

#### SdcStatus

In the output element *sdcStatus* the cells are coded as “u” (primary
suppressed), “x” (secondary suppression), and “s” (can be published).

``` r

print(ex2w$sdcStatus, row.names=FALSE)
```

     region annet arbeid soshjelp trygd Total
          A     s      s        s     s     s
          B     x      u        s     s     s
          C     s      s        s     s     s
          D     x      u        s     s     s
          E     s      s        s     s     s
          F     s      s        s     s     s
          G     s      x        s     x     s
          H     x      u        s     s     s
          I     u      u        s     u     s
          J     x      u        s     s     s
          K     x      u        s     s     s
      Total     s      s        s     s     s

#### Suppressed data

The output element *suppressed* is the same as *freq* with the exception
that suppressed cells (“u” and “x”) are set to missing (NA).

``` r

print(ex2w$suppressed, row.names=FALSE)
```

     region annet arbeid soshjelp trygd Total
          A    11     11       55    36   113
          B    NA     NA       29    18    55
          C     5      8       35    25    73
          D    NA     NA       17    13    45
          E     9     14       63    52   138
          F    12      9       24    22    67
          G     6     NA       22    NA    40
          H    NA     NA       38    15    65
          I    NA     NA        9    NA    14
          J    NA     NA       32    20    61
          K    NA     NA       18    11    35
      Total    88     54      342   222   706

#### Using named input and the SIMPLEHEURISTIC_OLD method

Now we specify the variables using names instead of numbers. Furthermore
we use the “SIMPLEHEURISTIC_OLD” method.

``` r

ex2wOLD <- ProtectTable(z2w,dimVar = c("region"),freqVar = c("annet", "arbeid", "soshjelp", "trygd"), method="SIMPLEHEURISTIC_OLD")  
```

``` r

print(ex2wOLD$suppressed, row.names=FALSE)
```

     region annet arbeid soshjelp trygd Total
          A    11     11       55    36   113
          B    NA     NA       29    18    55
          C     5      8       35    25    73
          D    13     NA       17    NA    45
          E     9     14       63    52   138
          F    12      9       24    22    67
          G     6      4       22     8    40
          H    NA     NA       38    15    65
          I    NA     NA        9    NA    14
          J    NA     NA       32    20    61
          K    NA     NA       18    11    35
      Total    88     54      342   222   706

#### More advanced use of ProtectTable

Here we include the tree first variables as dimensional variables. It
will be detected automatically that “fylke” and “kostragr” are
hierarchically related to “region” and that they are not hierarchically
related to each other. Zeros will not be suppressed and we will only
primarily suppress ones and twos.

``` r

ex2wAdvanced <- ProtectTable(z2w,dimVar = c("region", "fylke","kostragr"),freqVar = c("annet", "arbeid", "soshjelp", "trygd"), maxN=2, protectZeros=FALSE, method = "SIMPLEHEURISTIC", addName=TRUE)  
```

#### Suppressed data with totals and sub-totals

Now the output data will contain sub-totals of the additional variables
and the secondary suppression has taken those sub-totals into account.
Since addName is TRUE, sub-totals are named using “fylke” and
“kostragr”.

``` r

print(ex2wAdvanced$suppressed, row.names=FALSE)
```

           region annet arbeid soshjelp trygd Total
                A    NA     11       55    NA   113
                B    NA     NA       29    NA    55
                C    NA     NA       35    25    73
                D    NA     NA       17    13    45
                E     9     14       63    52   138
                F    12      9       24    22    67
                G     6     NA       22    NA    40
                H     9     NA       38    NA    65
                I    NA      0        9    NA    14
                J     9      0       32    20    61
                K    NA     NA       18    NA    35
          fylke_1    14     11       64    38   127
          fylke_4    NA     NA       29    NA    55
          fylke_5    18     10       52    38   118
          fylke_6    21     23       87    74   205
          fylke_8    15      7       60    23   105
         fylke_10    NA     NA       50    NA    96
     kostragr_300    NA     NA      283   189   596
     kostragr_400    NA     NA       59    33   110
            Total    88     54      342   222   706

#### Info

The output element *info* contains three parts.

1.  Since we have unstacked data an extra variable, named *var1*, is
    created. How the categories of this variable are related to the
    variable names are described. Here these categories are simply the
    variable names. In more advanced cases it is possible that more than
    a single variable are created from the variable names.

2.  Secondly, it is described how the tables(s) are created from the
    variables. In this case the problem is solved using two linked
    tables. The first table involves “fylke” and the second table
    involves “kostragr”.

3.  The last part contains summary output for each of the two linked
    tables.

``` r

prmatrix(ex2wAdvanced$info,rowlab=rep("",99),collab="",quote=FALSE)
```

                                                                                
                  var1                                                          
     annet       annet                                                          
     arbeid     arbeid                                                          
     soshjelp soshjelp                                                          
     trygd       trygd                                                          
     ==========                                                                 
              Variables Table1 Table2                                           
     1    region, fylke      1      0                                           
     2 region, kostragr      0      1                                           
     3             var1      1      1                                           
     ==========                                                                 
     Classes 'safeObj', 'data.table' and 'data.frame':\t90 obs. of  4 variables:
      $ region   : chr  "Total" "Total" "Total" "Total" ...                     
      $ var1     : chr  "Total" "annet" "arbeid" "soshjelp" ...                 
      $ Freq     : num  706 88 54 342 222 127 14 11 64 38 ...                   
      $ sdcStatus: chr  "s" "s" "s" "s" ...                                     
      - attr(*, ".internal.selfref")=<pointer: 0x56455e0aeef0>                  
     NULL                                                                       
     ==========                                                                 
     Classes 'safeObj', 'data.table' and 'data.frame':\t70 obs. of  4 variables:
      $ region   : chr  "Total" "Total" "Total" "Total" ...                     
      $ var1     : chr  "Total" "annet" "arbeid" "soshjelp" ...                 
      $ Freq     : num  706 88 54 342 222 596 72 52 283 189 ...                 
      $ sdcStatus: chr  "s" "s" "s" "s" ...                                     
      - attr(*, ".internal.selfref")=<pointer: 0x56455e0aeef0>                  
     NULL                                                                       

## Stacked data

Now we will use a stacked variant of the same data. A single column
(“ant”) holds cell counts and the variable “hovedint” contains the four
categories “annet”, “arbeid”, “soshjelp” and “trygd”.

``` r

z2 <- EasyData("z2") 
print(z2)
```

       region fylke kostragr hovedint ant
    1       A     1      300    annet  11
    2       B     4      300    annet   7
    3       C     5      300    annet   5
    4       D     5      300    annet  13
    5       E     6      300    annet   9
    6       F     6      300    annet  12
    7       G     8      300    annet   6
    8       H     8      300    annet   9
    9       I     1      400    annet   3
    10      J    10      400    annet   9
    11      K    10      400    annet   4
    12      A     1      300   arbeid  11
    13      B     4      300   arbeid   1
    14      C     5      300   arbeid   8
    15      D     5      300   arbeid   2
    16      E     6      300   arbeid  14
    17      F     6      300   arbeid   9
    18      G     8      300   arbeid   4
    19      H     8      300   arbeid   3
    20      I     1      400   arbeid   0
    21      J    10      400   arbeid   0
    22      K    10      400   arbeid   2
    23      A     1      300 soshjelp  55
    24      B     4      300 soshjelp  29
    25      C     5      300 soshjelp  35
    26      D     5      300 soshjelp  17
    27      E     6      300 soshjelp  63
    28      F     6      300 soshjelp  24
    29      G     8      300 soshjelp  22
    30      H     8      300 soshjelp  38
    31      I     1      400 soshjelp   9
    32      J    10      400 soshjelp  32
    33      K    10      400 soshjelp  18
    34      A     1      300    trygd  36
    35      B     4      300    trygd  18
    36      C     5      300    trygd  25
    37      D     5      300    trygd  13
    38      E     6      300    trygd  52
    39      F     6      300    trygd  22
    40      G     8      300    trygd   8
    41      H     8      300    trygd  15
    42      I     1      400    trygd   2
    43      J    10      400    trygd  20
    44      K    10      400    trygd  11

We run ProtectTable with stacked data the same way as with unstacked
data.

``` r

ex2 <- ProtectTable(z2,dimVar = c("region", "hovedint", "kostragr"), freqVar = "ant") 
```

Instead of three output elements we now have the single element *data*:

``` r

print(ex2$data)
```

       region hovedint freq sdcStatus suppressed
    1       A    annet   11         s         11
    2       B    annet    7         x         NA
    3       C    annet    5         s          5
    4       D    annet   13         x         NA
    5       E    annet    9         s          9
    6       F    annet   12         s         12
    7       G    annet    6         s          6
    8       H    annet    9         x         NA
    9       I    annet    3         u         NA
    10      J    annet    9         x         NA
    11      K    annet    4         s          4
    12      A   arbeid   11         s         11
    13      B   arbeid    1         u         NA
    14      C   arbeid    8         s          8
    15      D   arbeid    2         u         NA
    16      E   arbeid   14         s         14
    17      F   arbeid    9         s          9
    18      G   arbeid    4         s          4
    19      H   arbeid    3         u         NA
    20      I   arbeid    0         u         NA
    21      J   arbeid    0         u         NA
    22      K   arbeid    2         u         NA
    23      A soshjelp   55         s         55
    24      B soshjelp   29         s         29
    25      C soshjelp   35         s         35
    26      D soshjelp   17         s         17
    27      E soshjelp   63         s         63
    28      F soshjelp   24         s         24
    29      G soshjelp   22         s         22
    30      H soshjelp   38         s         38
    31      I soshjelp    9         s          9
    32      J soshjelp   32         s         32
    33      K soshjelp   18         s         18
    34      A    trygd   36         s         36
    35      B    trygd   18         s         18
    36      C    trygd   25         s         25
    37      D    trygd   13         s         13
    38      E    trygd   52         s         52
    39      F    trygd   22         s         22
    40      G    trygd    8         s          8
    41      H    trygd   15         s         15
    42      I    trygd    2         u         NA
    43      J    trygd   20         s         20
    44      K    trygd   11         x         NA
    45      A    Total  113         s        113
    46      B    Total   55         s         55
    47      C    Total   73         s         73
    48      D    Total   45         s         45
    49      E    Total  138         s        138
    50      F    Total   67         s         67
    51      G    Total   40         s         40
    52      H    Total   65         s         65
    53      I    Total   14         s         14
    54      J    Total   61         s         61
    55      K    Total   35         s         35
    56    300    annet   72         x         NA
    57    300   arbeid   52         x         NA
    58    300 soshjelp  283         s        283
    59    300    trygd  189         s        189
    60    300    Total  596         s        596
    61    400    annet   16         x         NA
    62    400   arbeid    2         u         NA
    63    400 soshjelp   59         s         59
    64    400    trygd   33         s         33
    65    400    Total  110         s        110
    66  Total    annet   88         s         88
    67  Total   arbeid   54         s         54
    68  Total soshjelp  342         s        342
    69  Total    trygd  222         s        222
    70  Total    Total  706         s        706

Unlike above addName is FALSE (default) and therefore the sub-totals
“300” and “400” are written without “kostragr”.

## Assuming micro data

Below no columns holds cell counts (no freqVar input) and therefore it
is assumed that each cell count is one. For this data set this is not
realistic, but in other cases rows are replicated.

``` r

ex2micro <- ProtectTable(z2,dimVar = c("region", "hovedint", "kostragr")) 
```

``` r

print(ex2micro$data)
```

       region hovedint freq sdcStatus suppressed
    1       A    annet    1         u         NA
    2       B    annet    1         u         NA
    3       C    annet    1         u         NA
    4       D    annet    1         u         NA
    5       E    annet    1         u         NA
    6       F    annet    1         u         NA
    7       G    annet    1         u         NA
    8       H    annet    1         u         NA
    9       I    annet    1         u         NA
    10      J    annet    1         u         NA
    11      K    annet    1         u         NA
    12      A   arbeid    1         u         NA
    13      B   arbeid    1         u         NA
    14      C   arbeid    1         u         NA
    15      D   arbeid    1         u         NA
    16      E   arbeid    1         u         NA
    17      F   arbeid    1         u         NA
    18      G   arbeid    1         u         NA
    19      H   arbeid    1         u         NA
    20      I   arbeid    1         u         NA
    21      J   arbeid    1         u         NA
    22      K   arbeid    1         u         NA
    23      A soshjelp    1         u         NA
    24      B soshjelp    1         u         NA
    25      C soshjelp    1         u         NA
    26      D soshjelp    1         u         NA
    27      E soshjelp    1         u         NA
    28      F soshjelp    1         u         NA
    29      G soshjelp    1         u         NA
    30      H soshjelp    1         u         NA
    31      I soshjelp    1         u         NA
    32      J soshjelp    1         u         NA
    33      K soshjelp    1         u         NA
    34      A    trygd    1         u         NA
    35      B    trygd    1         u         NA
    36      C    trygd    1         u         NA
    37      D    trygd    1         u         NA
    38      E    trygd    1         u         NA
    39      F    trygd    1         u         NA
    40      G    trygd    1         u         NA
    41      H    trygd    1         u         NA
    42      I    trygd    1         u         NA
    43      J    trygd    1         u         NA
    44      K    trygd    1         u         NA
    45      A    Total    4         s          4
    46      B    Total    4         s          4
    47      C    Total    4         s          4
    48      D    Total    4         s          4
    49      E    Total    4         s          4
    50      F    Total    4         s          4
    51      G    Total    4         s          4
    52      H    Total    4         s          4
    53      I    Total    4         s          4
    54      J    Total    4         s          4
    55      K    Total    4         s          4
    56    300    annet    8         x         NA
    57    300   arbeid    8         x         NA
    58    300 soshjelp    8         x         NA
    59    300    trygd    8         x         NA
    60    300    Total   32         s         32
    61    400    annet    3         u         NA
    62    400   arbeid    3         u         NA
    63    400 soshjelp    3         u         NA
    64    400    trygd    3         u         NA
    65    400    Total   12         s         12
    66  Total    annet   11         s         11
    67  Total   arbeid   11         s         11
    68  Total soshjelp   11         s         11
    69  Total    trygd   11         s         11
    70  Total    Total   44         s         44

.

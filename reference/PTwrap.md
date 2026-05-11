# Wrapper to ProtectTable() with additional methods (partly experimental)

Additional values of "method" is possible. Each new method (wrapper
method) will make a call to ProtectTable() using a specific parameter
setting.

## Usage

``` r
PTwrap(
  ...,
  maxN = 3,
  method = "SimpleSingle",
  exeArgus = "C:/Tau/TauArgus.exe",
  pathArgus = getwd(),
  solverArgus = "FREE",
  methodArgus = "OPT",
  rgArgus = 0
)
```

## Arguments

- ...:

  Parameters to ProtectTable

- maxN:

  Parameter to ProtectTable

- method:

  Parameter to ProtectTable or a wrapper method (see details)

- exeArgus:

  Parameter to
  [`runArgusBatchFile`](https://rdrr.io/pkg/sdcTable/man/runArgusBatchFile.html)

- pathArgus:

  Parameter to
  [`createArgusInput`](https://rdrr.io/pkg/sdcTable/man/createArgusInput.html)

- solverArgus:

  Parameter "solver" to
  [`createArgusInput`](https://rdrr.io/pkg/sdcTable/man/createArgusInput.html)

- methodArgus:

  Parameter "method" to
  [`createArgusInput`](https://rdrr.io/pkg/sdcTable/man/createArgusInput.html)

- rgArgus:

  Parameter "rg" in "primSuppRules" in
  [`createArgusInput`](https://rdrr.io/pkg/sdcTable/man/createArgusInput.html)

## Value

See
[`ProtectTable`](https://statisticsnorway.github.io/ssb-easysdctable/reference/ProtectTable.md)

## Details

The wrapper methods are:

**Simple:** "SIMPLEHEURISTIC" with detectSingletons=FALSE

**SimpleSingle:** "SIMPLEHEURISTIC" with detectSingletons=TRUE when
protectZeros=FALSE and "SIMPLEHEURISTIC" with threshold=1 (can be
overridden by input) when protectZeros=TRUE

**SimpleSingleOld:** "SIMPLEHEURISTIC" with detectSingletons=TRUE

**TauArgus:** Tau-argus will be run according to the settings of the
other input parameters.

Using `rgArgus=0` is equivalent to calling ProtectTable() with  
`method = list(exe=exeArgus, typ="tabular", path=pathArgus,`  
`solver=solverArgus, method=methodArgus)))`

Other values of `rgArgus` is equivalent to calling ProtectTable() with  
`method = list(exe=exeArgus, typ="microdata", path=pathArgus,`  
`solver=solverArgus, method=methodArgus,`  
`primSuppRules=list(list(type="freq", n=maxN+1, rg=rgArgus )))))`

**TauArgusOPT:** As "TauArgus" with `methodArgus="OPT"`

**TauArgusMOD:** As "TauArgus" with `methodArgus="MOD"`

**TauArgusGH:** As "TauArgus" with `methodArgus="GH"`

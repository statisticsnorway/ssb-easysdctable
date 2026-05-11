# Table suppression - Shiny Gui

Run `PTgui` from the R console or use `PTguiApp` to make a server
application

## Usage

``` r
PTgui(
  data = NULL,
  language = "English",
  exeArgus = NULL,
  pathArgus = getwd(),
  maxNchoices = c(1:10, 12, 15, 20),
  ...
)

PTguiApp(
  language = "English",
  exeArgus = NULL,
  pathArgus = "",
  maxNchoices = c(1:10, 12, 15, 20),
  ...
)

PTguiNO(
  data = NULL,
  language = "Norwegian",
  exeArgus = NULL,
  pathArgus = getwd(),
  maxNchoices = c(1:10, 12, 15, 20),
  ...
)

PTguiAppNO(
  language = "Norwegian",
  exeArgus = NULL,
  pathArgus = "",
  maxNchoices = c(1:10, 12, 15, 20),
  ...
)
```

## Arguments

- data:

  NULL or a data.frame

- language:

  Menu language, "English" or "Norwegian".

- exeArgus:

  Tau-argus executable

- pathArgus:

  Folder for (temporary) tau-argus files

- maxNchoices:

  Choices of maxN

- ...:

  Further parameters sent to ProtectTable

## Value

Output from
[`ProtectTable`](https://statisticsnorway.github.io/ssb-easysdctable/reference/ProtectTable.md).
The output is returned invisibly (via
[`invisible`](https://rdrr.io/r/base/invisible.html)) which means that
it is not automatically printed to the console.

## Note

`PTguiApp()`: New for server

## Examples

``` r
  if (FALSE) { # \dontrun{
  
# Start the gui.
PTgui()

# Start Norwegian gui with example data and catch output
out <- PTguiNO(data=EasyData("z1w"))   

# Note: Change to TauArgus.exe-path in your computer
exeArgus <- "C:/TauArgus4.2.0b2/TauArgus.exe"

# Note: Change to an existing folder 
pathArgus <- "C:/Users/nnn/Documents"
    
# Start the gui with possibility to run tau-argus.       
PTgui(exeArgus=exeArgus, pathArgus=pathArgus)     
  
  } # }

```

#' Table suppression - Shiny Gui
#'
#' @encoding UTF8
#' 
#' @param data NULL or a data.frame 
#' @param language Menu language, "English" or "Norwegian".  
#' @param exeArgus Tau-argus executable 
#' @param pathArgus Folder for (temporary) tau-argus files
#' @param maxNchoices Choices of maxN
#' @param ... Further parameters sent to ProtectTable
#'
#' @return Output from \code{\link{ProtectTable}}. The output is returned invisibly 
#'  (via \code{\link{invisible}}) which means that it is not automatically printed to the console.
#'
#' @export
#' @import shiny
#' @importFrom utils write.table read.csv
#' @importFrom SSBtools matlabColon
#' 
#' @examples
#'   \dontrun{
#'   
#' # Start the gui.
#' PTgui()
#' 
#' # Start Norwegian gui with example data and catch output
#' out <- PTguiNO(data=EasyData("z1w"))   
#' 
#' # Tau-argus executable 
#' exeArgus <- "C:/TauArgus/TauWindows4.1.4_updated/TauArgus4.1.4/TauArgus.exe"
#' 
#' # Folder for (temporary) tau-argus files
#' pathArgus <- "C:/Users/nnn/Documents"
#'     
#' # Start the gui with possibility to run tau-argus.       
#' PTgui(exeArgus=exeArgus, pathArgus=pathArgus)     
#'   
#'   }
#'
#'
PTgui <- function(data=NULL, language="English", exeArgus=NULL, pathArgus=getwd(),
                  maxNchoices=c(1:10,12,15,20), ...){
  invisible(print(PTguiObj(data=data,language=language,exeArgus=exeArgus,pathArgus=pathArgus,
                           maxNchoices=maxNchoices, ...)))
}


#' @rdname PTgui 
#' @encoding UTF8
#' @export
PTguiNO <- function(data=NULL,  language="Norwegian",exeArgus=NULL, pathArgus=getwd(), 
                    maxNchoices=c(1:10,12,15,20), ...) 
  PTgui(data=data, language="Norwegian",exeArgus=exeArgus, pathArgus=pathArgus,
        maxNchoices=maxNchoices, ...)




######################################
#
#  To start the app run this code:
#
#    library(easySdcTable) # Version 0.2.1 needed
#    library(shiny)
#    source("PtGui.R")     # source this file
#    PTgui()
#
######################################

## First some function needed
rc =       #  rc = SSBtools:::reverse_chars  (not exportet in current version)
function(string) {
  string_split <- strsplit(as.character(string), split = "")
  reversed_split <- string_split[[1]][rev(matlabColon(1, nchar(string)))]  
  paste(reversed_split, collapse = "")
}

WholeNumber <- function(x){
  if(is.integer(x)) return(TRUE)
  if(!is.numeric(x)) return(FALSE)
  suppressWarnings(identical(as.numeric(as.integer(x)), as.numeric(x)))
}  

# Remove ".csv" from filename
UnCsv = function(s,csv=".csv")
  rc(sub(rc(csv),"",rc(s)))

# Could also use as.logical()
SingleOutput = function(x){
  if(x=="NULL") return(NULL)
  if(x=="TRUE") return(TRUE)
  if(x=="FALSE") return(FALSE)
  x
}

##############################
# Here starts the main function
##############################

PTguiObj <- function(data=NULL, language, exeArgus, pathArgus, maxNchoices, ...){
  
  guienvir = environment()
  
  names(maxNchoices) <- as.character(maxNchoices) 
  
  #Switch names and values
  s <- function(x){
    z=names(x)
    names(z) = x
    z}
  
  mt = c(
  "title",
  "filesep",
  "filedec",
  "fileread",
  "run",
  "log",
  "filesupp",
  "filesave",
  "showData",
  "input", 
  "supp",
  "freq", 
  "info",
  "method",
  "SIMPLEHEURISTIC",
  "SimpleSingle", 
  "HITAS",
  "OPT",
  "HYPERCUBE",
  "protectZeros",
  "maxN",
  "freqVar",
  "dimVar",
  "singleOutput",
  "NULL",
  "TRUE",
  "FALSE",
  "addName",  
  "totalFirst",  
  "namesAsInput",  
  "sortByReversedColumns",  
  "orderAsInput")   
  names(mt) = mt
  
  ## Define vt (values) and set some elements (only some in use)
  vt = mt
  vt["filesep"] = ","
  vt["filedec"] = "."

  if(language=="Norwegian"){
    vt["filesep"] = ";"
    vt["filedec"] = ","
    mt["title"] = "Prikking av frekvenstabeller"
    mt["filesep"] <- "Separator" 
    mt["filedec"] <- "Decimal point" 
    mt["fileread"] <- "Les inn input csv-fil" 
    mt["run"] <- "Beregn"
    #mt["log"] <- "" 
    mt["filesupp"] <- "Prikk" 
    mt["filesave"] <- "Lagre"
    mt["showData"] <- "Vis data" 
    mt["input"] <- "Input" 
    mt["supp"] <- "Prikket" 
    mt["freq"] <- "Uprikket" 
    mt["info"] <- "Info" 
    mt["method"] <- "Metode"
    mt["SIMPLEHEURISTIC"] <- "SIMPLE - Rask og (for) enkel"
    mt["SimpleSingle"] <- "SIMPLE med detectSingletons"
    mt["HITAS"] <- "HITAS - Vanlig metode" 
    mt["OPT"] <-  "OPT - Optimal, men tidkrevende"
    mt["HYPERCUBE"] = "HYPERCUBE - ikke for koblet"
    mt["protectZeros"] <- "Prikk 0-ere"
    mt["maxN"] <- "Prikk mindre eller lik"
    mt["freqVar"]      <- "Cellefrekvenser"
    mt["dimVar"] <- "Andre tabellvariabler"
    #mt["singleOutput"] <- "" 
    mt["NULL"] <- "Auto" 
    mt["TRUE"] <- "Ja" 
    mt["FALSE"] <- "Nei" 
    #mt["addName"] <- "" 
    #mt["totalFirst"] <- "" 
    #mt["namesAsInput"] <- "" 
    #mt["sortByReversedColumns"] <- "" 
    #mt["orderAsInput"] <- "" 
  }
  
  if(language=="English"){
    vt["filesep"] = ","
    vt["filedec"] = "."
    mt["title"] = "Table suppression"
    mt["filesep"] <- "Separator" 
    mt["filedec"] <- "Decimal point" 
    mt["fileread"] <- "Read input csv file" 
    mt["run"] <- "Run"
    #mt["log"] <- "" 
    mt["filesupp"] <- "Symbol" 
    mt["filesave"] <- "Save"
    mt["showData"] <- "Show data" 
    mt["input"] <- "Input" 
    mt["supp"] <- "Suppressed" 
    mt["freq"] <- "Freq" 
    mt["info"] <- "Info" 
    mt["method"] <- "Method" 
    #mt["SIMPLEHEURISTIC"] <- ""
    mt["SimpleSingle"] <- "SIMPLEHEURISTIC with detectSingletons"
    #mt["HITAS"] <- "" 
    #mt["OPT"] <-  ""
    mt["HYPERCUBE"] = "HYPERCUBE (not linked)"
    #mt["protectZeros"] <- ""
    #mt["maxN"] <- ""
    mt["freqVar"]      <- "Counts"
    mt["dimVar"] <- "Other table variables"
    #mt["singleOutput"] <- "" 
    mt["NULL"] <- "Auto" 
    mt["TRUE"] <- "Yes" 
    mt["FALSE"] <- "No" 
    #mt["addName"] <- "" 
    #mt["totalFirst"] <- "" 
    #mt["namesAsInput"] <- "" 
    #mt["sortByReversedColumns"] <- "" 
    #mt["orderAsInput"] <- "" 
  } 
  
  # for(i in 1:length(mt))cat(sprintf('  #mt["%s"] <- ""',mt[[i]]),"\n") # To generate template

re <- reactiveValues()
re$regn = FALSE
re$ferdig = FALSE
re$code = NULL
re$a = data
re$exeArgus = exeArgus
re$pathArgus = pathArgus
re$threeDots = list(...)

if(!is.null(exeArgus)) 
  tau =c(TauArgusOPT="TauArgusOPT",
         TauArgusMOD="TauArgusMOD",
         TauArgusGH="TauArgusGH")
else tau = NULL

inputData <- reactive({
  re$a
})


ColNames <- reactive({
  colnames(re$a)
})

IsInteger <- reactive({
  #sapply(re$a,class)=="integer"
  sapply(re$a,WholeNumber)
})

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
##options(shiny.maxRequestSize = 9*1024^2)

##############################
# Here starts shinyApp/ui
##############################
shinyApp(ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel(mt["title"]),
      tags$hr(),
      radioButtons('method',mt["method"],
                   c(s(mt["SIMPLEHEURISTIC"]), 
                     s(mt["SimpleSingle"]),
                     s(mt["HITAS"]), 
                     s(mt["OPT"]), 
                     s(mt["HYPERCUBE"]),
                     tau
                     ), 
                   'SIMPLEHEURISTIC'),
      checkboxInput('protectZeros', mt["protectZeros"], TRUE),
      radioButtons('maxN',mt["maxN"],
                   maxNchoices, # c("1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10)
                   3,inline=TRUE),
      uiOutput('freqVar'),
      uiOutput('dimVar'),
      tags$hr(),
      radioButtons('singleOutput', 'singleOutput',
                   c(s(mt["NULL"]), 
                     s(mt["TRUE"]), 
                     s(mt["FALSE"])), 
                   "NULL",inline=TRUE),
      checkboxInput('addName','addName', FALSE),
      checkboxInput('totalFirst','totalFirst', FALSE),
      checkboxInput('namesAsInput','namesAsInput', TRUE),
      checkboxInput('sortByReversedColumns','sortByReversedColumns', FALSE),
      checkboxInput('orderAsInput','orderAsInput', TRUE),
      tableOutput('contents') # NULL here
    ),
    mainPanel(
      tags$hr(),
      splitLayout(cellWidths = c(120,120,400), # 100), ##50),
      radioButtons('sep', mt["filesep"],
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   vt["filesep"]),
      radioButtons('dec', mt["filedec"] ,
                   c(Dot='.',
                     Comma =','),
                   vt["filedec"] ),
      fileInput('file1', mt["fileread"], 
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv')
                )
      ),
      tags$hr(),
      splitLayout(cellWidths = c(90,55,55,120,300),
        uiOutput('beregn'), 
        checkboxInput('verbose', 'log', FALSE),
        textInput('prikk',mt["filesupp"],"."), ### , width='40px'),
        downloadButton('downloadData', mt["filesave"]), 
      radioButtons('showData',mt["showData"],
                   c(s(mt["input"]), 
                     s(mt["supp"]), 
                     s(mt["freq"]), 
                     s(mt["info"])), 
                   'input',inline=TRUE)),
      tags$hr(),
      textOutput('code'),
      tableOutput('A')
    )
  )
),

##############################
# Here starts server
##############################
server = function(input, output, session) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    re$a =
      read.csv(inFile$datapath, header = TRUE,
               sep = input$sep, dec = input$dec)
    #re$b = NULL
    re$ferdig = FALSE
    updateRadioButtons(session, "showData",
                       selected = "input")
    NULL
  })

  output$beregn <- renderUI({
    actionButton("go", mt["run"], icon(beregnIcon()),
                 style= beregnStyle())
  })

  
  datasetDec <- reactive(input$dec)
  datasetSep <- reactive(input$sep)
  datasetPrikk <- reactive(input$prikk)
  


  output$A <- renderTable({
    if(input$showData == "input") return(re$a)
    if(re$regn & !re$ferdig & !is.null(re$a)){
      data = re$a
      callPT = as.call( c(
        list(
          as.name("ProtectTable"),
          data=as.name("data"),
          dimVar=input$dimVar,
          freqVar=input$freqVar, 
          method=input$method,
          protectZeros=input$protectZeros,
          maxN=as.numeric(input$maxN),
          addName = input$addName,
          totalFirst=input$totalFirst, 
          namesAsInput=input$namesAsInput,
          sortByReversedColumns=input$sortByReversedColumns,
          orderAsInput=input$orderAsInput,
          singleOutput = SingleOutput(input$singleOutput)),
        re$threeDots,
        list(
          exeArgus = re$exeArgus,
          pathArgus = re$pathArgus,
          infoAsFrame =TRUE,
          IncProgress = as.name("incProgress"),
          verbose = input$verbose)
        ))
      re$code = as.character(as.expression(callPT[seq_len(length(callPT)-5)]))
      re$b = try(withProgress(eval(callPT), #singleOutput=FALSE),
                          value=0.2, message= "Computing . . . please wait")) # mt[""] not working here
      #list2env(list(bb=re$b),envir=guienvir)
      if(class(re$b)=="try-error")
        re$b = list(data=data.frame(ERROR=re$b[[1]]))
        
      assign("reb",re$b,envir=guienvir)
      re$ferdig = TRUE
    }
    re$regn = FALSE
    if(input$showData == "info")  return(re$b$info)  
    if(!is.null(re$b$data) )  return(re$b$data)
    if(input$showData == "supp") return(re$b$suppressed)
    if(input$showData == "freq") return(re$b$freq)
    NULL 
  },digits=0, na =  "-" # , align = 'l'   # align med lengde 1 virker ikke på linux
    )

  output$code <- renderText({
   if(input$showData == "info") return(re$code)
    else return(NULL)
  })
  

  datasetInput <- reactive({
    if(input$showData == "input") return(re$a)
    if(input$showData == "info") return(data.frame(info=re$b$info))
    if(!is.null(re$b$data) )  return(re$b$data)
    if(input$showData == "supp") return(re$b$suppressed)
    if(input$showData == "freq") return(re$b$freq)
    NULL
  })

  datasetCode <- reactive({
    if(input$showData == "input") return("")
    if(input$showData == "info") return("_i")
    if(input$showData == "supp") return("_s")
    if(input$showData == "freq") return("_f")
    NULL
  })


beregnStyle <- reactive({
if(!re$ferdig & !is.null(re$a)){
  if(re$regn)
  style="color: #f00; background-color: #337ab7; border-color: #2e6da4"
    else
  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
}
else
  style="color: #eee; background-color: #aaa; border-color: #2e6da4"
style
})


beregnIcon <- reactive({
  if(!re$ferdig & !is.null(re$a)){
    if(re$regn)
      style="spinner"
    else
      style="play"
  }
  else
    style="caret-right"
  style
})



  observeEvent(input$go,{
    re$regn=TRUE
    updateRadioButtons(session, "showData",
                       selected = "supp")
  })

  observeEvent(input$dimVar,{
    updateCheckboxGroupInput(session,"freqVar",
       selected = input$freqVar[!(input$freqVar %in% input$dimVar)])
  })  
  
  observeEvent(input$freqVar,{
    updateCheckboxGroupInput(session,"dimVar",
                             selected = input$dimVar[!(input$dimVar %in% input$freqVar)])
  })  
  

  observeEvent(c(input$protectZeros, input$dimVar,input$freqVar, input$method, input$maxN,
                 input$addName, input$totalFirst, input$namesAsInput,
                 input$sortByReversedColumns, input$orderAsInput,
                 input$singleOutput),{
    re$ferdig = FALSE
    #re$regn=FALSE
    updateRadioButtons(session, "showData",
                       selected = "input")
  })


output$freqVar <- renderUI({
    colNames <- ColNames()
    if(is.null(colNames))
      return()
    isInteger <- IsInteger()
    checkboxGroupInput("freqVar", mt["freqVar"],
                       choices  = colNames,
                       selected = colNames[isInteger])
  })

output$dimVar <- renderUI({
  colNames <- ColNames()
  if(is.null(colNames))
    return()
  isInteger <- IsInteger()

  checkboxGroupInput("dimVar", mt["dimVar"], 
                     choices  = colNames,
                     selected = colNames[!isInteger])
})

output$downloadData <- downloadHandler(
  filename = function() { paste(UnCsv(input$file1$name),datasetCode(),'.csv', sep='') },
  content = function(file) {
    write.table(datasetInput(), file,
               dec = datasetDec(), sep = datasetSep(),
               col.names = (datasetCode() != "_i"),
               row.names=FALSE, na=datasetPrikk(),qmethod = "double")

  })

#session$onSessionEnded(stopApp)        # Stop app when closing

session$onSessionEnded( function(){
  stopApp(get("reb",envir=guienvir)) # re$b directly not working
  })  # Stop app when closing
},
options = list(launch.browser = TRUE)) # downloadData not working in RStudio window. Browser needed.
}










# Altmetrics R Shiny app
# Corey Bradshaw
# Flinders University

## remove everything
rm(list = ls())

# load libraries
library(shiny)
library(rAltmetric)
library(magrittr)
library(purrr)
library(dplyr)
library(rcrossref)
library(ggplot2)
library(ggpubr)
library(car)
library(ggrepel)
library(tidyverse)

## call functions
source(file.path("./functions/", "AICc.R"), local=T)
source(file.path("./functions/", "deltaIC.R"), local=T)
source(file.path("./functions/", "weightIC.R"), local=T)
source(file.path("./functions/", "linregER.R"), local=T)
source(file.path("./functions/", "AltFunc.R"), local=T)
source(file.path("./functions/", "safe_altmetrics.R"), local=T)
source(file.path("./functions/", "alm.R"), local=T)
source(file.path("./functions/", "setBackgroundColor.R"), local=T)

ui <- fluidPage(
  
  tags$head(
    tags$meta(name="google-site-verification", content="bVQ54bgpekDeCqy30pOSnGOMgbNXXV1AdwIoMRXSAAI"),
    tags$script(type='text/javascript', src="embed.js")
  ),
  
  setBackgroundColor(
    color = "#f8fbf9",
    gradient = c("linear"),
    direction = c("bottom")
  ),

  # title of app
  titlePanel("AltmetricShiny: fetch, sort & analyse Altmetric data"),
  
  wellPanel(style = "background: #e4feea",
    tags$a(href="https://github.com/cjabradshaw/AltmetricShiny", tags$img(height = 200, src = "altmetric_logo.png", style="float:right")),
    tags$p(style="font-family:Avenir", tags$i(class="fab fa-r-project", title="R Project"),"Shiny App by", tags$a(href="https://globalecologyflinders.com/people/#CJAB", "Corey Bradshaw "),
           tags$a(href = "mailto:corey.bradshaw@flinders.edu.au","(",tags$i(class="far fa-envelope"),"e-mail"),";",
           tags$a(href = "https://github.com/cjabradshaw", tags$i(class="fab fa-github",title="Github"),"Github)")),
    tags$h4(style="font-family:Avenir", "Preamble"),
    tags$p(style="font-family:Avenir", "Ever wanted to collate the", tags$a(href="https://www.altmetric.com/about-altmetrics/what-are-altmetrics/","Altmetric"),
    "data for your articles, but couldn't be bothered to do it manually? I've made the process substantially easier by designing this",
    tags$i(class="fab fa-r-project"),"Shiny app. All you need to do is collate a list of",tags$a(href="https://www.doi.org", "'digital object identifiers'"),
    "('doi') for the articles of interest, and the app does it all for you. The app also produces outputs that plot the distribution of not only the relevant
    Altmetric scores, but also the rank percentiles for each article relative to articles of the same age in the journal, and to all articles in the journal
    with Altmetric data."), tags$p(style="font-family:Avenir", "The app also gives you the option of fetching citation data from",
    tags$a(href="https://www.crossref.org", "Crossref"), "to examine the patterns between Altmetric and citation trends."),
    tags$p(style="font-family:Avenir", "This", tags$i(class="fab fa-github"), "Github ",
           tags$a(href = "https://github.com/cjabradshaw/AltmetricShiny", "repository"),
           "provides all the 'under-the-bonnet'",tags$i(class="fab fa-r-project"),"code for the app."),
  
    tags$h4(style="font-family:Avenir", "Instructions"),
    tags$ol(tags$li(tags$p(style="font-family:Avenir", "Create a delimited text file of", tags$strong("exactly the same format"), "as the example file in this",
           tags$a(href="https://github.com/cjabradshaw/AltmetricShiny/blob/main/doisampSm.csv","repository", tags$i(class="far fa-file")), ",
           although you can specify the delimit character (", tags$em("comma"),", ", tags$em("space"),", ", tags$em("tab"),").")),
           tags$li(tags$p(style="font-family:Avenir", "Load your delimited text file in the app by clicking the",tags$i(class="fas fa-file-import"),
           tags$strong("choose file"), "button.")),
           tags$li(tags$p(style="font-family:Avenir", "Select whether you want to include Crossref citation data (",
           tags$i(class="fas fa-bookmark"), tags$strong("include Crossref citation data?"), "). Downloading these data will increase processing time.")),
           tags$a(href="https://globalecologyflinders.com/", tags$img(height = 100, src = "GEL Logo Kaurna transparent.png", style="float:right",
                                                               title="Global Ecology @ Flinders University")),
           tags$li(tags$p(style="font-family:Avenir", "Select whether you want to add doi labels",
                          tags$i(class="fas fa-tag"), "to the plots.")),
           tags$li(tags$p(style="font-family:Avenir", "Select whether you want to restrict the results to a particular date range",
                          tags$i(class="fas fa-clock"))),
           tags$li(tags$p(style="font-family:Avenir", "Choose how you want the output file to be", tags$i(class="fas fa-sort"),
           "sorted by selecting one of the four choices in the drop-down menu:", tags$strong("Altmetric score"),",",tags$strong("context rank percentile"),",",
           tags$strong("all-time rank percentile"),", or",tags$strong("publication date"),".")),
           tags$li(tags$p(style="font-family:Avenir", "Click the", tags$i(class="fas fa-network-wired"), tags$strong("fetch data"), "button.")),
           tags$li(tags$p(style="font-family:Avenir", "Download the results table as a tab-delimited", tags$i(class="fas fa-file-alt"), "file by clicking the", tags$i(class="fas fa-download"),
           tags$strong("download"), "button.")))
  ),
  
  tabsetPanel(id="tabs",
              tabPanel(value="tab1", title="fetch & process data",
                       
                       sidebarLayout(
                         sidebarPanel(
                           wellPanel(style = "background: #c7fdd4",
                             fileInput("file1", label=tags$p(tags$i(class='fas fa-file-import'),"choose delimited file with doi data (1 column)"),
                                       multiple=F, buttonLabel="choose file", placeholder="no file selected"),
                             tags$hr(),
                             radioButtons("sep",label=tags$p(tags$i(class="fas fa-file-csv"),"separator"),choices=c(comma=',',space="",tab="\t"), inline=T),
                             checkboxInput("header1", "header?", TRUE),
                             tags$hr(),
                             radioButtons("CRcitations", label=tags$p(tags$i(class='fas fa-bookmark'), "include Crossref citation data?"), inline=T,
                                          choiceNames = list((icon("fas fa-thumbs-down")), (icon("fas fa-thumbs-up"))), choiceValues = list("no","yes")),
                             tags$hr(),
                             radioButtons("doilabs", label=tags$p(tags$i(class='fas fa-tag'), "include doi labels on plots?"), inline=T,
                                          choiceNames = list((icon("fas fa-thumbs-down")), (icon("fas fa-thumbs-up"))), choiceValues = list("no","yes")),
                             tags$hr(),
                             dateRangeInput("timerange",label=tags$p(tags$i(class='fas fa-clock'), "restrict results to a date range?"),
                                            start="1970-01-01", end=NULL, format="dd-M-yyyy", startview="decade", separator=" to "),
                             tags$hr(),
                             selectInput("sortind",label=tags$p(tags$i(class='fas fa-sort'), "choose sort index"), 
                                         c("Altmetric score"="as","context rank percentile"="cp","all-time rank percentile"="ap","publication date"="d")),
                             tags$hr(),
                             actionButton("fetchButton", label="fetch data",icon=shiny::icon("fas fa-network-wired")),
                             br(),
                             tags$small(style="font-family:Avenir", "(refresh page to clear data)"),
                             tags$hr(),
                             downloadButton('downloadData', 'download',icon = shiny::icon("download"))
                           ),
                         ),
                         
                         # open main panel
                         mainPanel(style = "background: #f8fbf9",
                           
                           fluidRow(
                             tags$div(id = "firstOutput", 
                                      h3("input data"),
                                      dataTableOutput("table1")) 
                           ),
                           
                           fluidRow(
                             tags$div(id = "placeholder") # the dynamic UI will be inserted relative to this placeholder
                           ),

                         ) # close mainPanel
                         
                       ) # sidebarLayout
              ), # end tab1
              
              tabPanel(value="tab2", title=tags$strong("highlights"), style = "background: #e9f8ec",
                       tags$br(),
                       tags$p(style="font-family:Avenir", tags$strong("Some summary highlights from your sample (user-set time restrictions are reflected in these values):")),
                       tags$img(height = 150, src = "highlights.png", style="float:right"),
                       
                       mainPanel(
                         
                         tags$script(type='text/javascript', ' _altmetric_embed_init(); '),
                         textOutput('error'),
                         
                         tags$br(),
                         htmlOutput('daterange'),
                         tags$br(),
                         htmlOutput('totDOI'),
                         tags$br(),
                         htmlOutput('missingAltm'),
                         tags$br(),
                         htmlOutput('Narticles'),
                         tags$br(),
                         htmlOutput('topAS'),
                         tags$br(),
                         htmlOutput('topASartDetails', inline=T),
                         uiOutput('topASButton'),
                         tags$br(),
                         htmlOutput('medianAS'),
                         tags$br(),
                         htmlOutput('rangeAS'),
                         tags$br(),
                         htmlOutput('IQrangeAS'),
                         tags$br(),
                         htmlOutput('topCP'),
                         tags$br(),
                         htmlOutput('topAP'),
                         tags$br(),
                         htmlOutput('nArtPol'),
                         tags$br(),
                         htmlOutput('SArtPol'),
                         tags$br(),
                         tags$head(tags$style("#daterange{font-family:Avenir;color:red}"
                         )),
                         tags$head(tags$style("#totDOI{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#missingAltm{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#Narticles{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#topAS{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#topASartDetails{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#medianAS{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#rangeAS{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#IQrangeAS{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#topCP{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#topAP{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#nArtPol{font-family:Avenir;font-style:italic}"
                         )),
                         tags$head(tags$style("#SArtPol{font-family:Avenir;font-style:italic}"
                         )),
                         tags$br()
                         
                       )
                       
              ), # end tab2
              
              tabPanel(value="tab3", title=tags$strong("histograms"), style = "background: #e9f8ec",
                       tags$br(),
                       tags$p(style="font-family:Avenir", "These histograms show the distribution of three different Altmetrics:"),
                       tags$ul(tags$li(tags$p(style="font-family:Avenir", tags$strong("A: Altmetric score"), " — this is the Altmetric '",
                                              tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/", "attention score"),
                                              "' as an indicator of the volume of attention an article has received (higher scores = more attention.")),
                                       tags$li(tags$p(style="font-family:Avenir", tags$strong("B: Context-rank percentile score"),
                                                      " — this is the percentile of the Altmetric",
                                                      tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/",
                                                             "attention score"), tags$a(href="https://help.altmetric.com/support/solutions/articles/6000233313-putting-the-altmetric-attention-score-in-context","rank"),
                                                              "in context of all articles of the same age in the journal. Here, low percentiles = top.")),
                                       tags$li(tags$p(style="font-family:Avenir", tags$strong("C: All time-rank percentile"), " — this is the percentile of the Altmetric",
                                                      tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/",
                                                             "attention score"), tags$a(href="https://help.altmetric.com/support/solutions/articles/6000233313-putting-the-altmetric-attention-score-in-context","rank"),
                                                              "in context of all articles with Altmetric data ever published in the journal. Here, low percentiles = top."))
                      ), # end ul
                      
                       mainPanel(
                         tags$br(),
                         tags$p(style="font-family:Avenir","In each panel below, the median metric is indicated in the panel's title and also shown as a red, dashed vertical line."),
                         plotOutput(width="150%","histplots"),
                         tags$br()
                       )
              ), # end tab3
              
              tabPanel(value="tab4", title=tags$strong("inter-relationships"), style = "background: #e9f8ec",
                       tags$br(),
                       tags$p(style="font-family:Avenir", "The following plots show the relationship between each of the two percentile rank measures
                              (by age and all-time) and the log of the Altmetric scores:"),
                       tags$ul(tags$li(tags$p(style="font-family:Avenir", tags$strong("A: Context-rank percentile score ~ Altmetric score"))),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("B: All time-rank percentile score ~ Altmetric score")))
                       ), # end ul
                       
                       mainPanel(
                         tags$br(),
                         tags$p(style="font-family:Avenir","The information-theoretic evidence ratio (ER) for a linear trend and the adjusted R",
                                tags$sup("2"), "for each relationship are:"),
                         htmlOutput('ERASCP'),
                         htmlOutput('ERASAP'),
                         tags$head(tags$style("#ERASCP{font-family:Avenir}"
                         )),
                         tags$head(tags$style("#ERASAP{font-family:Avenir}"
                         )),
                         tags$br(),
                         tags$p(style="font-family:Avenir","In each panel below, the loess trend is indicated by the blue line, and the linear trend by a
                                red dashed line."),
                         tags$br(),
                         plotOutput(width="150%", height="1200px", "interplots")
                       )
              ), # end tab4
              
              tabPanel(value="tab5", title=tags$strong("temporal trends"), style = "background: #e9f8ec",
                       tags$br(),
                       tags$p(style="font-family:Avenir", "The following time plots show the temporal patterns in the three different Altmetrics, as well
                              as the trend in policy-document citations:"),
                       tags$ul(tags$li(tags$p(style="font-family:Avenir", tags$strong("A: Altmetric score"), " — this is the Altmetric '",
                                              tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/", "attention score"),
                                              "' as an indicator of the volume of attention an article has received (higher scores = more attention.")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("B: Context-rank percentile score"),
                                              " — this is the percentile of the Altmetric",
                                              tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/",
                                                     "attention score"), tags$a(href="https://help.altmetric.com/support/solutions/articles/6000233313-putting-the-altmetric-attention-score-in-context","rank"),
                                              "in context of all articles of the same age in the journal. Here, low percentiles = top.")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("C: All time-rank percentile"), " — this is the percentile of the Altmetric",
                                              tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/",
                                                     "attention score"), tags$a(href="https://help.altmetric.com/support/solutions/articles/6000233313-putting-the-altmetric-attention-score-in-context","rank"),
                                              "in context of all articles with Altmetric data ever published in the journal. Here, low percentiles = top.")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("D: Policy-document citations"), " — the number of policy-document
                                              citations over time"))
                       ), # end ul
                       
                       mainPanel(
                         tags$br(),
                         tags$p(style="font-family:Avenir","The information-theoretic evidence ratio (ER) for a linear trend and the adjusted R",
                                tags$sup("2"), "for each index are:"),
                         htmlOutput('ERASt'),
                         htmlOutput('ERCPt'),
                         htmlOutput('ERAPt'),
                         tags$head(tags$style("#ERASt{font-family:Avenir}"
                                  )),
                         tags$head(tags$style("#ERCPt{font-family:Avenir}"
                         )),
                         tags$head(tags$style("#ERAPt{font-family:Avenir}"
                         )),
                         tags$br(),
                         tags$p(style="font-family:Avenir","In the panels below, the loess trend is indicated by the blue line, and the linear trend by a
                                red dashed line."),
                         tags$br(),
                         plotOutput(height="1600px", width="150%", "timeplots")
                       )
              ), # end tab5
              
              tabPanel(value="tab6", title=tags$strong("citation analysis"), style = "background: #e9f8ec",
                       tags$br(),
                       tags$p(style="font-family:Avenir", "If you selected to include Crossref citation data, the following plots show the power-law relationship
                              between article citations and each of the three different Altmetrics:"),
                       tags$ul(tags$li(tags$p(style="font-family:Avenir", tags$strong("A: Altmetric score"), " — this is the Altmetric '",
                                              tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/", "attention score"),
                                              "' as an indicator of the volume of attention an article has received (higher scores = more attention.")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("B: Context-rank percentile score"),
                                              " — this is the percentile of the Altmetric",
                                              tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/",
                                                     "attention score"), tags$a(href="https://help.altmetric.com/support/solutions/articles/6000233313-putting-the-altmetric-attention-score-in-context","rank"),
                                              "in context of all articles of the same age in the journal. Here, low percentiles = top.")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("C: All time-rank percentile"), " — this is the percentile of the Altmetric",
                                              tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/",
                                                     "attention score"), tags$a(href="https://help.altmetric.com/support/solutions/articles/6000233313-putting-the-altmetric-attention-score-in-context","rank"),
                                              "in context of all articles with Altmetric data ever published in the journal. Here, low percentiles = top."))
                       ), # end ul
                       
                       mainPanel(
                         tags$br(),
                         tags$p(style="font-family:Avenir","The information-theoretic evidence ratio (ER) for a linear trend and the adjusted R",
                                tags$sup("2"), "for each index are:"),
                         htmlOutput('ERASc'),
                         htmlOutput('ERCPc'),
                         htmlOutput('ERAPc'),
                         tags$head(tags$style("#ERASc{font-family:Avenir}"
                         )),
                         tags$head(tags$style("#ERCPc{font-family:Avenir}"
                         )),
                         tags$head(tags$style("#ERAPc{font-family:Avenir}"
                         )),
                         tags$br(),
                         tags$p(style="font-family:Avenir","In each panel below, the linear trend is indicated by a red dashed line."),
                         tags$br(),
                         plotOutput(height="1600px", width="150%", "citationplots")
                       )
              ), # end tab6
              
             tabPanel(value="tab7", title=tags$strong("output table descriptors"), style = "background: #e9f8ec",
                      tags$br(),
                      tags$a(href="https://flinders.edu.au/", tags$img(height = 100, src = "F_V_CMYK.png", style="float:right",title="Flinders University")),
                      tags$h2(style="font-family:Avenir", "Description of columns in the output file"),
                       
                       tags$ol(tags$li(tags$p(style="font-family:Avenir", tags$strong("first author"), "(COLUMN", tags$em("firstAu"),
                                                     ") — first author of the article")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("publication date"), "(COLUMN", tags$em("PublDate"),
                                              ") — DD/MM/YY date of the article's publication")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("article title"), "(COLUMN", tags$em("title"),
                                              ") — the first 20 characters of the article's title")),
                               tags$a(href="https://epicaustralia.org.au/", tags$img(height = 150, src = "CABAHlogo.png",
                                                                                     style="float:right",
                                                                                     title="ARC Centre of Excellence for Australian Biodiversity and Heritage")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("journal name"), "(COLUMN", tags$em("Journal"),
                                              ") — the journal in which the article was published")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("digital object identifier"), "(COLUMN", tags$em("doi"),
                                               ") — the article's", tags$a(href="https://www.doi.org", "doi"))),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("Altmetric attention score"), "(COLUMN", tags$em("AltmScore"),
                                                     ") — the downloaded Altmetric",
                                                     tags$a(href="https://www.altmetric.com/about-our-data/the-donut-and-score/", "attention score"),
                                                     "of the article")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("'In-context' rank percentile (by age)"), "(COLUMN", tags$em("rnkCxtPc"),
                                              ") — the percentile of the Altmetric rank relative to all articles in the journal of the same age")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("'In-context' rank percentile (all time)"), "(COLUMN", tags$em("rnkAllPc"),
                                              ") — the percentile of the Altmetric rank relative to all articles in the journal")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("Number of Crossref citations"), "(COLUMN", tags$em("CRcites"),
                                              ") — total number of citations to date according to", tags$a(href="https://www.crossref.org", "Crossref"),
                                              "(if Crossref citations selected)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("Number of Crossref citations/year"), "(COLUMN", tags$em("CRcitesYr"),
                                              ") — total number of citations/year to date according to", tags$a(href="https://www.crossref.org", "Crossref"),
                                              "(if Crossref citations selected)")),
                               tags$li(tags$p(style="font-family:Avenir", tags$strong("Number of policy-document citations"), "(COLUMN", tags$em("polCit"),
                                              ") — total number of citations found in", tags$a(href="https://www.altmetric.com/blog/announcing-our-new-and-improved-policy-tracker/",
                                                                                                   "policy documents"), "around the world for articles in the sample")),
                               tags$a(href="https://github.com/cjabradshaw/AltmetricShiny/blob/main/LICENSE",
                                      tags$img(height = 50, src = "GNU GPL3.png", style="float:right", title="GNU General Public Licence v3.0")),
                               tags$br()
                       ) # end ol
         ) # end tab7
  ) # end tabsetPanel
  
  
) # close fluidPage

server <- function(input, output, session) {
  
  observeEvent(input$tabs, {
    
    if(input$tabs == "tab1"){
      
      output$table1 <- renderDataTable({
        
        file_to_read = input$file1
        if(is.null(file_to_read)){
          return()
        }
        read.table(file_to_read$datapath, sep=input$sep, header=input$header1)
      }) # end output table1
      
      datin <- reactive({
        fileinp <- input$file1
        if(is.null(fileinp)){return()}
        inpdat <- data.frame(read.table(fileinp$datapath, sep=input$sep, header = input$header1))
        return(inpdat)
      }) # end datin

      sortInd <- reactiveValues()
      observe({
        sortInd$x <- as.character(input$sortind)
      })
    
      # when action button pressed ...
      observeEvent(input$fetchButton, {
        removeUI("div:has(>#firstOutput)")
        insertUI(
          selector = "#placeholder",
          where = "afterEnd", # inserts UI after the end of the placeholder element
          ui = fluidRow(
            h3("fetching data ... (this can take some time depending on the number of articles in your sample)"),
              output$etable <- renderDataTable({
                if(is.null(datin())){return ()}
                results.list <<- AltFunc(datsamp=(datin()), InclCit=input$CRcitations, sortindex=sortInd$x)
                results <<- results.list$rnkDatAsort
              })))
      }) # end observeEvent
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("AltmetricListOut", ".txt", sep = "")
        },
        
        content = function(file) {
          
          write.table(results, file, sep="\t", row.names = F)
        }
      )
    } # end if for tab1
    
    if(input$tabs == "tab2"){
      
      date_start <- as.Date(input$timerange[1], origin = "1970-01-01")
      date_end <- as.Date(input$timerange[2], origin = "1970-01-01")
      results <<- results[(results$PublDate >= date_start) & (results$PublDate <= date_end), ]
      
      output$daterange <- renderText({
                paste("SPECIFIED DATE RANGE: ", format(date_start, "%d %B %Y"), " to ", format(date_end, "%d %B %Y"), sep="")
        })
      output$totDOI <- renderText({
        paste("• number of digital object identifiers (doi) processed in this sample: ", results.list$missingAltm + length(results$AltmScore), sep="")
      })
      output$missingAltm <- renderText({
        paste("• number of digital object identifiers (doi) with missing Altmetrics data in this sample: ", results.list$missingAltm, sep="")
      })
      output$Narticles <- renderText({
        paste("• number of articles with Altmetrics data in this sample: ", length(results$AltmScore), sep="")
      })
      output$topAS <- renderText({
        paste("• top Altmetric attention score in this sample: ", round(results$AltmScore[1], 1), sep="")
      })
      output$topASButton <- renderUI({
        tags$meta(name="citation_doi", content=results$doi[1])
        tags$script(type='text/javascript', src="embed.js")
        withTags(div('data-badge-type'="donut", 'data-doi'=results$doi[1], className="altmetric-embed"))
      })
      output$medianAS <- renderText({
        paste("• median Altmetric attention score for this sample: ", round(10^median(log10(results$AltmScore), na.rm=T), 1), sep="")
      })
      output$rangeAS <- renderText({
        paste("• Altmetric attention score range for this sample: ", round(min(results$AltmScore, na.rm=T), 1)," — ",round(max(results$AltmScore, na.rm=T), 1), sep="")
      })
      output$IQrangeAS <- renderText({
        paste("• Altmetric attention score inter-quartile range for this sample: ", round(10^quantile(log10(results$AltmScore), probs=0.25, na.rm=T), 1)," — ",round(quantile(results$AltmScore, probs=0.75, na.rm=T), 1), sep="")
      })
      output$topASartDetails <- renderText({
        paste("• top Altmetric-scoring article in this sample: ", results$firstAu[1], ". ", as.numeric(format(results$PublDate[1], format="%Y")), ". '", results$title[1], "'", ". ", results$Journal[1], " doi:", results$doi[1], sep="")
      })
      output$topCP <- renderText({
        paste("• median top percentile rank (by age) of this sample: ", "top ",round(median(results$rnkCxtPc, na.rm=T), 2),"%", sep="")
      })
      output$topAP <- renderText({
        paste("• median top percentile rank (all time) of this sample: ", "top ",round(median(results$rnkAllPc, na.rm=T), 2),"%", sep="")
      })
      output$nArtPol <- renderText({
        paste("• number articles in this sample cited in policy documents: ", length(which(results$polCit > 0)), sep="")
      })
      output$SArtPol <- renderText({
        paste("• total number of policy-document citations in this sample: ", sum(results$polCit), sep="")
      })

    } # end if for tab2
    
    if(input$tabs == "tab3"){
      
      date_start <- as.Date(input$timerange[1], origin = "1970-01-01")
      date_end <- as.Date(input$timerange[2], origin = "1970-01-01")
      results <<- results[(results$PublDate >= date_start) & (results$PublDate <= date_end), ]

      output$histplots <- renderPlot({
        input$histplots
        
        Ctheme = theme(
          axis.title.x = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 18))
        
        AS <- ggplot(data=results, aes(log10(AltmScore))) + 
          geom_histogram(bins=round(dim(results)[1]/5),col="grey",fill="black",alpha=0.5) +
          geom_vline(xintercept=median(log10(results$AltmScore)), linetype=2, color="red", size=1) +
          labs(title=paste("median = ", round(10^median(log10(results$AltmScore)),1),sep="")) +
          labs(x="log Altmetric score", y="frequency") +
          Ctheme
          
        CP <- ggplot(data=results, aes((rnkCxtPc))) + 
          geom_histogram(bins=round(dim(results)[1]/5),col="grey",fill="black",alpha=0.5) +
          geom_vline(xintercept=median((results$rnkCxtPc)), linetype=2, color="red", size=1) +
          labs(title=paste("median = top ", round(median((results$rnkCxtPc)),1),"%",sep="")) +
          labs(x="rank % (by age)", y=NULL) +
          Ctheme
        
        AP <- ggplot(data=results, aes((rnkAllPc))) + 
          geom_histogram(bins=round(dim(results)[1]/5),col="grey",fill="black",alpha=0.5) +
          geom_vline(xintercept=median((results$rnkAllPc)), linetype=2, color="red", size=1) +
          labs(title=paste("median = top ", round(median((results$rnkAllPc)),1),"%",sep="")) +
          labs(x="rank % (all time)", y=NULL) +
          Ctheme
        
        ggarrange(AS, CP, AP,
                  labels=c("A", "B", "C"),
                  ncol=3, nrow=1)
        })
      } # end if for tab3

    if(input$tabs == "tab4"){
      
      date_start <- as.Date(input$timerange[1], origin = "1970-01-01")
      date_end <- as.Date(input$timerange[2], origin = "1970-01-01")
      results <<- results[(results$PublDate >= date_start) & (results$PublDate <= date_end), ]
      
      output$ERASCP <- renderText({
        ER <- linregER(log10(results$AltmScore), logit(results$rnkCxtPc/100))[1]
        ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
        paste("A. evidence ratio = ", ERf,";",
              " R<sup>2</sup>",  " = ",round(linregER(log10(results$AltmScore), logit(results$rnkCxtPc/100))[2], 3),sep="")
      })
      output$ERASAP <- renderText({
        ER <- linregER(log10(results$AltmScore), logit(results$rnkAllPc/100))[1]
        ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
        paste("B. evidence ratio = ", ERf,";",
              " R<sup>2</sup>",  " = ", round(linregER(log10(results$AltmScore), logit(results$rnkAllPc/100))[2], 3),sep="")
      })

      if (input$doilabs == "yes") {
        output$interplots <- renderPlot({
          input$interplots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14))
            
          ASCP <- ggplot(data=results, aes(x=log10(AltmScore), y=logit(rnkCxtPc/100))) + 
            geom_point() +
            geom_smooth() +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
            labs(x=NULL, y="logit rank proportion (by age)") +
            geom_label_repel(aes(label = doi),
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            scale_radius(c(0.45,0.45)) +
            Ctheme
            
          ASAP <- ggplot(data=results, aes(x=log10(AltmScore), y=logit(rnkAllPc/100))) + 
            geom_point() +
            geom_smooth() +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
            labs(x="log Altmetric score", y="logit rank proportion (all time)") +
            geom_label_repel(aes(label = doi),
                             box.padding   = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            scale_radius(c(0.45,0.45)) +
            Ctheme
          ASAP
          
          ggarrange(ASCP, ASAP,
                    labels=c("A", "B"),
                    ncol=1, nrow=2
          )
        })
      } # end if
      
      if (input$doilabs == "no") {
        output$interplots <- renderPlot({
          input$interplots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14))
          
          ASCP <- ggplot(data=results, aes(x=log10(AltmScore), y=logit(rnkCxtPc/100))) + 
            geom_point() +
            geom_smooth() +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
            labs(x=NULL, y="logit rank proportion (by age)") +
            scale_radius(c(0.45,0.45)) +
            Ctheme
          
          ASAP <- ggplot(data=results, aes(x=log10(AltmScore), y=logit(rnkAllPc/100))) + 
            geom_point() +
            geom_smooth() +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
            labs(x="log Altmetric score", y="logit rank proportion (all time)") +
            scale_radius(c(0.45,0.45)) +
            Ctheme
          
          ggarrange(ASCP, ASAP,
                    labels=c("A", "B"),
                    ncol=1, nrow=2
          )
        })
      } # end if
      
    } # end if for tab4
    
    if(input$tabs == "tab5"){
      
      date_start <- as.Date(input$timerange[1], origin = "1970-01-01")
      date_end <- as.Date(input$timerange[2], origin = "1970-01-01")
      results <<- results[(results$PublDate >= date_start) & (results$PublDate <= date_end), ]
      
      output$ERASt <- renderText({
        ER <- linregER(results$PublDate, log10(results$AltmScore))[1]
        ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
        paste("A. evidence ratio = ", ERf,";",
              " R<sup>2</sup>",  " = ",round(linregER(results$PublDate, log10(results$AltmScore))[2], 3),sep="")
      })
      output$ERCPt <- renderText({
        ER <- linregER(results$PublDate, results$rnkCxtPc)[1]
        ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
        paste("B. evidence ratio = ", ERf,";",
              " R<sup>2</sup>",  " = ", round(linregER(results$PublDate, results$rnkCxtPc)[2], 3),sep="")
      })
      output$ERAPt <- renderText({
        ER <- linregER(results$PublDate, results$rnkAllPc)[1]
        ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
        paste("C. evidence ratio = ", ERf,";",
              " R<sup>2</sup>",  " = ", round(linregER(results$PublDate, results$rnkAllPc)[2], 3),sep="")
      })

      if (input$doilabs == "yes") {
        if (input$CRcitations == "yes") {
          
          output$timeplots <- renderPlot({
          input$timeplots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14))
          
          ASt <- ggplot(data=results, aes(x=PublDate, y=log10(AltmScore), size=CRcites)) + 
            geom_smooth(show.legend = F) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
            geom_point(alpha=0.6, colour="green") +
            scale_size(range = c(0.1, 10), name="citations") +
            labs(x=NULL, y="log Altmetric score") +
            geom_label_repel(aes(label = doi),
                             size=3,
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          ASt
          
          CPt <- ggplot(data=results, aes(x=PublDate, y=rnkCxtPc, size=CRcites)) + 
            geom_smooth(show.legend = F) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
            geom_point(alpha=0.6, colour="green", show.legend = T) +
            scale_size(range = c(0.1, 10), name="citations") +
            labs(x=NULL, y="rank % (by age)") +
            geom_label_repel(aes(label = doi),
                             size=3,
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          
          APt <- ggplot(data=results, aes(x=PublDate, y=rnkAllPc, size=CRcites)) + 
            geom_smooth(show.legend = F) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
            geom_point(alpha=0.6, colour="green", show.legend = T) +
            scale_size(range = c(0.1, 10), name="citations") +
            labs(x=NULL, y="rank % (all time)") +
            geom_label_repel(aes(label = doi),
                             size=3,
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          
          polDat <- results[which(results$polCit > 0),]
          Polt <- ggplot(data=polDat, aes(x=PublDate, y=polCit, size=CRcites)) + 
            geom_point(alpha=0.6, colour="green", show.legend = T) +
            scale_size(range = c(0.1, 10), name="citations") +
            geom_smooth(show.legend = F) +
            labs(x=NULL, y="policy citations") +
            geom_label_repel(aes(label = doi),
                             size=3,
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          
          ggarrange(ASt, CPt, APt, Polt, 
                    labels=c("A", "B", "C", "D"),
                    ncol=1, nrow=4
                    )
        })
      } # end citations yes if
      
        if (input$CRcitations == "no") {
        output$timeplots <- renderPlot({
          input$timeplots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14))
          
          ASt <- ggplot(data=results, aes(x=PublDate, y=log10(AltmScore))) + 
            geom_smooth(show.legend = F) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
            geom_point(show.legend = T) +
            labs(x=NULL, y="log Altmetric score") +
            geom_label_repel(aes(label = doi),
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          
          CPt <- ggplot(data=results, aes(x=PublDate, y=rnkCxtPc)) + 
            geom_smooth(show.legend = F) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
            geom_point(show.legend = T) +
            labs(x=NULL, y="rank % (by age)") +
            geom_label_repel(aes(label = doi),
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          
          APt <- ggplot(data=results, aes(x=PublDate, y=rnkAllPc)) + 
            geom_smooth(show.legend = F) +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
            geom_point(show.legend = F) +
            labs(x=NULL, y="rank % (all time)") +
            geom_label_repel(aes(label = doi),
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          
          polDat <- results[which(results$polCit > 0),]
          Polt <- ggplot(data=polDat, aes(x=PublDate, y=polCit)) + 
            geom_point(show.legend = F) +
            geom_smooth(show.legend = F) +
            labs(x=NULL, y="policy citations") +
            geom_label_repel(aes(label = doi),
                             box.padding = 0.35, 
                             point.padding = 0.5,
                             segment.color = 'grey50',
                             segment.alpha = 0.7,
                             show.legend = F,
                             alpha=0.7) +
            Ctheme
          
          ggarrange(ASt, CPt, APt, Polt, 
                    labels=c("A", "B", "C", "D"),
                    ncol=1, nrow=4
          )
        })
      } # end citations no if
      } # end doi labels if

      if (input$doilabs == "no") {
        if (input$CRcitations == "yes") {
          
          output$timeplots <- renderPlot({
            input$timeplots
            
            Ctheme = theme(
              axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 14))
            
            ASt <- ggplot(data=results, aes(x=PublDate, y=log10(AltmScore), size=CRcites)) + 
              geom_smooth(show.legend = F) +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
              geom_point(alpha=0.6, colour="green") +
              scale_size(range = c(0.1, 10), name="citations") +
              labs(x=NULL, y="log Altmetric score") +
              Ctheme
            
            CPt <- ggplot(data=results, aes(x=PublDate, y=rnkCxtPc, size=CRcites)) + 
              geom_smooth(show.legend = F) +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
              geom_point(alpha=0.6, colour="green", show.legend = T) +
              scale_size(range = c(0.1, 10), name="citations") +
              labs(x=NULL, y="rank % (by age)") +
              Ctheme

            APt <- ggplot(data=results, aes(x=PublDate, y=rnkAllPc, size=CRcites)) + 
              geom_smooth(show.legend = F) +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
              geom_point(alpha=0.6, colour="green", show.legend = T) +
              scale_size(range = c(0.1, 10), name="citations") +
              labs(x=NULL, y="rank % (all time)") +
              Ctheme

            polDat <- results[which(results$polCit > 0),]
            Polt <- ggplot(data=polDat, aes(x=PublDate, y=polCit, size=CRcites)) + 
              geom_point(alpha=0.6, colour="green", show.legend = T) +
              scale_size(range = c(0.1, 10), name="citations") +
              geom_smooth(show.legend = F) +
              labs(x=NULL, y="policy citations") +
              Ctheme

            ggarrange(ASt, CPt, APt, Polt, 
                      labels=c("A", "B", "C", "D"),
                      ncol=1, nrow=4
            )
          })
        } # end citations yes if
        
        if (input$CRcitations == "no") {
          output$timeplots <- renderPlot({
            input$timeplots
            
            Ctheme = theme(
              axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 14))
            
            ASt <- ggplot(data=results, aes(x=PublDate, y=log10(AltmScore))) + 
              geom_smooth(show.legend = F) +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
              geom_point(show.legend = T) +
              labs(x=NULL, y="log Altmetric score") +
              Ctheme

            CPt <- ggplot(data=results, aes(x=PublDate, y=rnkCxtPc)) + 
              geom_smooth(show.legend = F) +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
              geom_point(show.legend = T) +
              labs(x=NULL, y="rank % (by age)") +
              Ctheme

            APt <- ggplot(data=results, aes(x=PublDate, y=rnkAllPc)) + 
              geom_smooth(show.legend = F) +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red", show.legend = F) +
              geom_point(show.legend = F) +
              labs(x=NULL, y="rank % (all time)") +
              Ctheme

            polDat <- results[which(results$polCit > 0),]
            Polt <- ggplot(data=polDat, aes(x=PublDate, y=polCit)) + 
              geom_point(show.legend = F) +
              geom_smooth(show.legend = F) +
              labs(x=NULL, y="policy citations") +
              Ctheme

            ggarrange(ASt, CPt, APt, Polt, 
                      labels=c("A", "B", "C", "D"),
                      ncol=1, nrow=4
            )
          })
        } # end citations no if
      } # end doi labels if
      
    } # end if for tab5

    if(input$tabs == "tab6"){
      
      if (input$CRcitations == "yes") {
        if (input$doilabs == "no") {
          
        date_start <- as.Date(input$timerange[1], origin = "1970-01-01")
        date_end <- as.Date(input$timerange[2], origin = "1970-01-01")
        results <<- results[(results$PublDate >= date_start) & (results$PublDate <= date_end), ]
        
        output$ERASc <- renderText({
          dataERAScNA <- data.frame(log10(results$CRcitesYr), log10(results$AltmScore))
          dataERAScREL <- na.omit(do.call(data.frame,lapply(dataERAScNA,function(x) replace(x, is.infinite(x), NA))))
          ER <- linregER(dataERAScREL[,1], dataERAScREL[,2])[1]
          ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
          paste("A. evidence ratio = ", ERf,";",
                " R<sup>2</sup>",  " = ",round(linregER(dataERAScREL[,1], dataERAScREL[,2])[2], 3),sep="")
        })
        output$ERCPc <- renderText({
          dataERCPcNA <- data.frame(log10(results$CRcitesYr), logit(results$rnkCxtPc/100))
          dataERCPcREL <- na.omit(do.call(data.frame,lapply(dataERCPcNA,function(x) replace(x, is.infinite(x), NA))))
          ER <- linregER(dataERCPcREL[,1], dataERCPcREL[,2])[1]
          ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
          paste("B. evidence ratio = ", ERf,";",
                " R<sup>2</sup>",  " = ", round(linregER(dataERCPcREL[,1], dataERCPcREL[,2])[2], 3),sep="")
        })
        output$ERAPc <- renderText({
          dataERAPcNA <- data.frame(log10(results$CRcitesYr), logit(results$rnkAllPc/100))
          dataERAPcREL <- na.omit(do.call(data.frame,lapply(dataERAPcNA,function(x) replace(x, is.infinite(x), NA))))
          ER <- linregER(dataERAPcREL[,1], dataERAPcREL[,2])[1]
          ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
          paste("C. evidence ratio = ", ERf,";",
                " R<sup>2</sup>",  " = ", round(linregER(dataERAPcREL[,1], dataERAPcREL[,2])[2], 3),sep="")
        })

        output$citationplots <- renderPlot({
          input$citationplots
          
          Ctheme = theme(
            axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 14))
          
          ASc <- ggplot(data=results, aes(x=log10(CRcitesYr), y=log10(AltmScore))) + 
            geom_point() +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
            labs(x=NULL, y="log Altmetric score") +
            Ctheme
          
          CPc <- ggplot(data=results, aes(x=log10(CRcitesYr), y=logit(rnkCxtPc/100))) + 
            geom_point() +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
            labs(x=NULL, y="logit rank proportion (by age)") +
            Ctheme
          
          APc <- ggplot(data=results, aes(x=log10(CRcitesYr), y=logit(rnkAllPc/100))) + 
            geom_point() +
            geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
            labs(x="log Crossref citations/year", y="logit rank proportion (all time)") +
            Ctheme
          
          ggarrange(ASc, CPc, APc,
                    labels=c("A", "B", "C"),
                    ncol=1, nrow=3
          )
        })
        } # end no doi labels if
        
        if (input$doilabs == "yes") {
          
          date_start <- as.Date(input$timerange[1], origin = "1970-01-01")
          date_end <- as.Date(input$timerange[2], origin = "1970-01-01")
          results <<- results[(results$PublDate >= date_start) & (results$PublDate <= date_end), ]
          
          output$ERASc <- renderText({
            dataERAScNA <- data.frame(log10(results$CRcitesYr), log10(results$AltmScore))
            dataERAScREL <- na.omit(do.call(data.frame,lapply(dataERAScNA,function(x) replace(x, is.infinite(x), NA))))
            ER <- linregER(dataERAScREL[,1], dataERAScREL[,2])[1]
            ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
            paste("A. evidence ratio = ", ERf,";",
                  " R<sup>2</sup>",  " = ",round(linregER(dataERAScREL[,1], dataERAScREL[,2])[2], 3),sep="")
          })
          output$ERCPc <- renderText({
            dataERCPcNA <- data.frame(log10(results$CRcitesYr), logit(results$rnkCxtPc/100))
            dataERCPcREL <- na.omit(do.call(data.frame,lapply(dataERCPcNA,function(x) replace(x, is.infinite(x), NA))))
            ER <- linregER(dataERCPcREL[,1], dataERCPcREL[,2])[1]
            ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
            paste("B. evidence ratio = ", ERf,";",
                  " R<sup>2</sup>",  " = ", round(linregER(dataERCPcREL[,1], dataERCPcREL[,2])[2], 3),sep="")
          })
          output$ERAPc <- renderText({
            dataERAPcNA <- data.frame(log10(results$CRcitesYr), logit(results$rnkAllPc/100))
            dataERAPcREL <- na.omit(do.call(data.frame,lapply(dataERAPcNA,function(x) replace(x, is.infinite(x), NA))))
            ER <- linregER(dataERAPcREL[,1], dataERAPcREL[,2])[1]
            ERf <- ifelse(ER > 100, format(ER, format="e", digits=3), round(ER, 3))
            paste("C. evidence ratio = ", ERf,";",
                  " R<sup>2</sup>",  " = ", round(linregER(dataERAPcREL[,1], dataERAPcREL[,2])[2], 3),sep="")
          })
          
          output$citationplots <- renderPlot({
            input$citationplots
            
            Ctheme = theme(
              axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 14))
            
            ASc <- ggplot(data=results, aes(x=log10(CRcitesYr), y=log10(AltmScore))) + 
              geom_point() +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
              labs(x=NULL, y="log Altmetric score") +
              geom_label_repel(aes(label = doi),
                               box.padding = 0.35, 
                               point.padding = 0.5,
                               segment.color = 'grey50',
                               segment.alpha = 0.7,
                               show.legend = F,
                               alpha=0.7) +
              scale_radius(c(0.45,0.45)) +
              Ctheme
            
            CPc <- ggplot(data=results, aes(x=log10(CRcitesYr), y=logit(rnkCxtPc/100))) + 
              geom_point() +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
              labs(x=NULL, y="logit rank proportion (by age)") +
              geom_label_repel(aes(label = doi),
                               box.padding = 0.35, 
                               point.padding = 0.5,
                               segment.color = 'grey50',
                               segment.alpha = 0.7,
                               show.legend = F,
                               alpha=0.7) +
              scale_radius(c(0.45,0.45)) +
              Ctheme
            
            APc <- ggplot(data=results, aes(x=log10(CRcitesYr), y=logit(rnkAllPc/100))) + 
              geom_point() +
              geom_smooth(method=lm, se=F, linetype="dashed", color="dark red") +
              labs(x="log Crossref citations/year", y="logit rank proportion (all time)") +
              geom_label_repel(aes(label = doi),
                               box.padding = 0.35, 
                               point.padding = 0.5,
                               segment.color = 'grey50',
                               segment.alpha = 0.7,
                               show.legend = F,
                               alpha=0.7) +
              scale_radius(c(0.45,0.45)) +
              Ctheme
            
            ggarrange(ASc, CPc, APc,
                      labels=c("A", "B", "C"),
                      ncol=1, nrow=3
            )
          })
        } # end yes doi labels if
        
      } # end citations if
      
    } # end if for tab6
    
  }) # end tab Events
  
  session$onSessionEnded(stopApp)
  
} # end server

shinyApp(ui, server)


library(shiny)
library(shinydashboard)
library(dygraphs)
library(d3heatmap)
library(keboola.shiny.lib)

shinyUI(
    keboolaPage(
        bootstrapPage(
            tabsetPanel(
                # first tab content
                tabPanel("Dashboard",
                    h1("Executive Summary"),
                    fluidRow(  #row 1
                        box(width=6, height="200px",title="Select Data", solidHeader = T, status = "primary",
                            selectInput("gaTable", "GA Table", choices=c()),
                            selectInput("medium_select", 
                                        "Select Traffic Channel",
                                        choices = c("All" = "total",
                                                    "Direct" = "(none)",
                                                    "Email" = "email",
                                                    "SEO" = "organic",
                                                    "Referral" = "referral",
                                                    "Social" = "social")),
                            helpText("Select which channel to show in the plots below.  For demo purposes it loads sessions, but could easily be revenue, goals or other metrics and segments.")
                        ),
                        box(width=6, height="200px",title = "What is this?", solidHeader = T, status = "info",
                            p("This is a demo Google Analytics dashboard using R, Shiny and various other features."),
                            p("Check out the source code via the Github link to the left. ")
                        )
                    ),
                  h2("   Top Level Trends"),
                  fluidRow(
                    valueBoxOutput("WoW"),
                    valueBoxOutput("MoM"),
                    valueBoxOutput("YoY")
                  ),
                  h2("Plots"),
                  fluidRow(
                    tabBox(title = "", width=12,
                           tabPanel(title=tagList(shiny::icon("line-chart"), "Trend"),
                                    helpText("Click and drag on the plot to zoom and select date ranges.  Events that have been uploaded to the MySQL backend are shown as dotted lines, and unusual time points are annotated below them."),
                                    selectInput("agg_select", 
                                                "Select Trend Type",
                                                choices = c("Day" = "day",
                                                            "Week" = "week",
                                                            "Month" = "month",
                                                            "Annual" = "year"),
                                                selected = "week"
                                    ),
                                    textOutput("date_shown"),
                                    dygraphOutput("plot1", height = "600px")
                           ),
                           tabPanel(title=tagList(shiny::icon("calendar"), "Day Of Week"),
                                    helpText("Zoom in by selecting with the mouse.  This heatmap representation of the data helps show if the day of the week holds any patterns."),
                                    textOutput("current_week"),
                                    d3heatmapOutput("heatmap", height="800px")
                           ),
                           tabPanel(title=tagList(shiny::icon("thumbs-up"), "Event Impact"),
                                    helpText("The estimated impact of the events on total sessions for two weeks is shown below, as well as if the impact can be considered statistically significant.  Get more detail in the analysis section."),
                                    plotOutput("CausalPlotSummary")
                           )
                        )
                    )
                ),
                # second tab content
                tabPanel("Join",
                      h1("Select Data to Join"),
                      helpText("Upload a CSV file with two columns: date (YYYY-MM-DD) and eventname (string), to store it in the database. 0.5MB limit.")),
                      fluidRow(
                        box(height="200px",
                            title = "Upload file",
                            solidHeader = T,
                            status ="primary",
                            fileInput("eventUploadFile",
                                      "Marketing Events",
                                      accept = c("text/csv", ".csv")),
                            
                        box(height="200px",
                            title = "Public Upload", solidHeader=T, status="info",
                            p("For this demo, anyone can load up data into the MySQL database which will appear in the table below.  This data is persistent between Shiny app sessions."),
                            p("These events are then used in the trend graphs and in the Analyse section.  Other applications include uploading CRM data, offline transactions or campaign info to bind to your GA data.")
                        ),
                        box(width = 12, solidHeader = T, status="success",
                            title = "Current Data Loaded",
                            DT::dataTableOutput("eventTable")                
                        )    
                    )
                ),
                      # third tab content
                      tabPanel("Analyse",
                              h2("Deepdive Analysis"),
                              helpText("This section demonstrates some R packages that work with time-series data.  This leverages the strength of using R for dashboarding. "),
                              tabBox("Select Analysis", width = 12,
                                     tabPanel(tagList(shiny::icon("thumbs-up"), "Event Effects"),
                                              helpText("The estimated impact of the uploaded events on total traffic is shown below.  It uses code similar to the GA Effect app linked in the menu to the left."),
                                              uiOutput("multiple_plots")
                                     ),
                                     tabPanel(tagList(shiny::icon("exclamation"), "Anomaly Detection"),
                                              helpText("What dates had unusual activity?  This uses Twitter's AnomalyDetection package to find unusual activity.  Fine tune the dates shown using the controls below."),
                                              selectInput("medium_select2", 
                                                          "Select Plot Channel",
                                                          choices = c("All" = "total",
                                                                      "Direct" = "(none)",
                                                                      "Email" = "email",
                                                                      "SEO" = "organic",
                                                                      "Referral" = "referral",
                                                                      "Social" = "social")
                                              ),
                                              selectInput("agg_select2", 
                                                          "Select Plot Date Type",
                                                          choices = c("Day" = "day",
                                                                      "Week" = "week",
                                                                      "Month" = "month"),
                                                          selected = "day"
                                              ),
                                              sliderInput("max_anoms",
                                                          "Sensitivity",
                                                          min=0.0,
                                                          max=0.4,
                                                          value = 0.1,
                                                          step = 0.05),
                                              plotOutput("anomalyPlot"),
                                              DT::dataTableOutput("anomalyTable")
                                              
                        )
                    )
                )
            )
        ), appTitle = "GA Dashboard"
    )
)
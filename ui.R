
library(shiny)
library(shinydashboard)
library(dygraphs)
library(d3heatmap)
library(keboola.shiny.lib)

shinyUI(
    keboolaPage(
        dashboardPage(
            dashboardHeader(disable = TRUE),
            dashboardSidebar(
                h3("What is this?"),
                helpText("This is a demo Google Analytics dashboard using R, Shiny and various other features."),
                selectInput("gaTable", "GA Table", choices=c()),
                helpText("Select a table that contains the results of your GA extractor query"),
                selectInput("gaMetric", "Metric", choices=c()),
                selectInput("gaDimension", "Dimension",choices=c())
            ),
            dashboardBody(
              tabsetPanel(
                # first tab content
                tabPanel("Setup Events",
                    h4("Select Event Table and Columns"),
                    fluidRow(
                        column(4,
                            helpText("Select the table that contains at least 1 date column and one text column (ex: Name)"),           
                            selectInput("eventsTable", "Events Table", choices=c())
                        ),
                        column(4,
                            helpText("Select the date column.  Note, if the format is not Y-m-d, please provide the format in the textbox"),
                            fluidRow(
                                column(6,selectInput("eventDate", "Date Column", choices=c())),
                                column(6,textInput("eventDateFormat", "Format", value="Y-m-d"))
                            )
                        ),
                        column(4,
                            helpText("Select a column which identifies the event."),
                            selectInput("evenName", "Name Column", choices=c())     
                        )
                    ),
                    h4("Event Data"),
                    DT::dataTableOutput("eventDatatable")
                ),
                tabPanel("Plots",
                    h4("Filter by Dimension"),
                    selectInput("gaSegment", "Select a Segment",
                                choices = c("All" = "total",
                                             "Direct" = "(none)",
                                             "Email" = "email",
                                             "SEO" = "organic",
                                             "Referral" = "referral",
                                             "Social" = "social")),
                    fluidRow(
                        tabBox(title = "", width=12,
                            tabPanel(title=tagList(shiny::icon("thumbs-up"), "Top Level Trends"),
                                 fluidRow(
                                     valueBoxOutput("WoW"),
                                     valueBoxOutput("MoM"),
                                     valueBoxOutput("YoY")
                                 )                     
                            ),
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
                              tabPanel(title=tagList(shiny::icon("flash"), "Event Impact"),
                                helpText("The estimated impact of the events on total sessions for two weeks is shown below, as well as if the impact can be considered statistically significant.  Get more detail in the analysis section."),
                                plotOutput("CausalPlotSummary")
                              )
                            )
                    )
                ),
                
                  # third tab content
                  tabPanel("Analyse",
                          h2("Deepdive Analysis"),
                          helpText("This section demonstrates some R packages that work with time-series data.  This leverages the strength of using R for dashboarding. "),
                          tabBox("Select Analysis", width = 12,
                            tabPanel(tagList(shiny::icon("flash"), "Event Effects"),
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
            )
        ), appTitle = "GA Dashboard"
    )
)
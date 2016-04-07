
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
                helpText("Feel free to fork it on Github (upper right) and make it your own. "),
                selectInput("gaTable", "GA Table", choices=c()),
                helpText("Select a table that contains the results of your GA extractor query"),
                selectInput("events", "Events Table", choices=c()),
                helpText("Select the table that contains at least 1 date column and one text column (ex: Name)"),
                selectInput("medium_select", 
                            "Select Traffic Channel",
                            choices = c("All" = "total",
                                        "Direct" = "(none)",
                                        "Email" = "email",
                                        "SEO" = "organic",
                                        "Referral" = "referral",
                                        "Social" = "social"))
            ),
            dashboardBody(
              tabsetPanel(
                # first tab content
                tabPanel("Dashboard",
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
            )
        ), appTitle = "GA Dashboard"
    )
)

library(shiny)
library(keboola.shiny.lib)

source('functions.R')

## for the loop over plots
max_plots <- 10 # options()$shinyMulti$max_plots

shinyServer(function(input, output, session) {
  
    # Create instance of keboola/shiny-lib
    klib <- KeboolaShiny$new()
  
    # This is the startup method, it will run whenever the page is loaded
    keboola <- reactive({
        
        # here we create and register a list of our inputs
        # doing this means that when configs are loaded these inputs will be automatically updated
        appInputs <- list(
            list(id="gaTable", type="select")
        )
        # in.c-ex-google-analytics-kbcga
        # start it up
        # for a full list of options available to the startup method please see the documentation
        ret <- klib$startup(
            list(
                appTitle = "GA Dashboard", 
                tables="all", 
                inputList = appInputs, 
                dataToSave = ga_data,
                forkButtonRef="https://github.com/keboola/ga-dashboard/fork"
            )
        )
        return(ret$loginInfo)
    })
    
    sourceData <- reactive({
        if (keboola()$ready) {

            sessionsSegments <- c("All" = "total",
                                      "Direct" = "(none)",
                                      "Email" = "email",
                                      "SEO" = "organic",
                                      "Referral" = "referral",
                                      "Social" = "social")
                
            sd <- klib$sourceData()()
            sd$gaData <- sd[[input$gaTable]]
            sd$gaData$id <- sd$gaData$idprofile <- sd$gaData[['_timestamp']] <- NULL
            print("NAMES of GADATA")
            print(names(sd$gaData))
            updateSelectInput(session, "gaMetric", choices=c("Select"="",names(sd$gaData)))
            updateSelectInput(session, "gaDimension", choices=c("Select"="",names(sd$gaData)))
            sd
        } else {
            NULL
        }
    })
    
    observe({
        if (keboola()$ready) {
            
            # grab a list of all table objects (meta data about the table, not the actual data)
            tables <- klib$client$listTables(bucket = klib$bucket)
            
            # grab the name of each table object
            tableNames <- lapply(tables, function(t) { t$name })
            
            #update our GA select input element with tablenames
            updateSelectInput(session,"gaTable",choices=c(tableNames))
            
        } else {
            print("server.R not ready")
            NULL
        }
    })
    
    config <- callModule(appConfig,"kb",klib$kfig)
    
    ga_data <- reactive({
        if (input$gaTable != "") {
            gadata <- sourceData()[[input$gaTable]]
            gadata$date <- as.Date(gadata$date)
            if (input$gaMetric == "" || input$gaDimension == "") {
                return(NULL)
            }
            gadata$metric <- as.numeric(gadata[[input$gaMetric]])
            gadata$dimension <- as.factor(gadata[[input$gaDimension]])
            # remove unused columns
            gadata <- gadata[,names(gadata) %in% c("dimension", "metric", "date")]
            # get totals
            data <- tidyr::spread(gadata, dimension, metric)
            data$total <- rowSums(as.matrix(data[,-1]), na.rm=T)
            # update the segment select input with our new columnnames
            updateSelectInput(session,"gaSegment",choices=names(data[,-1]))
            data
        } else {
            return(NULL)
        }
    })
  
    anomalyData <- reactive({
        data <- ga_data()
        choice <- input$gaSegment
        ## make reactive to choice
        agg    <- input$agg_select
    
        agg_data <- aggregate_data(data[,c('date', choice)], agg)
        ad <- try(anomalyDetect(agg_data[,c('date', choice)], direction="both", max_anoms = 0.05))
    
        if(!is.error(ad)){
            return(ad)
        } else {
            NULL
        }
    
    })
  
    anomalyData2 <- reactive({
        data <- ga_data()
        choice <- input$medium_select2
        ## make reactive to choice
        agg    <- input$agg_select2
        max_a <- input$max_anoms
    
        agg_data <- aggregate_data(data[,c('date', choice)], agg)
        ad <- try(anomalyDetect(agg_data[,c('date', choice)], direction="both", max_anoms = max_a))
    
        if(!is.error(ad)){
            return(ad)
        } else {
            NULL
        }
    })
  
    output$anomalyPlot <- renderPlot({
        anomalyData2()$plot
    })
  
    output$anomalyTable <- DT::renderDataTable({
        a_table <- anomalyData2()$anoms
        a_table$timestamp <- as.Date(as.character(a_table$timestamp))
        names(a_table) <- c("Anomaly Date", "Value")
        a_table 
    })
  
        output$date_shown <- renderText({
        pdata <- plot_date_data()
        min_date <- as.character(min(pdata$date))
        max_date <- as.character(max(pdata$date))
        paste0(min_date, " to ", max_date)
    })
  
    plot_date_data <- reactive({
        data <- ga_data()
        if(is.null(data)){
            return(NULL)
        }
    
        choice <- input$gaSegment
        plot1_dates <- input$plot1_date_window
    
        min_date <- plot1_dates[1]
        max_date <- plot1_dates[2]
    
        pdata <- data[data$date > min_date &
                    data$date < max_date,]
    
        pdata <- pdata[,colnames(pdata) %in% c("date", choice)]
    
        pdata
    })
  
    output$heatmap <- renderD3heatmap({
    #     validate(
    #       need(is.null(plot_date_data()), "Plot data")
    #     )
    
        hm_data <- plot_date_data()
    
        week_pad <- period_function_generator("week", pad=T)
    
        hm_data$wday <- lubridate::wday(hm_data$date, label=T)
    
        hm_data$week <- paste0(year(hm_data$date), 
                           " W", 
                           week_pad(hm_data$date)
        )
    
        names(hm_data) <- c("date", "sessions", "wday", "week")
    
        hm_data <- tbl_df(hm_data)
        
        # sum up any duplicates
        hm_data_grp <- group_by(hm_data,date,wday,week) 
        hm_data <- summarise(hm_data_grp, sessions = sum(sessions),  na.rm=T)
    
        hm_f <- tidyr::spread(hm_data[,c("sessions","wday","week")], 
                          wday, 
                          sessions)
    
        hm_m <- as.matrix(hm_f %>% dplyr::select(-week))
        row.names(hm_m) <- factor(hm_f$week)
    
        hm_m[is.na(hm_m)] <- 0
    
        d3heatmap(hm_m, 
              colors = "Blues",
              Rowv = FALSE,
              Colv = FALSE,
              labRow = row.names(hm_m),
              labCol = colnames(hm_m)
        )
    })
  
    output$current_week <- renderText({
    
        paste("Currently Week", week(today()))
    
    })
  
    output$plot1 <- renderDygraph({
    
        data   <- ga_data()
        choice <- input$gaSegment
        agg    <- input$agg_select
        events <- eventData()
        anomalies <- anomalyData()$anoms
    
        agg_data <- data[,c('date', choice)]
        names(agg_data) <- c('date', 'metric')
    
        agg_data <- aggregate_data(data[,c('date', choice)], agg)
    
    #     ## aggregate data if not agg == date
    #     if(agg %in% c('week', 'month', 'year')){
    #       agg_data <- tbl_df(agg_data)
    #       date_type_function <- period_function_generator(agg, pad=T)
    #       
    #       agg_data <-  agg_data %>% 
    #         mutate(period_type = paste0(year(date),
    #                                     "_",
    #                                     date_type_function(date))) %>%
    #         group_by(period_type) %>%
    #         summarise(date = min(date),
    #                   metric = sum(metric))
    #       
    #       agg_data <- data.frame(agg_data)
    #     }
    
        ## dygraph needs a time-series, zoo makes it easier
        ts_data <- zoo(agg_data[,choice], 
                   order.by = agg_data[,'date'])
    
        start <- Sys.Date() - 450
        end <- Sys.Date() - 1
    
        d <- dygraph(ts_data, main=str_to_title(paste(choice, "Sessions "))) %>%
            dyRangeSelector(dateWindow = c(start, end)) %>%
            dySeries("V1", color = "#8a48a4", label = str_to_title(choice) )
    
        ## a dreaded for loop, but it makes sense for this?
        if(!is.null(events)){
            for(i in 1:length(events$date)){
                d <- d %>% dyEvent(events$date[i], label = events$eventname[i]) 
            }
        }
        if(!is.null(anomalies) && nrow(anomalies) > 0){
            for(i in 1:length(anomalies$timestamp))
                d <- d %>% dyAnnotation(anomalies$timestamp[i], 
                                text = paste("Anomaly"),
                                tooltip = paste("Anomaly:", anomalies$anoms[i]),
                                attachAtBottom = T,
                                width = 60,
                                height = 25)
        }
    
        return(d)
    
    })
  
    output$WoW <- renderValueBox({
    
        data <- ga_data()
        choice <- input$gaSegment
        
        wow_data <- data[,c('date', choice)]
        
        valueBoxTimeOnTime(wow_data, "week")
    })
  
    output$MoM <- renderValueBox({
        data <- ga_data()
        choice <- input$gaSegment
    
        mom_data <- data[,c('date', choice)]
    
        valueBoxTimeOnTime(mom_data, "month")
    })
  
    output$YoY <- renderValueBox({
    
        data <- ga_data()
        choice <- input$gaSegment
        
        yoy_data <- data[,c('date', choice)]
        
        valueBoxTimeOnTime(yoy_data, "monthYear")
    
    })
  
  eventData <- reactive({
    eventUploaded <- input$eventUploadFile
    
    if(is.null(eventUploaded)){
      ## get it from the SQL database instead
      uploaded_csv <- try(loadData("onlineGAshiny_events"))
      
      if(!is.error(uploaded_csv)){
        return(uploaded_csv)        
      } else {
        message("No event data found in SQL")
        return(NULL)
      }
    } else {
      uploaded_csv <- try(read.csv(eventUploaded$datapath, stringsAsFactors = F))
      if(!is.error(uploaded_csv)){
        ## check uploaded_csv
        if(all(names(uploaded_csv) %in% c('date', 'eventname'))){
          uploaded_csv <- uploaded_csv[complete.cases(uploaded_csv),]
          ## convert dates
          dates_guessed <- as.Date(uploaded_csv$date,
                                   guess_formats(uploaded_csv$date, 
                                                 c("Y-m-d", "m-d-Y")))
          message("Dates: ", dates_guessed)
          uploaded_csv$date <- dates_guessed
          #uploadBool <- overWriteTable("onlineGAshiny_events", uploaded_csv)
          #if(uploadBool) message("File uploaded successfully.")
          return(uploaded_csv)          
        } else {
          stop("File did not include 'date' and 'eventname' columns.  Found: ", names(uploaded_csv))
        }       
        
      } else {
        message("Problem uploading file.")
        return(NULL)
      }
    }
    
  })
  
    output$eventTable <- DT::renderDataTable({
        eventData()
    })
  
    causalImpactData <- reactive({
    
        ci       <- ga_data()
        events   <- eventData()
    
        ts_data <- zoo(ci[,'total'], 
                    order.by = ci[,'date'])
    
        ci_list <- getCausalImpactList(ts_data, events)
    
        ## get data via Reduce(rbind, lapply(events$eventname, function(x) ci_list[[x]]$summary)))
    })
  
    causalTable <- reactive({
        ci_list <- causalImpactData()
        events   <- eventData()
    
        ## output cumulative totals only
        ci_l <- plyr::ldply(lapply(events$eventname, function(x) 
            ci_list[[x]]$summary[row.names(ci_list[[x]]$summary) == "Cumulative",])
        )
        ci_l$eventname <- events$eventname
    
        ci_l
    })
  
  output$CausalPlotSummary <- renderPlot({
    ct <- causalTable()
    
    if(!is.null(ct)){
      ct$sig <- ifelse(ct$p < ct$alpha, "Significant" , "Not Significant")
      
      gg <- ggplot(ct, aes(x = eventname, y = AbsEffect, fill = sig)) + theme_bw()
      
      gg <- gg + geom_bar(stat="identity")  + scale_fill_brewer(palette = "Set1")
      
      gg <- gg + ylab("Estimated Total Impact") + xlab("Event Type") + guides(fill=FALSE, alpha=FALSE)
      
      gg <- gg + geom_text(aes(x = eventname, y = -100, label = sig))
      
      print(gg)
      
    }
  })
  
  
  ## loop over possible events
  output$multiple_plots <- renderUI({
    events <- eventData()
    
    if(!is.null(events)){
      ci_sig_output_list <- lapply(1:nrow(events), function(i){
        ci_sig_name <- paste("ci_sig", i, sep="")
        valueBoxOutput(ci_sig_name)
      })
      
      ci_rel_output_list <- lapply(1:nrow(events), function(i){
        ci_sig_name <- paste("ci_rel", i, sep="")
        valueBoxOutput(ci_sig_name)
      })
      
      null_plot_list <- lapply(1:nrow(events), function(i){
        null_plot_name <- paste("null_plot", i, sep="")
        box(width = 8, status="primary", dygraphOutput(null_plot_name, height = "300px"))
      })
      
      ci_table_list <- lapply(1:nrow(events), function(i){
        ci_table_name <- paste("ci_table", i, sep="")
        DT::renderDataTable(ci_table_name)
      })
      
      for(i in 1:nrow(events)){
        add <- c(null_plot_list[i], ci_sig_output_list[i],ci_rel_output_list[i] )
        if(i>1){
          output_list <- c(output_list, add)        
        } else {
          output_list <- add
        }
        
      }
      
      all_the_plots <- do.call(tagList, output_list)
      
      return(fluidRow(all_the_plots))     
      
    } else {
      
      return(NULL)
      
    }
    
    
  })
  
  
  ### loops over events
  for(i in 1:max_plots){
    #local needed to have reevaluation of i
    local({
      my_i <- i
      
      #       ## the dynamic output for this Table     
      #       ci_table_name <- paste("ci_table", i, sep="")
      #       output[[ci_table_name]] <- DT::renderDataTable({
      #         ci_l <- causalImpactData()
      #         events <- eventData()
      #         ## accessing the CI list summary for this event
      #         ci <- ci_l[[events$eventname[my_i]]]$summary
      # 
      #       })     
      
      
      
      ## the dynamic output for this Box     
      ci_sig_name <- paste("ci_sig", i, sep="")
      output[[ci_sig_name]] <- renderValueBox({
        ci_l <- causalImpactData()
        events <- eventData()
        ## accessing the CI list summary for this event
        ci <- ci_l[[events$eventname[my_i]]]$summary
        
        if(!is.null(ci)){
          
          if(ci$p[1] < 0.05){
            valueBox(
              "Significant", 
              "The probability of obtaining this effect by chance alone is small",
              icon = icon("thumbs-o-up"),
              color = "green"
            )
          } else {
            valueBox(
              "No Effect", 
              "Its likely that any effect observed is just by chance",
              icon = icon("thumbs-o-down"),
              color = "red"
            )
          }
        } else {
          valueBox(
            "No data", 
            ""
          )
          
        }
      })
      
      ## dynamic output for Box 2
      ci_rel_name <- paste("ci_rel", i, sep="")
      output[[ci_rel_name]] <- renderValueBox({
        ci_l <- causalImpactData()
        events <- eventData()
        ## accessing the CI list summary for this event
        ci <- ci_l[[events$eventname[my_i]]]$summary
        
        if(!is.null(ci)){
          relEffect <- round(ci$RelEffect[2], 2) * 100
          
          valueBox(
            paste(as.character(relEffect), "%"), 
            paste("Estimated Average % Change"),
            icon = icon("pie-chart"),
            color = "blue"
          )
        }else {
          valueBox(
            "No data", 
            ""
          )
          
        }
      })
      
      ## dynamic output for plot result
      null_plot_name <- paste("null_plot", i, sep="")
      output[[null_plot_name]] <- renderDygraph({
        
        ci_l <- causalImpactData()
        events <- eventData()
        ci <- ci_l[[events$eventname[my_i]]]$series
        
        event_date <- as.Date(events$date[my_i])
        
        start <- event_date - options()$myCausalImpact$test_time * 4
        enddd <- event_date + options()$myCausalImpact$test_time
        
        dygraph(data=ci[,c('response', 'point.pred', 'point.pred.lower', 'point.pred.upper')], 
                main=paste("Impact of Event:", str_to_title(events$eventname[my_i]))) %>%
          dyEvent(date = event_date, events$eventname[my_i]) %>%
          dySeries(c('point.pred.lower', 'point.pred','point.pred.upper'), label='Expected') %>%
          dySeries('response', label="Observed")
      })
      
      
    }) # end local
  } # end loop over plots
  
})

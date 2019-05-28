# This contains the server logic to render the UI
#------------------------------------------------

server <- function(input, output, session) {
  reloadData <- reactiveTimer(216000000, session)

  wBO <- reactive({
    reloadData()
    weekScraper()
  })

  mBO <- reactive({
    reloadData()
    monthScraper()
  })

  qBO <- reactive({
    reloadData()
    quarterScraper()
  })
  
  #----- Dashboard -----
  observeEvent(input$infoDashboard, {
    showModal(
      modalDialog(easyClose = TRUE, size = "l",
        h2("Dashboard Information", align = "center"),
        includeMarkdown("infoDashboard.md")
      )
    )
  })
  
  # Box 1 - Domestic
  output$compD <- renderPlot({
    plotCompBarplot(wBO(), mBO(), qBO(), "domestic")
  })
  
  # Box 1 - International
  output$compI <- renderPlot({
    plotCompBarplot(wBO(), mBO(), qBO(), "international")
  })
  
  # Box 1 - Global
  output$compG <- renderPlot({
    plotCompBarplot(wBO(), mBO(), qBO(), "global")
  })
  
  # Box 2 - Donut
  output$donut <- renderPlot({
    # Create test data.
    dat <- data.frame(perc = c(58, 42),
                      prog = factor(c("Sales to Date", "Remaining"),
                                    levels = c("Sales to Date", "Remaining")))
    
    # Add addition columns, needed for drawing with geom_rect.
    dat$fraction = dat$perc / sum(dat$perc)
    dat <- dat[order(dat$fraction), ]
    dat$perc <- paste0(dat$perc, "%")
    dat$ymax <- cumsum(dat$fraction)
    dat$ymin <- c(0, head(dat$ymax, n = -1))
    dat$y <- (dat$ymax + dat$ymin)/2
    
    ggplot(dat, aes(fill = prog, ymax = ymax, ymin = ymin, y = y, xmax = 2, xmin = 1, label = perc)) +
      scale_fill_manual(values = c("mediumpurple3", "lightsteelblue1"), name = "") +
      geom_rect(colour = "grey30") +
      coord_polar(theta = "y") +
      xlim(c(0, 3)) +
      geom_label(x = 3, show.legend = FALSE, size = 8, label.padding = unit(0.5, "lines")) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = c(0.5, 0.9),
            legend.text = element_text(size = 16))
  })
  
  # Box 3 - Week
  output$moviesW <- renderPlot({
    plotMoviePareto(wBO())
  })
  
  # Box 3 - Month
  output$moviesM <- renderPlot({
    plotMoviePareto(mBO())
  })
  
  # Box 3 - Quarter
  output$moviesQ <- renderPlot({
    plotMoviePareto(qBO())
  })
  
  #----- Analytics -----
  observeEvent(input$infoAnalytics, {
    showModal(
      modalDialog(easyClose = TRUE, size = "l",
                  h2("Analytics Information", align = "center"),
                  includeMarkdown("infoAnalytics.md")
      )
    )
  })
  
  # Prediction Tab
  output$predFilms_UI <- renderUI({
    if (input$predTime == "Q3") {
      dat <- movieDB[movieDB$startDate >= "2019-07-01" & movieDB$startDate <= "2019-09-30",]
      jul <- dat$film[substr(dat$startDate, 6, 7) == "07"]
      aug <- dat$film[substr(dat$startDate, 6, 7) == "08"]
      sep <- dat$film[substr(dat$startDate, 6, 7) == "09"]
      pickerInput("predFilms", label = "Film(s) to View:",
                  choices = list("July" = jul, "August" = aug, "September" = sep),
                  multiple = TRUE, selected = c(jul,aug,sep)[1:3],
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 1"))
    } else if (input$predTime == "Q4") {
      dat <- movieDB[movieDB$startDate >= "2019-10-01" & movieDB$startDate <= "2019-12-31",]
      oct <- dat$film[substr(dat$startDate, 6, 7) == "10"]
      nov <- dat$film[substr(dat$startDate, 6, 7) == "11"]
      dec <- dat$film[substr(dat$startDate, 6, 7) == "12"]
      pickerInput("predFilms", label = "Film(s) to View:",
                  choices = list("October" = oct, "November" = nov, "December" = dec),
                  multiple = TRUE, selected = c(oct,nov,dec)[1:3],
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 1"))
    } else {
      dat <- movieDB[movieDB$startDate >= input$predDates[1] & movieDB$startDate <= input$predDates[2],]
      pickerInput("predFilms", label = "Film(s) to View:",
                  choices = dat$film,
                  multiple = TRUE, selected = dat$film[1:3],
                  options = list(`actions-box` = TRUE, `selected-text-format` = "count > 1"))
    }
  })
  
  futureBO <- reactive({
    if (input$update == 0)
      return()
    
    isolate({
      getFutureBO(input$predTime, input$predDates, input$predFilms)
    })
  })
  
  observeEvent(futureBO(), {
    output$predGross <- renderPlotly({
      validate(
        need(length(input$predFilms) > 0, "Please select at least one film and click 'Update Plots'.")
      )
      
      g <- ggplot(futureBO(), aes(x = Date, y = Total)) + geom_line(size = 2, color = "mediumpurple3") +
        xlab("Date") + ylab("Projected Boxoffice Gross") +
        scale_y_continuous(labels = scales::dollar) +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 16),
              axis.text = element_text(size = 12))
      ggplotly(g)
    })
    
    output$predLifecycle <- renderPlotly({
      validate(
        need(length(input$predFilms) > 0, "Please select at least one film and click 'Update Plots'.")
      )
      
      dat1 <- futureBO()[,-ncol(futureBO())]
      dat1 <- reshape2::melt(dat1, id.vars = "Date", variable.name = "Film", value.name = "Gross")
      dat1$Film <- as.factor(dat1$Film)
      
      g <- ggplot(dat1, aes(x = Date, y = Gross, color = Film)) + geom_line(size = 2) +
        xlab("Date") + ylab("Projected Boxoffice Gross") + 
        scale_y_continuous(labels = scales::dollar) +
        scale_color_manual(values = colFunc(length(unique(dat1$Film))), name = "") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 16),
              axis.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              legend.position = "top")
      p <- ggplotly(g)
      p %>% layout(legend = list(orientation = "h",
                                 xanchor = "center", x = 0.5,
                                 yanchor = "top", y = 100))
    })
  })
  
  # Historical Tab
  output$histComparison <- renderPlotly({
    if (input$histTime == "Last Week") {
      x <- rep(seq(floor_date(Sys.Date()-7, unit = "w", week_start = 5),
                   floor_date(Sys.Date(), unit = "w", week_start = 5), by = "d"),
               each = 2)
    } else if (input$histTime == "Last Month") {
      x <- rep(seq(floor_date(Sys.Date()-day(Sys.Date())-1, unit = "m"),
                   rollback(Sys.Date()), by = "d"),
               each = 2)
    } else {
      x <- rep(seq(floor_date(Sys.Date()-months(3), unit = "q"),
                   rollback(ceiling_date(Sys.Date()-months(3), unit = "q")), by = "d"),
               each = 2)
    }
    w <- rep(c("Projected", "Actual"), length(x)/2); w <- factor(w, levels = c("Projected", "Actual"))
    z <- data.frame(x, w, stringsAsFactors = FALSE)
    wg <- round(rnorm(1, 125000, 25000))
    y <- c()
    for (i in 1:nrow(z)) {
      y <- c(y,
             round(ifelse(wday(z$x[i]) == 1, wg*rnorm(1,.18,.03),
                   ifelse(wday(z$x[i]) == 2, wg*rnorm(1,.07,.02),
                   ifelse(wday(z$x[i]) == 2, wg*rnorm(1,.06,.02),
                   ifelse(wday(z$x[i]) == 2, wg*rnorm(1,.07,.02),
                   ifelse(wday(z$x[i]) == 2, wg*rnorm(1,.19,.03),
                   ifelse(wday(z$x[i]) == 2, wg*rnorm(1,.21,.04),
                   wg*rnorm(1,.22,.04))))))))
      )
    }
    z <- data.frame(z, y, stringsAsFactors = FALSE)
      
    g <- ggplot(z, aes(x = x, y = y, color = w)) + geom_line(size = 2) +
      xlab("Date") + ylab("Boxoffice Gross") + 
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      scale_color_manual(values = c("lightsteelblue1", "mediumpurple3"), name = "") +
      theme(axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12),
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.position = "top")
    p <- ggplotly(g)
    p %>% layout(legend = list(orientation = "h",
                               xanchor = "center", x = 0.5,
                               yanchor = "top", y = 100))
  })
  
  # Raw Data Tab
  output$fmDT <- DT::renderDT(df, options = list(scrollX = TRUE, searching = FALSE))

  output$cwDT <- DT::renderDT({
    wBO() %>% dplyr::group_by(movie) %>%
      dplyr::summarize(distributor = unique(distributor)[1],
                       gross = sum(gross, na.rm = TRUE),
                       percent_change = unique(rev(change))[1],
                       theaters = sum(thtrs.),
                       per_theater = sum(`per thtr.`, na.rm = TRUE),
                       total_gross = sum(`total gross`, na.rm = TRUE),
                       weeks = max(week, na.rm = TRUE)) %>%
      dplyr::filter(!is.na(movie))
  }, options = list(scrollX = TRUE, searching = FALSE))

  output$cmDT <- DT::renderDT({
    mBO() %>% dplyr::group_by(movie) %>%
      dplyr::summarize(distributor = unique(distributor)[1],
                       gross = sum(gross, na.rm = TRUE),
                       percent_change = unique(rev(change))[1],
                       theaters = sum(thtrs.),
                       per_theater = sum(`per thtr.`, na.rm = TRUE),
                       total_gross = sum(`total gross`, na.rm = TRUE),
                       weeks = max(week, na.rm = TRUE)) %>%
      dplyr::filter(!is.na(movie))
  }, options = list(scrollX = TRUE, searching = FALSE))

  output$cqDT <- DT::renderDT({
    qBO() %>% dplyr::group_by(movie) %>%
      dplyr::summarize(distributor = unique(distributor)[1],
                       gross = sum(gross, na.rm = TRUE),
                       percent_change = unique(rev(change))[1],
                       theaters = sum(thtrs.),
                       per_theater = sum(`per thtr.`, na.rm = TRUE),
                       total_gross = sum(`total gross`, na.rm = TRUE),
                       weeks = max(week, na.rm = TRUE)) %>%
      dplyr::filter(!is.na(movie))
  }, options = list(scrollX = TRUE, searching = FALSE))
  
  #----- Screen Optimization -----
  observeEvent(input$infoScheduler, {
    showModal(
      modalDialog(easyClose = TRUE, size = "l",
                  h2("Screen Optimization Information", align = "center"),
                  includeMarkdown("infoScheduler.md")
      )
    )
  })
  
  output$schedFilms_UI <- renderUI({
    mdb <- movieDB[movieDB$startDate <= input$schedDate & movieDB$endDate >= input$schedDate, "film"]
    pickerInput("schedFilms", label = "Film(s) to Schedule:",
                choices = mdb, multiple = TRUE, selected = mdb,
                options = list(`actions-box` = TRUE, `selected-text-format` = "count > 1"))
  })
  
  optimalSched <- reactive({
    if (input$optimize == 0)
      return()
    
    isolate({
      if (length(input$schedFilms) == 0) {
        showModal(
          modalDialog(easyClose = TRUE, footer = modalButton("OK"),
                      h2("Please select at least one film.", align = "center")
          )
        )
      }
      
      validate(
        need(length(input$schedFilms) > 0, "Please select at least one film.")
      )
      
      optimizeShowtimes(input$schedDate, input$firstShow, input$lastShow, input$interval, input$schedFilms, input$screens, input$allShown)
    })
  })
  
  observeEvent(optimalSched(), {
    yn <- length(unique(optimalSched()$content)) == length(input$schedFilms)*input$allShown
    if (input$allShown > 0 & !yn) {
      showModal(
        modalDialog(easyClose = TRUE, footer = modalButton("OK"),
          h2("Attention!", align = "center"),
          h4("Based on the films selected with their associated durations, in addition to the
             available screening window and the possible start times, one or more films
             could not meet the minimum number of occurrences constraint. Please click the
             information button for ways to work around this potential issue. The current
             schedule as shown is optimal minus meeting this constraint.")
        )
      )
    }
    
    output$optSchedule <- renderTimevis({
      timevis(data = optimalSched(), groups = groupsData,
              options = list(
                start = substr(ymd_hms(min(optimalSched()$start)) - minutes(30), 1, 16),
                end = substr(ymd_hms(max(optimalSched()$end)) + hours(1) + minutes(30), 1, 16),
                showCurrentTime = FALSE,
                selectable = FALSE,
                stack = FALSE,
                showTooltips = TRUE,
                tooltip.followMouse = TRUE))
    })
  })
}
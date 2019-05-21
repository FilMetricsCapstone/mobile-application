# This contains the server logic to render the UI
#------------------------------------------------

server <- function(input, output, session) {
  reloadData <- reactiveTimer(216000000, session)
  
  wBO <- reactive({
    reloadData()
    getBoxOffice("w")
  })

  mBO <- reactive({
    reloadData()
    getBoxOffice("m")
  })

  qBO <- reactive({
    reloadData()
    getBoxOffice("q")
  })
  
  
  #----- Dashboard -----
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
            legend.position = c(0.5, 0.1),
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
  # Prediction Tab
  output$predFilms_UI <- renderUI({
    if (input$predQuarter == "Q3") {
      selectInput("predFilms", "Select Film(s) to View", multiple = TRUE,
                  choices = list(
                    "July" = c("Spider-Man: Far From Home", "The Lion King", "Once Upon a Time in Hollywood"),
                    "August" = c("Hobbs and Shaw", "Dora and the Lost City of Gold", "The Angry Birds Movie 2"),
                    "September" = c("IT Chapter Two", "Downton Abbey", "Rambo: Last")))
    } else {
      selectInput("predFilms", "Select Film(s) to View", multiple = TRUE,
                  choices = list(
                    "October" = c("Joker", "Zombieland 2", "Gemini Man", "The Addams Family", "Maleficent: Mistress of Evil"),
                    "November" = c("Terminator: Dark Fate", "Sonic the Hedgehog", "Charlie's Angels", "Frozen 2"),
                    "December" = c("Jumanji Sequel", "Star Wars: The Rise of Skywalker", "Spies in Disguise")))
    }
  })
  
  output$predGross <- renderPlot({
    x <- seq(as.POSIXct(input$predDates[1]), as.POSIXct(input$predDates[2]), by = "hour")
    y <- runif(length(x), 500000, 1000000)
    z <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
    
    ggplot(z, aes(x = x, y = y)) + geom_line() +
      xlab("Date") + ylab("Gross") + 
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      theme(axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12))
  })
  
  output$predLifecycle <- renderPlot({
    x <- 1:100
    y <- c()
    for (i in 1:length(input$predFilms)) {
      y <- cbind(y, runif(100, 500000, 1000000))
    }
    z <- data.frame(x, y, stringsAsFactors = FALSE)
    colnames(z)[2:(ncol(z))] <- input$predFilms
    z <- reshape2::melt(z, id.vars = "x", value.name = "y")
    
    ggplot(z, aes(x = x, y = y, color = variable)) + geom_line() +
      xlab("Time") + ylab("Gross") + 
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      theme(axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12),
            legend.title = element_text(face = "bold", size = 16),
            legend.text = element_text(size = 12))
  })
  
  # Historical Tab
  output$histComparison <- renderPlot({
    if (input$histTime == "Last Week") {
      x <- rep(seq(lubridate::floor_date(Sys.Date(), unit = "w", week_start = 5), Sys.Date()-1, by = "d"), each = 2)
      w <- rep(c("Projected", "Actual"), length(x)/2)
      y <- runif(length(x), 500000, 1000000)
      z <- data.frame(x,w,y,stringsAsFactors = FALSE)
      
      ggplot(z, aes(x = x, y = y, color = w)) + geom_line() +
        xlab("Date") + ylab("Gross") + 
        scale_y_continuous(labels = scales::dollar) +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 16),
              axis.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(size = 12))
    } else if (input$histTime == "Last Month") {
      x <- rep(seq(lubridate::floor_date(Sys.Date(), unit = "m"), Sys.Date()-1, by = "d"), each = 2)
      w <- rep(c("Projected", "Actual"), length(x)/2)
      y <- runif(length(x), 500000, 1000000)
      z <- data.frame(x,w,y,stringsAsFactors = FALSE)
      
      ggplot(z, aes(x = x, y = y, color = w)) + geom_line() +
        xlab("Date") + ylab("Gross") + 
        scale_y_continuous(labels = scales::dollar) +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 16),
              axis.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(size = 12))
    } else {
      x <- rep(seq(lubridate::floor_date(Sys.Date(), unit = "q"), Sys.Date()-1, by = "d"), each = 2)
      w <- rep(c("Projected", "Actual"), length(x)/2)
      y <- runif(length(x), 500000, 1000000)
      z <- data.frame(x,w,y,stringsAsFactors = FALSE)
      
      ggplot(z, aes(x = x, y = y, color = w)) + geom_line() +
        xlab("Date") + ylab("Gross") + 
        scale_y_continuous(labels = scales::dollar) +
        theme_bw() +
        theme(axis.title = element_text(face = "bold", size = 16),
              axis.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(size = 12))
    }
  })
  
  # Raw Data Tab
  output$fmDT <- DT::renderDT(df, options = list(scrollX = TRUE))

  output$cwDT <- DT::renderDT({
    wBO() %>% dplyr::group_by(movie) %>%
      dplyr::summarize(distributor = unique(distributor)[1],
                       gross = sum(gross),
                       percent_change = unique(percent_change)[1],
                       theaters = sum(theaters),
                       per_theater = sum(per_theater),
                       total_gross = sum(total_gross),
                       days = max(days, na.rm = TRUE),
                       date = max(date, na.rm = TRUE)) %>%
      dplyr::filter(!is.na(movie))
  }, options = list(scrollX = TRUE))

  output$cmDT <- DT::renderDT({
    mBO() %>% dplyr::group_by(movie) %>%
      dplyr::summarize(distributor = unique(distributor)[1],
                       gross = sum(gross),
                       percent_change = unique(percent_change)[1],
                       theaters = sum(theaters),
                       per_theater = sum(per_theater),
                       total_gross = sum(total_gross),
                       days = max(days, na.rm = TRUE),
                       date = max(date, na.rm = TRUE)) %>%
      dplyr::filter(!is.na(movie))
  }, options = list(scrollX = TRUE))

  output$cqDT <- DT::renderDT({
    qBO() %>% dplyr::group_by(movie) %>%
      dplyr::summarize(distributor = unique(distributor)[1],
                       gross = sum(gross),
                       percent_change = unique(percent_change)[1],
                       theaters = sum(theaters),
                       per_theater = sum(per_theater),
                       total_gross = sum(total_gross),
                       days = max(days, na.rm = TRUE),
                       date = max(date, na.rm = TRUE)) %>%
      dplyr::filter(!is.na(movie))
  }, options = list(scrollX = TRUE))
  
  #----- Screen Optimization -----
  
  
  #----- Overview (README) ------
  
}
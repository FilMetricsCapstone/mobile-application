# This contains the server logic to render the UI
#------------------------------------------------

server <- function(input, output, session) {
  # values <- reactiveValues(wBO = getBoxOffice("w"),
  #                          mBO = getBoxOffice("m"),
  #                          qBO = getBoxOffice("q"))
  
  
  #----- Dashboard -----
  # Box 1 - Domestic
  
  # Box 1 - International
  
  # Box 1 - Global
  
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
    plotMoviePareto(wBO)
  })
  
  # Box 3 - Month
  output$moviesM <- renderPlot({
    plotMoviePareto(mBO)
  })
  
  # Box 3 - Quarter
  output$moviesQ <- renderPlot({
    plotMoviePareto(qBO)
  })
  
  
  # Top Week
  output$topWeek <- renderValueBox({
    valueBox(
      value = "This Week",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topWeek_T <- renderTable({
    data.frame(
      Rank = 1:5,
      Title = c("Avengers: Endgame", "The Intruder (2019)", "Long Shot", "Uglydolls", "Captain Marvel"),
      Gross = c("$186,551,101", "$14,375,126", "$13,611,935", "$10,360,796", "$5,472,061")
    )
  }, width = "100%")
  
  # Top Month
  output$topMonth <- renderValueBox({
    valueBox(
      value = "This Month",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topMonth_T <- renderTable({
    data.frame(
      Rank = 1:5,
      Title = c("Avengers: Endgame", "Shazam!", "Pet Sematary (2019)", "The Curse of La Llorona", "Little"),
      Gross = c("$728,447,735", "$137,175,154", "$54,269,143", "$51,623,137", "$39,759,470")
    )
  }, width = "100%")
  
  # Top Quarter
  output$topQuarter <- renderValueBox({
    valueBox(
      value = "This Quarter",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topQuarter_T <- renderTable({
    data.frame(
      Rank = 1:5,
      Title = c("Avengers: Endgame", "Shazam!", "Pokemon Detective Pikachu", "Pet Sematary (2019)", "The Curse of La Llorona"),
      Gross = c("$728,447,735", "$137,175,154", "$58,058,457", "$54,269,143", "$51,623,137")
    )
  }, width = "100%")
  
  #----- Analytics -----
  
  
  #----- Screen Optimization -----
  
  
  #----- Overview (README) ------
  
}
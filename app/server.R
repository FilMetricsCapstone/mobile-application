# This contains the server logic to render the UI
#------------------------------------------------

server <- function(input, output, session) {
  
  #----- Dashboard -----
  #-- Mobile App View --
  # Top Week
  output$topWeek_1 <- renderValueBox({
    valueBox(
      value = "This Week",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topWeek_T1 <- renderTable({
    data.frame(
      Rank = 1:5,
      Title = c("Avengers: Endgame", "The Intruder (2019)", "Long Shot", "Uglydolls", "Captain Marvel"),
      Gross = c("$186,551,101", "$14,375,126", "$13,611,935", "$10,360,796", "$5,472,061")
    )
  }, width = "100%")
  
  # Top Month
  output$topMonth_1 <- renderValueBox({
    valueBox(
      value = "This Month",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topMonth_T1 <- renderTable({
    data.frame(
      Rank = 1:5,
      Title = c("Avengers: Endgame", "Shazam!", "Pet Sematary (2019)", "The Curse of La Llorona", "Little"),
      Gross = c("$728,447,735", "$137,175,154", "$54,269,143", "$51,623,137", "$39,759,470")
    )
  }, width = "100%")
  
  # Top Quarter
  output$topQuarter_1 <- renderValueBox({
    valueBox(
      value = "This Quarter",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topQuarter_T1 <- renderTable({
    data.frame(
      Rank = 1:5,
      Title = c("Avengers: Endgame", "Shazam!", "Pokemon Detective Pikachu", "Pet Sematary (2019)", "The Curse of La Llorona"),
      Gross = c("$728,447,735", "$137,175,154", "$58,058,457", "$54,269,143", "$51,623,137")
    )
  }, width = "100%")
  
  #-- Desktop View --
  # Top Week
  output$topWeek_2 <- renderValueBox({
    valueBox(
      value = "This Week",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topWeek_T2 <- renderTable({
    data.frame(
      Rank = 1:5,
      Title = c("Avengers: Endgame", "The Intruder (2019)", "Long Shot", "Uglydolls", "Captain Marvel"),
      Gross = c("$186,551,101", "$14,375,126", "$13,611,935", "$10,360,796", "$5,472,061")
    )
  }, width = "100%")
  
  # Top Month
  output$topMonth_2 <- renderValueBox({
    valueBox(
      value = "This Month",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topMonth_T2 <- renderTable({
    data.frame(
      Rank = 1:5,
      Title = c("Avengers: Endgame", "Shazam!", "Pet Sematary (2019)", "The Curse of La Llorona", "Little"),
      Gross = c("$728,447,735", "$137,175,154", "$54,269,143", "$51,623,137", "$39,759,470")
    )
  }, width = "100%")
  
  # Top Quarter
  output$topQuarter_2 <- renderValueBox({
    valueBox(
      value = "This Quarter",
      subtitle = "Projected Earnings",
      icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$topQuarter_T2 <- renderTable({
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
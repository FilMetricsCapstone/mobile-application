# This contains the server logic to render the UI
#------------------------------------------------

server <- function(input, output, session) {
  
  #----- Dashboard -----
  output$yearTopFive_1 <- renderImage({
    list(
      src = normalizePath(file.path("./www/2019_top_five.JPG"))
    )
  }, deleteFile = FALSE)
  
  output$quarterTopFive_1 <- renderImage({
    list(
      src = normalizePath(file.path("./www/2019_q2.JPG"))
    )
  }, deleteFile = FALSE)
  
  output$yearTopFive_2 <- renderImage({
    list(
      src = normalizePath(file.path("./www/2019_top_five.JPG"))
    )
  }, deleteFile = FALSE)
  
  output$quarterTopFive_2 <- renderImage({
    list(
      src = normalizePath(file.path("./www/2019_q2.JPG"))
    )
  }, deleteFile = FALSE)
  
  #----- Analytics -----
  output$donut_1 <- renderImage({
    list(
      src = normalizePath(file.path("./www/donut.png"))
    )
  }, deleteFile = FALSE)
  
  output$bar_1 <- renderImage({
    list(
      src = normalizePath(file.path("./www/bar.png"))
    )
  }, deleteFile = FALSE)
  
  output$donut_2 <- renderImage({
    list(
      src = normalizePath(file.path("./www/donut.png"))
    )
  }, deleteFile = FALSE)
  
  output$bar_2 <- renderImage({
    list(
      src = normalizePath(file.path("./www/bar.png"))
    )
  }, deleteFile = FALSE)
  
  #----- Screen Optimization -----
  output$gantt_screen_schedule_1 <- renderImage({
    list(
      src = normalizePath(file.path("./www/gantt_screen_schedule.JPG"))
    )
  }, deleteFile = FALSE)
  
  output$gantt_screen_schedule_2 <- renderImage({
    list(
      src = normalizePath(file.path("./www/gantt_screen_schedule.JPG"))
    )
  }, deleteFile = FALSE)
  
  #----- Overview (README) ------
  
}
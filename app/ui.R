# This contains the user interface definitions
#---------------------------------------------

dashboardPage(title = "Glen Art Theater Analytics", skin = "purple", # Specifies text to be displayed in browser tab
  
  #----- Title (displays above sidebar menu) -----
  dashboardHeader(title = "Glen Art Theater Analytics", titleWidth = 275),
  
  #----- Sidebar menu items to navigate through the app -----
  dashboardSidebar(collapsed = TRUE,
    sidebarMenu(id = "glenArt", # This name allows button clicks to move throughout the app
      # Dashboard          
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), # Icons from https://fontawesome.com/icons?d=gallery
      
      # Analytics
      menuItem("Analytics", tabName = "analytics", icon = icon("line-chart")),
      
      # Screen Optimization
      menuItem("Scheduler", tabName = "scheduler", icon = icon("clock")),
      
      # About Us
      menuItem("About Us", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Logo
    HTML('<footer><img src="logo.jpg" width="95%"</img> </footer>')
  ),
  
  #----- Main panel display depending on which sidebar menu item is selected -----
  dashboardBody(
    tags$head(tags$link(href = "style.css", rel = "stylesheet"),
              tags$style(HTML('#infoDashboard{background-color:#CAE1FF}')),
              tags$style(HTML('#infoAnalytics{background-color:#CAE1FF}')),
              tags$style(HTML('#infoScheduler{background-color:#CAE1FF}'))),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$style(type = 'text/css', "footer{position: absolute; bottom:5%; left: 5%; padding:5px;}"),
    tags$style(HTML(".box.box-primary>.box-header {
                      background: #fff
                    }
                    .box.box-primary{
                      border-bottom-color:#8968CD;
                      border-left-color:#8968CD;
                      border-right-color:#8968CD;
                      border-top-color:#8968CD;
                    }
                    .box.box-info>.box-header {
                      background: #fff
                    }
                    .box.box-info{
                      border-bottom-color:#CAE1FF;
                      border-left-color:#CAE1FF;
                      border-right-color:#CAE1FF;
                      border-top-color:#CAE1FF;
                    }
                    .nav-tabs-custom .nav-tabs li.active {
                      background: #8968CD
                    }
                    .nav-tabs-custom .nav-tabs li.active{
                      border-bottom-color:#8968CD;
                      border-left-color:#8968CD;
                      border-right-color:#8968CD;
                      border-top-color:#8968CD;
                    }
                    .shiny-notification {
                      position:fixed;
                      top: calc(50%);;
                      left: calc(50%);;
                    }
                    .shiny-notification .progress-bar {
                      background: #8968CD;
                    }")),
    tabItems(
      
      #----- Dashboard -----
      # This page...
      tabItem(tabName = "dashboard",
              div(actionButton("infoDashboard", label = "", icon = icon("info-circle")), align = "right"),
              div(helpText(HTML(paste0("For assistance contact ",
                                       a("customersupport@filmetrics.com", href = "mailto:customersupport@filmetrics.com")))),
                  align = "right"),
              fluidRow(
                column(width = 6,
                  box(div(titlePanel("% Annual Sales Target to Date"), align = "center"),
                      plotOutput("donut"), width = NULL, solidHeader = FALSE, status = "primary")
                ),
                column(width = 6,
                  tabBox(width = NULL,
                    tabPanel("Domestic", plotOutput("compD")),
                    tabPanel("International", plotOutput("compI")),
                    tabPanel("Global", plotOutput("compG"))
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                  tabBox(width = NULL,
                    tabPanel("Week", plotOutput("moviesW")),
                    tabPanel("Month", plotOutput("moviesM")),
                    tabPanel("Quarter", plotOutput("moviesQ"))
                  )
                )
              )
      ),
      
      #----- Analytics -----
      # This page...
      tabItem(tabName = "analytics",
        div(actionButton("infoAnalytics", label = "", icon = icon("info-circle")), align = "right"),
        div(helpText(HTML(paste0("For assistance contact ",
                                 a("customersupport@filmetrics.com", href = "mailto:customersupport@filmetrics.com")))),
            align = "right"),
        tabsetPanel(
          tabPanel(title = "Predictive",
            br(),
            fluidRow(
              column(width = 3,
                box(title = "Inputs", collapsible = TRUE, width = NULL, solidHeader = FALSE, status = "info",
                    selectInput("predMetric", "Prediction Method:",
                                choices = c("Predicted Gross Revenue", "Film Lifecycle Earnings")),
                    radioGroupButtons("predTime", "Time Period:", justified = TRUE,
                                      choices = c("Q3","Q4","Date Range"), selected = "Q3"),
                    conditionalPanel(
                      condition = "input.predTime == 'Date Range'",
                      dateRangeInput("predDates", "Date Range:", start = "2019-07-01",
                                     end = as.character(ymd("2019-07-01")+6), min = "2019-07-01")
                    ),
                    uiOutput("predFilms_UI"),
                    actionButton("update", "Update Plots")
                )
              ),
              column(width = 9,
                conditionalPanel(
                  condition = "input.predMetric == 'Predicted Gross Revenue'",
                  box(div(titlePanel("Glen Art Theater Forecasted Performance"), align = "center"),
                      collapsible = TRUE, width = NULL, solidHeader = FALSE, status = "primary",
                      plotlyOutput("predGross")
                  )
                ),
                conditionalPanel(
                  condition = "input.predMetric == 'Film Lifecycle Earnings'",
                  box(div(titlePanel("Forecasted Film Lifecycle Earnings"), align = "center"),
                      collapsible = TRUE, width = NULL, solidHeader = FALSE, status = "primary",
                      plotlyOutput("predLifecycle")
                  )
                )
              )
            )
          ),
          tabPanel(title = "Historical",
            br(),
            fluidRow(
              column(width = 3,
                box(title = "Input", collapsible = TRUE, width = NULL, solidHeader = FALSE, status = "info",
                    selectInput("histTime", "Time Period:", choices = c("Last Week", "Last Month", "Last Quarter"))
                )
              ),
              column(width = 9,
                box(div(titlePanel("Glen Art Theater Historical Performance"), align = "center"),
                    collapsible = TRUE, width = NULL, solidHeader = FALSE, status = "primary",
                    plotlyOutput("histComparison")
                )
              )
            )
          ),
          tabPanel(title = "Raw Data",
            br(),
            fluidRow(
              column(width = 3,
                box(title = "Inputs", collapsible = TRUE, width = NULL, solidHeader = FALSE, status = "info",
                    selectInput("dataset", "Data Set to View:", list(
                      "FilMetrics" = c("FilMetrics Data"),
                      "The-Numbers.com" = c("Current Week", "Current Month", "Current Quarter")
                    ))
                )
              ),
              column(width = 9,
                box(title = "Data Table", collapsible = TRUE, width = NULL, solidHeader = FALSE, status = "primary",
                    conditionalPanel(
                      condition = "input.dataset == 'FilMetrics Data'",
                      DT::DTOutput("fmDT"),
                      helpText(HTML(paste0("These data were obtained from a number of open
                                           sources including ",
                                           a("imdb.com", target = "_blank", href = "https://www.imdb.com/"), ", ",
                                           a("themoviedb.org", target = "_blank", href = "https://www.themoviedb.org/"), ", and ",
                                           a("movielens.org", target = "_blank", href = "https://movielens.org/"),
                                           ".")))
                    ),
                    conditionalPanel(
                      condition = "input.dataset == 'Current Week'",
                      DT::DTOutput("cwDT"),
                      helpText(HTML(paste0("These data were obtained from ",
                                           a("the-numbers.com", target = "_blank", href = "https://www.the-numbers.com/"),
                                           ".")))
                    ),
                    conditionalPanel(
                      condition = "input.dataset == 'Current Month'",
                      DT::DTOutput("cmDT"),
                      helpText(HTML(paste0("These data were obtained from ",
                                           a("the-numbers.com", target = "_blank", href = "https://www.the-numbers.com/"),
                                           ".")))
                    ),
                    conditionalPanel(
                      condition = "input.dataset == 'Current Quarter'",
                      DT::DTOutput("cqDT"),
                      helpText(HTML(paste0("These data were obtained from ",
                                           a("the-numbers.com", target = "_blank", href = "https://www.the-numbers.com/"),
                                           ".")))
                    )
                  )
                )
              )
            )
          )
      ),
      
      #----- Screen Optimization -----
      # This page...
      tabItem(tabName = "scheduler",
        div(actionButton("infoScheduler", label = "", icon = icon("info-circle")), align = "right"),
        div(helpText(HTML(paste0("For assistance contact ",
                                 a("customersupport@filmetrics.com", href = "mailto:customersupport@filmetrics.com")))),
            align = "right"),
        fluidRow(
          column(width = 3,
            box(title = "Theater Constraints", collapsible = TRUE, width = NULL, solidHeader = FALSE, status = "info",
                dateInput("schedDate", "Date to Schedule:", value = Sys.Date(),
                          min = Sys.Date(), max = max(movieDB$endDate)),
                timeInput("firstShow", "Earliest Start Time:", value = strptime("11:00:00", "%T"),
                          seconds = FALSE),
                timeInput("lastShow", "Latest Finish Time:", value = strptime("00:30:00", "%T"),
                          seconds = FALSE),
                numericInput("interval", "Interval Between Start Times (min):", value = 5,
                             min = 1, max = 60, step = 1),
                numericInput("screens", "Available Screens:", value = 4, min = 1, max = 4, step = 1)
            )
          ),
          column(width = 9,
            fluidRow(
              box(title = "Film Inputs", collapsible = TRUE, width = 4, solidHeader = FALSE, status = "info",
                uiOutput("schedFilms_UI"),
                numericInput("allShown", "Minimum # Showings per Film:", value = 1, min = 0, max = 2),
                actionButton("optimize", "Schedule", icon = icon("clock"))
              )
            ),
            fluidRow(
              box(div(titlePanel("Optimal Schedule"), align = "center"),
                  collapsible = TRUE, width = 12, solidHeader = FALSE, status = "primary",
                  timevisOutput("optSchedule")
              )
            )
          )
        )        
      ),
      
      #----- Overview (README) -----
      tabItem(tabName = "about",
        includeMarkdown("aboutUs.md")      
      )
    )
  )
)

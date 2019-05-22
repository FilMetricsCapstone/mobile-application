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
      
      # Overview (README)
      menuItem("Overview", tabName = "overview", icon = icon("info-circle"))
    ),
    
    # Logo
    p(img(src = "logo.jpg", width = "95%"), align = "center")
  ),
  
  #----- Main panel display depending on which sidebar menu item is selected -----
  dashboardBody(tags$script(HTML("$('body').addClass('fixed');")),
    tabItems(
      
      #----- Dashboard -----
      # This page...
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 6,
                  box(title = "% Annual Target to Date", plotOutput("donut"), width = NULL)
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
        tabsetPanel(
          tabPanel(title = "Predictive",
            fluidRow(
              column(width = 3,
                box(title = "Inputs", collapsible = TRUE, width = NULL,
                    selectInput("predMetric", "Forecast Method:",
                                choices = c("Predicted Gross", "Film Lifecycle")),
                    conditionalPanel(
                      condition = "input.predMetric == 'Predicted Gross'",
                      dateRangeInput("predDates", "Forecast Date:", start = as.character(Sys.Date()+1),
                                     end = as.character(Sys.Date()+7), min = as.character(Sys.Date()+1), weekstart = 5)
                    ),
                    conditionalPanel(
                      condition = "input.predMetric == 'Film Lifecycle'",
                      radioButtons("predQuarter", "Quarter:", c("Q3", "Q4")),
                      uiOutput("predFilms_UI")
                    )
                )
              ),
              column(width = 9,
                box(title = "Predictive Visuals", collapsible = TRUE, width = NULL,
                    conditionalPanel(
                      condition = "input.predMetric == 'Predicted Gross'",
                      plotOutput("predGross")
                    ),
                    conditionalPanel(
                      condition = "input.predMetric == 'Film Lifecycle'",
                      plotOutput("predLifecycle")
                    )
                )
              )
            )
          ),
          tabPanel(title = "Historical",
            fluidRow(
              column(width = 3,
                box(title = "Inputs", collapsible = TRUE, width = NULL,
                    selectInput("histTime", "Time Period:", choices = c("Last Week", "Last Month", "Last Quarter"))
                )
              ),
              column(width = 9,
                box(title = "Historical Visual", collapsible = TRUE, width = NULL,
                    plotOutput("histComparison")
                )
              )
            )
          ),
          tabPanel(title = "Raw Data",
            fluidRow(
              column(width = 3,
                box(title = "Inputs", collapsible = TRUE, width = NULL,
                    selectInput("dataset", "Data Set to View:", list(
                      "FilMetrics" = c("FilMetrics Data"),
                      "The-Numbers.com" = c("Current Week", "Current Month", "Current Quarter")
                    ))
                )
              ),
              column(width = 9,
                box(title = "Data Table", collapsible = TRUE, width = NULL,
                    conditionalPanel(
                      condition = "input.dataset == 'FilMetrics Data'",
                      DT::DTOutput("fmDT")
                    ),
                    conditionalPanel(
                      condition = "input.dataset == 'Current Week'",
                      DT::DTOutput("cwDT")
                    ),
                    conditionalPanel(
                      condition = "input.dataset == 'Current Month'",
                      DT::DTOutput("cmDT")
                    ),
                    conditionalPanel(
                      condition = "input.dataset == 'Current Quarter'",
                      DT::DTOutput("cqDT")
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
        fluidRow(
          column(width = 3,
            box(title = "Inputs", collapsible = TRUE, width = NULL,
                dateInput("schedDate", "Date to Schedule:", value = Sys.Date(),
                          min = Sys.Date(), max = max(movieDB$endDate)),
                tableOutput("availableFilms_UI"),
                timeInput("firstShow", "Earliest Start Time:", value = strptime("11:00:00", "%T"),
                          seconds = FALSE),
                timeInput("lastShow", "Latest Finish Time:", value = strptime("00:30:00", "%T"),
                          seconds = FALSE),
                numericInput("interval", "Interval Between Start Times (min)", value = 5,
                             min = 1, max = 60, step = 1),
                numericInput("screens", "Available Screens", value = 4, min = 1, max = 4, step = 1),
                numericInput("allShown", "Minimum # Showings per Film", value = 1, min = 0, max = 2)
            )
          ),
          column(width = 9,
            actionButton("optimize", "Schedule", icon = icon("clock")),
            br(),br(),
            box(title = "Optimal Schedule", collapsible = TRUE, width = NULL,
                timevisOutput("optSchedule")
            )
          )
        )        
      ),
      
      #----- Overview (README) -----
      tabItem(tabName = "overview",
              h1("User's Guide Stuff Here", align = "center"),
              div(
                div(h5("Created by Omer Ahmad, Nick Betzsold, Jeff Grobart, Muthukumar Palani, and Suresh Sabramanian"),
                    style = "position: absolute; bottom: 5px"),
                align = "center"
              )
      )
    )
  )
)

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
                  tabBox(width = NULL,
                    tabPanel("Domestic", "Barplot US vs Glen Art"), #plotOutput("compWMQ", width = NULL)
                    tabPanel("International", "Barplot International vs Glen Art"),
                    tabPanel("Global", "Barplot Global vs Glen Art")
                  )
                ),
                column(width = 6,
                  box(title = "% Annual Target to Date", plotOutput("donut"), width = NULL)
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
      tabItem(tabName = "analytics"
      ),
      
      #----- Screen Optimization -----
      # This page...
      tabItem(tabName = "scheduler"
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

# This contains the user interface definitions
#---------------------------------------------

dashboardPage(title = "Glen Art Theater Analytics", skin = "purple", # Specifies text to be displayed in browser tab
  
  #----- Title (displays above sidebar menu) -----
  dashboardHeader(title = "Glen Art Theater Analytics", titleWidth = 275),
  
  #----- Sidebar menu items to navigate through the app -----
  dashboardSidebar(collapsed = TRUE,
    sidebarMenu(id = "glenArt", # This name allows button clicks to move throughout the app
      radioGroupButtons("dashboardLayout", label = "", choices = c("Mobile","Desktop"), selected = "Mobile",
                        justified = TRUE),
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
              conditionalPanel(
                condition = "input.dashboardLayout == 'Mobile'",
                valueBoxOutput("topWeek_1", width = "100%"),
                tableOutput("topWeek_T1"),
                valueBoxOutput("topMonth_1", width = "100%"),
                tableOutput("topMonth_T1"),
                valueBoxOutput("topQuarter_1", width = "100%"),
                tableOutput("topQuarter_T1")
              ),
              conditionalPanel(
                condition = "input.dashboardLayout == 'Desktop'",
                fluidRow(
                  column(width = 4,
                         valueBoxOutput("topWeek_2", width = NULL),
                         tableOutput("topWeek_T2")),
                  column(width = 4,
                         valueBoxOutput("topMonth_2", width = NULL),
                         tableOutput("topMonth_T2")),
                  column(width = 4,
                         valueBoxOutput("topQuarter_2", width = NULL),
                         tableOutput("topQuarter_T2"))
                )
              )
      ),
      
      #----- Analytics -----
      # This page...
      tabItem(tabName = "analytics"#,
        # conditionalPanel(
        #   condition = "input.dashboardLayout == 'Mobile'",
        #   imageOutput("donut_1", width = "100%"),
        #   imageOutput("bar_1", width = "100%")
        # ),
        # conditionalPanel(
        #   condition = "input.dashboardLayout == 'Desktop'",
        #   fluidRow(
        #     column(imageOutput("donut_2", width = "100%"), width = 6),
        #     column(imageOutput("bar_2", width = "100%"), width = 6)
        #   )
        # )
      ),
      
      #----- Screen Optimization -----
      # This page...
      tabItem(tabName = "scheduler"#,
        # conditionalPanel(
        #   condition = "input.dashboardLayout == 'Mobile'",
        #   imageOutput("gantt_screen_schedule_1", width = "100%")
        # ),
        # conditionalPanel(
        #   condition = "input.dashboardLayout == 'Desktop'",
        #   fluidRow(
        #     column(imageOutput("gantt_screen_schedule_2", width = "100%"), width = 12)
        #   )
        # )
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

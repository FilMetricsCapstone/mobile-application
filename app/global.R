# This contains the necessary libraries and global variables/functions
#---------------------------------------------------------------------

#----- Needed Libraries -----
library(ggplot2)
library(shiny)
library(shinydashboard)
library(timevis)

#----- Global Variables -----
# Final Model Data Set
df <- read.csv("data/Final_Model_data.csv"); df <- df[,-c(1,3,4,12)]
df <- df[!(df$budget < 1000 | df$revenue < 1000),]

# Ticket Prices
tix_mat <- 5
tix_reg <- 7

#----- Global Functions -----
getBoxOffice <- function(type) {
  # Grab dates
  if (type == "w") {
    lu <- seq(lubridate::floor_date(Sys.Date(), unit = "w", week_start = 5), Sys.Date()-1, by = "d")
  } else if (type == "m") {
    lu <- seq(lubridate::floor_date(Sys.Date(), unit = "m"), Sys.Date()-1, by = "d")
  } else {
    lu <- seq(lubridate::floor_date(Sys.Date(), unit = "q"), Sys.Date()-1, by = "d")
  }
  
  # Get box office data
  out <- boxoffice::boxoffice(lu, "numbers")
  return(out)
}

wBO <- getBoxOffice("w")
mBO <- getBoxOffice("m")
qBO <- getBoxOffice("q")

plotMoviePareto <- function(dat) {
  # Manipulate data
  dat1 <- dat %>% dplyr::group_by(movie) %>%
    dplyr::summarize(gross = sum(gross)) %>%
    dplyr::filter(!is.na(gross)) %>% 
    dplyr::arrange(dplyr::desc(gross)) %>% 
    dplyr::mutate(cp = cumsum(gross)/sum(gross), movie = factor(movie, levels = movie)) %>% 
    dplyr::filter(signif(cp, 1) <= 0.8)
  
  # Plot
  g <- ggplot(dat1, aes(x = movie, y = gross)) +
    geom_bar(stat = "identity", fill = "mediumpurple3") +
    xlab("Film") + ylab("Gross") +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
    scale_y_continuous(labels = scales::dollar) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold", size = 16),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  return(g)
}
# This contains the necessary libraries and global variables/functions
#---------------------------------------------------------------------

#----- Needed Libraries -----
library(boxoffice)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(timevis)

#----- Global Variables -----
# Final Model Data Set
df <- read.csv("data/Final_Model_data.csv"); df <- df[,-1]
# df <- df[,-c(1,3,4,12)]
# df <- df[!(df$budget < 1000 | df$revenue < 1000),]

# # Ticket Prices
# tix_mat <- 5
# tix_reg <- 7

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

# wBO <- getBoxOffice("w")
# mBO <- getBoxOffice("m")
# qBO <- getBoxOffice("q")

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

plotCompBarplot <- function(w, m, q, x) {
  wBO1 <- sum(w$gross[!is.na(w$gross)])
  mBO1 <- sum(m$gross[!is.na(m$gross)])
  qBO1 <- sum(q$gross[!is.na(q$gross)])
  dat <- data.frame(stringsAsFactors = FALSE,
                    time = rep(c("Week", "Month", "Quarter"), each = 6),
                    type = rep(rep(c("domestic", "international", "global"), each = 2), 3),
                    group = rep(c("Average", "Glen Art"), 9),
                    gross = c(wBO1*runif(1,0.7,0.75), NA, mBO1*runif(1,0.6,0.65), NA, qBO1*runif(1,0.8,0.85), NA,
                              wBO1*runif(1,0.7,0.75), NA, mBO1*runif(1,0.6,0.65), NA, qBO1*runif(1,0.8,0.85), NA,
                              wBO1*runif(1,0.7,0.75), NA, mBO1*runif(1,0.6,0.65), NA, qBO1*runif(1,0.8,0.85), NA))
  dat[dat$type == "domestic" & is.na(dat$gross), "gross"] <- dat[dat$type == "domestic" & !is.na(dat$gross), "gross"]*runif(1,0.8,1.1)
  dat[dat$type == "global" & is.na(dat$gross), "gross"] <- dat[dat$type == "global" & !is.na(dat$gross), "gross"]*runif(1,0.8,1.1)
  dat[dat$type == "international" & is.na(dat$gross), "gross"] <- dat[dat$type == "global" & dat$group == "Glen Art", "gross"] - dat[dat$type == "domestic" & dat$group == "Glen Art", "gross"]
  dat$time <- factor(dat$time, levels = c("Week", "Month", "Quarter"))
  
  # Plot
  if (x == "domestic") {
    datd <- dat[dat$type == "domestic",]
    g <- ggplot(datd, aes(x = time, y = gross)) +
      geom_bar(aes(fill = group), stat = "identity", position = position_dodge2()) +
      xlab("") + ylab("Gross") +
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      scale_fill_manual(values = c("mediumpurple3", "lightsteelblue1"), name = "") +
      theme(axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))
  } else if (x == "international") {
    dati <- dat[dat$type == "international",]
    g <- ggplot(dati, aes(x = time, y = gross)) +
      geom_bar(aes(fill = group), stat = "identity", position = position_dodge2()) +
      xlab("") + ylab("Gross") +
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      scale_fill_manual(values = c("mediumpurple3", "lightsteelblue1"), name = "") +
      theme(axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))
  } else {
    datg <- dat[dat$type == "global",]
    g <- ggplot(datg, aes(x = time, y = gross)) +
      geom_bar(aes(fill = group), stat = "identity", position = position_dodge2()) +
      xlab("") + ylab("Gross") +
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      scale_fill_manual(values = c("mediumpurple3", "lightsteelblue1"), name = "") +
      theme(axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))
  }
  return(g)
}
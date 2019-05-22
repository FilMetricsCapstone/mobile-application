# This contains the necessary libraries and global variables/functions
#---------------------------------------------------------------------

#----- Needed Libraries -----
library(boxoffice)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyTime)
library(timevis)

#----- Global Variables -----
movieDB <- read.csv("data/movieDB.csv", stringsAsFactors = FALSE)

# Final Model Data Set
df <- read.csv("data/Final_Model_data.csv"); df <- df[,-1]
# df <- df[,-c(1,3,4,12)]
# df <- df[!(df$budget < 1000 | df$revenue < 1000),]

# # Ticket Prices
# tix_mat <- 5
# tix_reg <- 7

# Group data for timevis output
groupsData <- data.frame(id = 1:4,
                         content = c("Screen 1", "Screen 2", "Screen 3", "Screen 4"))

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

optimizeShowtimes <- function(showDate = "2019-05-21", firstShow = "11:00", lastShow = "00:30", interval = 5, screens = 4, allShown = 1) {
  # Format date-times
  firstShow <- paste(showDate, substr(strftime(firstShow, "%T"), 1, 5))
  lastShow <- substr(strftime(lastShow, "%T"), 1, 5)
  if (substr(lastShow,1,1) == 0) {
    lastShow <- paste(ymd(showDate) + 1, lastShow)
  } else {
    lastShow <- paste(showDate, lastShow)
  }
  
  # Get screening window times
  times <- seq.POSIXt(as.POSIXct(firstShow, tz="GMT"),
                      as.POSIXct(lastShow, tz="GMT"), by = paste(interval, "min"))
  screeningWindow <- 1:length(times); names(screeningWindow) <- substr(times, 1, 19); rm(times)
  
  # Subset movieDB
  films <- movieDB[movieDB$startDate < showDate & movieDB$endDate > showDate,]
  
  # Movie durations
  durations <- ceiling((films$runtime + films$addition)/interval)
  
  # Possible start and end times for each movie
  showtimes <- c()
  for (i in 1:length(durations)) {
    s <- 1:(length(screeningWindow) - durations[i] + 1)
    f <- s + durations[i] - 1
    x <- cbind(s,f)
    rownames(x) <- rep(films$film[i], nrow(x))
    showtimes <- rbind(showtimes, x)
  }
  rm(f, i, s, x)
  
  # Add demand for movies at each start time (NEEDS TO GET REPLACED BY FORECASTING PIECE)
  d <- sample(1:100, nrow(showtimes), replace = TRUE)
  showtimes <- cbind(showtimes, d); rm(d)
  
  # Sort by finish time
  showtimes <- showtimes[order(showtimes[,"f"]),]
  
  # Calculate showtimes with maximum demand
  runWIS <- function(shows) {
    # Initialize maximum demand vector
    d <- unname(shows[,"d"])
    
    # Initialize list of movie start times to get demands in d
    ind <- as.list(1:nrow(shows))
    
    # Calculate maximum demand
    for (i in 2:nrow(shows)) {
      d_i <- d[i]
      ind_j <- c()
      for (j in 1:(i-1)) {
        if (shows[j,"f"] <= shows[i,"s"]) {
          if(max(d[i], d[j] + shows[i,"d"]) > d_i) {
            d_i <- max(d[i], d[j] + shows[i,"d"])
            ind_j <- ind[[j]]
          }
        }
      }
      d[i] <- d_i
      ind[[i]] <- sort(c(ind[[i]], ind_j))
    }
    rm(d_i, i, ind_j, j)
    
    out <- ind[[which.max(d)]]
    return(out)
  }
  
  optSchedule <- c()
  for (i in 1:screens) {
    ind <- runWIS(showtimes)
    temp <- showtimes[ind,]
    temp <- data.frame(content = rownames(temp),
                       start = temp[,"s"],
                       end = temp[,"f"],
                       d = temp[,"d"],
                       group = i, stringsAsFactors = FALSE)
    optSchedule <- data.frame(rbind(optSchedule, temp), stringsAsFactors = FALSE)
    showtimes <- showtimes[-ind,]
  }
  rm(i, ind, temp)
  optSchedule <- data.frame(id = 1:nrow(optSchedule), optSchedule, stringsAsFactors = FALSE)
  
  # Check number of occurrences of each film
  occurrences <- sapply(films$film, function(x) sum(x == optSchedule$content))
  ind1 <- which(occurrences < allShown); ind2 <- which(occurrences <= allShown)
  
  # Number in each group
  N <- c(0, unname(cumsum(table(optSchedule$group))))
  
  # Temporary copy of optSchedule
  optSchedule1 <- list()
  for (i in 1:(length(N)-1)) {
    optSchedule1[[i]] <- optSchedule[(N[i]+1):N[i+1],]
  }
  
  getIndSmallD <- function(df, n) {
    df$d[df$content %in% n] <- NA
    which.min(df$d)
  }
  
  # Adjust schedule to meet number of shows constraint
  ind_n <- c()
  while (length(ind1) > 0) {
    for (i in 1:length(ind1)) {
      indSmallD <- sapply(optSchedule1, function(x) getIndSmallD(x, names(ind2)))
      smallD <- sapply(optSchedule1, function(x) min(x$d[!(x$content %in% names(ind2))]))
      st <- showtimes[rownames(showtimes)==names(ind1)[i],]
      st <- data.frame(film = rownames(st),
                       s = st[,"s"],
                       f = st[,"f"],
                       d = st[,"d"],
                       stringsAsFactors = FALSE)
      
      f <- c(); s <- c()
      for (j in 1:length(optSchedule1)) {
        s <- c(s, ifelse(indSmallD[j]==1, 1, optSchedule1[[j]]$end[indSmallD[j]-1]))
        f <- c(f, ifelse(indSmallD[j]==nrow(optSchedule1[[j]]), length(screeningWindow), optSchedule1[[j]]$start[indSmallD[j]+1]))
      }
      z <- data.frame(cbind(s,f))
      d <- c(); ind <- c()
      for (k in 1:nrow(z)) {
        ind <- c(ind, ifelse(length(st[st$s >= z$s[k] & st$f <= z$f[k],"d"]) == 0, NA, which.max(st[st$s >= z$s[k] & st$f <= z$f[k],"d"])))
        d <- c(d, ifelse(length(st[st$s >= z$s[k] & st$f <= z$f[k],"d"]) == 0, NA, max(st[st$s >= z$s[k] & st$f <= z$f[k],"d"])))
      }
      if (all(is.na(ind))) {
        ind_n <- c(ind_n, names(ind1)[i])
      } else {
        new <- st[st$s >= z$s[which.min(smallD-d)] & st$f <= z$f[which.min(smallD-d)],][ind[which.min(smallD-d)],]
        optSchedule1[[which.min(smallD-d)]][indSmallD[which.min(smallD-d)],2:5] <- new
      }
      
      # Check number of occurrences of each film
      occurrences <- rowSums(sapply(optSchedule1, function(x) sapply(films$film, function(y) sum(y == x$content))))
      ind2 <- which(occurrences <= allShown)
    }
    ind1 <- which(occurrences < allShown); ind1 <- ind1[!(names(ind1) %in% ind_n)]
  }
  rm(d, f, i, ind, ind_n, ind1, ind2, indSmallD, j, k, N, occurrences, s, smallD, st, z)
  temp <- c()
  for (l in 1:length(optSchedule1)) {
    temp <- rbind(temp, optSchedule1[[l]])
  }
  optSchedule1 <- temp; rm(temp)
  optSchedule <- optSchedule1; rm(optSchedule1)
  
  optSchedule$start <- names(screeningWindow)[optSchedule$start]
  optSchedule$end <- names(screeningWindow)[optSchedule$end]
  
  return(optSchedule)
}

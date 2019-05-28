# This contains the necessary libraries and global variables/functions
#---------------------------------------------------------------------

#----- Needed Libraries -----
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(rvest)
library(shiny)
library(shinydashboard)
library(shinyTime)
library(shinyWidgets)
library(timevis)

#----- Global Variables -----
# Movie database containing start/end dates, runtimes, additional times, & projected popularity
movieDB <- read.csv("data/movieDB.csv", stringsAsFactors = FALSE)

# Estimated mean and std. deviation for popularity categories
load("data/projRev.Rdata")

# Final Model Data Set
df <- read.csv("data/Final_Model_data.csv"); df <- df[,-1]

# Group data for timevis output
groupsData <- data.frame(id = 1:4, content = c("Screen 1", "Screen 2", "Screen 3", "Screen 4"))

#----- Global Functions -----
# Data
weekScraper <- function() {
  urlW <- "https://www.the-numbers.com/box-office-chart/weekly/"
  lu <- format(floor_date(Sys.Date()-7, unit = "w", week_start = 5), format = "%Y/%m/%d")
  testW <- paste0(urlW, lu) %>% read_html() %>% html_nodes(xpath='//*[@id="page_filling_chart"]/center[1]/table') %>% html_table()
  testW <- testW[[1]][,-c(1:2)]
  if (nrow(testW) < 1) {
    lu <- format(floor_date(ymd("2019-05-24")-8, unit = "w", week_start = 5),
                 format = "%Y/%m/%d")
    testW <- paste0(urlW, lu) %>% read_html() %>% html_nodes(xpath='//*[@id="page_filling_chart"]/center[1]/table') %>% html_table()
    testW <- testW[[1]][,-c(1:2)]
  }
  testW$Gross <- as.numeric(gsub("[$,]", "", testW$Gross))
  testW$Change <- as.numeric(gsub("[+%]", "", testW$Change))
  testW$Thtrs. <- as.numeric(gsub("[,]", "", testW$Thtrs.))
  testW$`Per Thtr.` <- as.numeric(gsub("[$,]", "", testW$`Per Thtr.`))
  testW$`Total Gross` <- as.numeric(gsub("[$,]", "", testW$`Total Gross`))
  colnames(testW) <- tolower(colnames(testW))
  return(testW)
}

monthScraper <- function() {
  urlM <- "https://www.the-numbers.com/box-office-chart/weekly/"
  lu <- seq(floor_date(Sys.Date()-28, unit = "w", week_start = 5), Sys.Date()-1, by = "w")
  lu <- format(lu, format = "%Y/%m/%d")
  testM <- sapply(lu, function(x) paste0(urlM, x) %>% read_html() %>% html_nodes(xpath='//*[@id="page_filling_chart"]/center[1]/table') %>% html_table())
  temp <- c()
  for (i in 1:length(testM)) {
    temp <- rbind(temp, testM[[i]][,-c(1:2)])
  }
  testM <- temp; rm(i, temp)
  testM$Gross <- as.numeric(gsub("[$,]", "", testM$Gross))
  testM$Change <- as.numeric(gsub("[+%]", "", testM$Change))
  testM$Thtrs. <- as.numeric(gsub("[\\,]", "", testM$Thtrs.))
  testM$`Per Thtr.` <- as.numeric(gsub("[$,]", "", testM$`Per Thtr.`))
  testM$`Total Gross` <- as.numeric(gsub("[$,]", "", testM$`Total Gross`))
  colnames(testM) <- tolower(colnames(testM))
  return(testM)
}

quarterScraper <- function() {
  urlQ <- "https://www.the-numbers.com/box-office-chart/weekly/"
  
  lu <- seq(ceiling_date(floor_date(Sys.Date(), unit = "q"), unit = "w", week_start = 5),
            Sys.Date()-1, by = "w")
  lu <- format(lu, format = "%Y/%m/%d")
  testQ <- sapply(lu, function(x) paste0(urlQ, x) %>% read_html() %>% html_nodes(xpath='//*[@id="page_filling_chart"]/center[1]/table') %>% html_table())
  temp <- c()
  for (i in 1:length(testQ)) {
    temp <- rbind(temp, testQ[[i]][,-c(1:2)])
  }
  testQ <- temp; rm(i, temp)
  testQ$Gross <- as.numeric(gsub("[$,]", "", testQ$Gross))
  testQ$Change <- as.numeric(gsub("[+%]", "", testQ$Change))
  testQ$Thtrs. <- as.numeric(gsub("[,]", "", testQ$Thtrs.))
  testQ$`Per Thtr.` <- as.numeric(gsub("[$,]", "", testQ$`Per Thtr.`))
  testQ$`Total Gross` <- as.numeric(gsub("[$,]", "", testQ$`Total Gross`))
  colnames(testQ) <- tolower(colnames(testQ))
  return(testQ)
}

# Colors
colFunc <- colorRampPalette(c("mediumpurple3", "lightsteelblue1"))

# Dashboard
plotCompBarplot <- function(w, m, q, x) {
  wBO1 <- sum(w$`per thtr.`[!is.na(w$`per thtr.`)])
  mBO1 <- sum(m$`per thtr.`[!is.na(m$`per thtr.`)])
  qBO1 <- sum(q$`per thtr.`[!is.na(q$`per thtr.`)])
  gaW <- wBO1*runif(1,0.5,0.8); gaM <- mBO1*runif(1,0.55,0.85); gaQ <- qBO1*runif(1,0.6,0.9)
  dat <- data.frame(stringsAsFactors = FALSE,
                    time = rep(c("Week", "Month", "Quarter"), each = 6),
                    type = rep(rep(c("domestic", "international", "global"), each = 2), 3),
                    group = rep(c("Average per Theater", "Glen Art"), 9),
                    gross = round(c(wBO1, gaW, wBO1*runif(1,0.2,0.5), gaW, NA, gaW,
                                    mBO1, gaM, mBO1*runif(1,0.2,0.5), gaM, NA, gaM,
                                    qBO1, gaQ, qBO1*runif(1,0.2,0.5), gaQ, NA, gaQ)))
  dat[is.na(dat$gross), "gross"] <- dat[dat$type == "domestic" & dat$group == "Average per Theater", "gross"] + dat[dat$type == "international" & dat$group == "Average per Theater", "gross"]
  dat$time <- factor(dat$time, levels = c("Week", "Month", "Quarter"))
  
  # Plot
  if (x == "domestic") {
    datd <- dat[dat$type == "domestic",]
    g <- ggplot(datd, aes(x = time, y = gross)) +
      geom_bar(aes(fill = group), stat = "identity", position = position_dodge2()) +
      xlab("") + ylab("Box Office Gross") + ggtitle("Glen Art Revenue Comparison") +
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      scale_fill_manual(values = c("mediumpurple3", "lightsteelblue1"), name = "") +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "top")
  } else if (x == "international") {
    dati <- dat[dat$type == "international",]
    g <- ggplot(dati, aes(x = time, y = gross)) +
      geom_bar(aes(fill = group), stat = "identity", position = position_dodge2()) +
      xlab("") + ylab("Box Office Gross") +
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      scale_fill_manual(values = c("mediumpurple3", "lightsteelblue1"), name = "") +
      theme(axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "top")
  } else {
    datg <- dat[dat$type == "global",]
    g <- ggplot(datg, aes(x = time, y = gross)) +
      geom_bar(aes(fill = group), stat = "identity", position = position_dodge2()) +
      xlab("") + ylab("Box Office Gross") +
      scale_y_continuous(labels = scales::dollar) +
      theme_bw() +
      scale_fill_manual(values = c("mediumpurple3", "lightsteelblue1"), name = "") +
      theme(axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "top")
  }
  return(g)
}

plotMoviePareto <- function(dat, x) {
  # Manipulate data
  dat1 <- dat %>% dplyr::group_by(movie) %>%
    dplyr::summarize(gross = sum(gross)) %>%
    dplyr::filter(!is.na(gross)) %>% 
    dplyr::arrange(dplyr::desc(gross)) %>% 
    dplyr::mutate(cp = cumsum(gross)/sum(gross), movie = factor(movie, levels = movie))
  if (x == "q") {
    dat1 <- dat1 %>% dplyr::filter(signif(cp, 1) <= 0.75)
  } else {
    dat1 <- dat1 %>% dplyr::filter(signif(cp, 1) <= 0.8)
  }
  
  # Plot
  g <- ggplot(dat1, aes(x = movie, y = gross)) +
    geom_bar(stat = "identity", fill = "mediumpurple3") +
    xlab("Film") + ylab("Box Office Gross") + ggtitle("Top Performing Films") +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
    scale_y_continuous(labels = scales::dollar) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 16),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  return(g)
}

# Analytics
dayMultiplier <- function(d) {
  ifelse(d == 1, rnorm(1, 0.18, 0.03),
  ifelse(d == 2, rnorm(1, 0.07, 0.02),
  ifelse(d == 3, rnorm(1, 0.06, 0.02),
  ifelse(d == 4, rnorm(1, 0.07, 0.02),
  ifelse(d == 5, rnorm(1, 0.19, 0.03),
  ifelse(d == 6, rnorm(1, 0.21, 0.04), rnorm(1, 0.22, 0.04)))))))
}

getFutureBO <- function(pt, pd = NULL, pf) {
  if (pt == "Q3") {
    Date <- seq(as.POSIXct("2019-07-01"), as.POSIXct("2019-09-30"), by = "day")
  } else if (pt == "Q4") {
    Date <- seq(as.POSIXct("2019-10-01"), as.POSIXct("2019-12-31"), by = "day")
  } else {
    Date <- seq(as.POSIXct(pd[1]), as.POSIXct(pd[2]), by = "day")
  }
  
  inc <- 1/length(pf)
  dat <- data.frame(Date, stringsAsFactors = FALSE)
  withProgress(message = "Calculating Projected Gross Income", value = 0.1, {
    for (i in 1:length(pf)) {
      x <- c()
      ge <- ifelse(movieDB[movieDB$film == pf[i], "popularity"] == 5, rnorm(1, u1, s1),
            ifelse(movieDB[movieDB$film == pf[i], "popularity"] == 4, rnorm(1, u2, s2),
            ifelse(movieDB[movieDB$film == pf[i], "popularity"] == 3, rnorm(1, u3, s3),
            ifelse(movieDB[movieDB$film == pf[i], "popularity"] == 2, rnorm(1, u4, s4), rnorm(1, u5, s5)))))/400
      for (j in 1:length(Date)) {
        if (Date[j] <= movieDB[movieDB$film == pf[i], "endDate"]) {
          wk <- ceiling(as.numeric(as.duration(movieDB[movieDB$film == pf[i], "startDate"] %--% Date[j]), "weeks"))
          x <- c(x, ifelse(wk == 1, ge*dayMultiplier(wday(Date[j]))*0.304,
                    ifelse(wk == 2, ge*dayMultiplier(wday(Date[j]))*0.1875,
                    ifelse(wk == 3, ge*dayMultiplier(wday(Date[j]))*0.1284,
                    ifelse(wk == 4, ge*dayMultiplier(wday(Date[j]))*0.0944,
                    ifelse(wk == 5, ge*dayMultiplier(wday(Date[j]))*0.0713,
                    ifelse(wk == 6, ge*dayMultiplier(wday(Date[j]))*0.0615,
                    ifelse(wk == 7, ge*dayMultiplier(wday(Date[j]))*0.051, ge*dayMultiplier(wday(Date[j]))*0.0439))))))))
        } else {
          x <- c(x, NA)
        }
      }
      dat <- cbind(dat, round(x))
      incProgress(inc, detail = pf[i])
    }
  })
  colnames(dat)[2:ncol(dat)] <- pf
  dat$Total <- rowSums(dat[,-1, drop = FALSE], na.rm = TRUE)
  dat$Total <- ifelse(dat$Total == 0, NA, dat$Total)
  return(dat)
}

# Scheduler
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

getIndSmallD <- function(df, n) {
  df$d[df$content %in% n] <- NA
  which.min(df$d)
}

optimizeShowtimes <- function(showDate = "2019-05-21", firstShow = "11:00", lastShow = "00:30", interval = 5, filmList, screens = 4, allShown = 1) {
  withProgress(message = "Optimizing Showtimes", value = 0.1, {
    
    incProgress(1/5, detail = "Determing all possible show times...")
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
    films <- movieDB[movieDB$film %in% filmList,]
    
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
    
    incProgress(1/5, detail = "Calculating demand for each selected film...")
    # Add demand for movies at each start time
    inc <- (3/5-2/5)/nrow(showtimes)
    d <- c()
    for (i in 1:nrow(showtimes)) {
      ge <- ifelse(films[films$film == rownames(showtimes)[i], "popularity"] == 5, rnorm(1, u1, s1),
            ifelse(films[films$film == rownames(showtimes)[i], "popularity"] == 4, rnorm(1, u2, s2),
            ifelse(films[films$film == rownames(showtimes)[i], "popularity"] == 3, rnorm(1, u3, s3),
            ifelse(films[films$film == rownames(showtimes)[i], "popularity"] == 2, rnorm(1, u4, s4), rnorm(1, u5, s5)))))/400
      
      wk <- ceiling(as.numeric(as.duration(films[films$film == rownames(showtimes)[i], "startDate"] %--% showDate), "weeks"))
      d <- c(d, ifelse(wk == 1, ge*dayMultiplier(wday(showDate))*0.304,
                ifelse(wk == 2, ge*dayMultiplier(wday(showDate))*0.1875,
                ifelse(wk == 3, ge*dayMultiplier(wday(showDate))*0.1284,
                ifelse(wk == 4, ge*dayMultiplier(wday(showDate))*0.0944,
                ifelse(wk == 5, ge*dayMultiplier(wday(showDate))*0.0713,
                ifelse(wk == 6, ge*dayMultiplier(wday(showDate))*0.0615,
                ifelse(wk == 7, ge*dayMultiplier(wday(showDate))*0.051, ge*dayMultiplier(wday(showDate))*0.0439))))))))
      incProgress(inc, detail = "Calculating demand for each selected film...")
    }
    d <- round(d/max(d)*100, 1); showtimes <- cbind(showtimes, d); rm(d)
    
    # Sort by finish time
    showtimes <- showtimes[order(showtimes[,"f"]),]
    
    incProgress(1/5, detail = "Running optimization algorithm...")
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
    
    incProgress(1/5, detail = "Checking if all constraints met...")
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
    temp <- c()
    for (l in 1:length(optSchedule1)) {
      temp <- rbind(temp, optSchedule1[[l]])
    }
    optSchedule1 <- temp; rm(temp)
    optSchedule <- optSchedule1; rm(optSchedule1)
    
    optSchedule$start <- names(screeningWindow)[optSchedule$start]
    optSchedule$end <- names(screeningWindow)[optSchedule$end]
    
    optSchedule$title <- paste(optSchedule$content, "\n",
                               substr(optSchedule$start, 12, 16), "-", 
                               substr(optSchedule$end, 12, 16), "\nDemand:",
                               optSchedule$d)
  })
  return(optSchedule)
}

library(data.table)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(RJDBC)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(rjson)

#---------------------------------------------------------------------------------------------------------------------#
# function to read data from database
fetchDB <- function(dbTable, startDate = '2015-01-01', endDate = '2015-12-31'){
  # Establish connection to Vertica DB
  # get IP address to figure our which connection to use
  myIP <- fromJSON(readLines("http://api.hostip.info/get_json.php", warn=F))$ip
  
  if(myIP != "15.126.252.31"){
    # this is the local connection configuration
    BDDE_himss <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="C:/Users/abdoa/Downloads/vertica-jdbc-7.1.1-0.jar")
    himss <- dbConnect(BDDE_himss, "jdbc:vertica://localhost/BDDE_himss", username = dbTable, password = "vertica")
  }
  else{
    # this is the server connection configuration
    BDDE_himss <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="~shiny2/vertica-jdbc-7.1.1-0.jar")
    himss <- dbConnect(BDDE_himss, "jdbc:vertica://206.164.65.108/BDDE_himss", username = dbTable, password = "vertica")
  }
  # buile query
  query <- sprintf("
                   SELECT hss.* 
                   FROM health_status_snapshot hss 
                   INNER JOIN participant p 
                   ON p.participant_id = hss.participant_id
                   WHERE DATE(hss.health_status_snapshot_date) >= \'%s\' AND DATE(hss.health_status_snapshot_date) <= \'%s\' AND p.participant_id != 9999
                   --AND p.beacon_id IS NOT NULL
                   AND p.email IS NOT NULL
                   ", startDate, endDate)
  
  
  DT <- fetch(dbSendQuery(himss, query), n = -1)
  
  # disconnect from DB
  dbDisconnect(himss)
  
  # convert read data into data.table
  DT <- as.data.table(DT)
  
  # set data.table key for speed
  setkey(DT, health_status_snapshot_date)
  
  return(DT)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
# function to process data.table read from Vertica DB
processDT <- function(DT, simulate = FALSE, addXY = TRUE, pUP = 0.5, pDN = 0.2){
  # the simulate parameter should be invoked if no health status updates occur as time passes
  # make a copy of the data.table to process
  DT.tmp <- copy(DT)
  
  # remove unwanted columns
  DT.tmp[, c("health_status_timestamp", "time_recorded", "reason_changed", "synthesized") := NULL]
  
  # set health status to NA if "is_active" flag is false. 
  # these values will be reset to 0 with the f_dowle3 function
  DT.tmp[ is_active == "f", health_status_ref_id := NA]
  
  # simulate data if prompted
  if(simulate){
    # get number of participants in DB
    Npop <- DT.tmp[, length(unique(participant_id))]
    # get number of hours in DB
    iter <- DT.tmp[, length(unique(health_status_snapshot_date))]
    # draw a random sample for health statuses 
    dist <- sample(1:6, Npop, replace = T)
    
    # data.table to contain wide format for plots    
    DTW <- data.table(participant_id = DT.tmp[, unique(participant_id)], HS.1 = sample(x = dist, size = Npop, replace = T))
    DTW[, level.1 := as.factor(sapply(HS.1, bucket2))]
    set.seed(123)
    for(i in 2:iter){
      one <- paste0("HS.",i)
      two <- paste0("HS.",i-1)
      three <- paste0("level.", i)
      four <- paste("change", i, i-1, sep = "_")
      DTW[, as.character(one) := sapply(eval(parse(text=two)), change, p_up = pUP, p_dn = pDN)]
      DTW[, as.character(four) := as.factor(sapply(diag(outer(eval(parse(text = one)), eval(parse(text= two)), "-")), factorize))]
      DTW[(eval(parse(text = two)) == 0 & eval(parse(text = one)) == 0) | 
            (eval(parse(text = two)) == 0 & eval(parse(text = one)) > 0), as.character(four) := "Inactive"]
      DTW[, as.character(three) := as.factor(sapply(eval(parse(text=one)), bucket2))]
    }
  }
  else{
    # convert to wide format to match code 
    DTW <- dcast.data.table(DT.tmp, participant_id ~ health_status_snapshot_date, value.var = "health_status_ref_id")
    
    # change columns names
    Names <- paste0("HS.", 1:(ncol(DTW) - 1))
    setnames(DTW, c("participant_id", Names))
    
    f_dowle3(DTW)
    
    DTW[, level.1 := as.factor(sapply(HS.1, bucket2))]
    # now add new calculated columns necessary for the visualization
    for(i in 2:(ncol(DTW)-2)){
      one <- paste0("HS.",i)
      two <- paste0("HS.",i-1)
      three <- paste0("level.", i)
      four <- paste("change", i, i-1, sep = "_")
      DTW[, as.character(four) := as.factor(sapply(diag(outer(eval(parse(text = one)), eval(parse(text= two)), "-")), factorize))]
      DTW[(eval(parse(text = two)) == 0 & eval(parse(text = one)) == 0) | 
            (eval(parse(text = two)) == 0 & eval(parse(text = one)) > 0), as.character(four) := "Inactive"]
      DTW[, as.character(three) := as.factor(sapply(eval(parse(text=one)), bucket2))]
    }
  }
  
  if(addXY){
    tmp <- squareFun2(nrow(DTW))
    tmp <- tmp[sample(1:nrow(tmp))]
    DTW[, c("x", "y") := list(tmp[1:nrow(DTW), x], tmp[1:nrow(DTW), y])]
  }
  setkey(DTW, participant_id)
  return(DTW)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
# function to factorize health statuses
bucket <- function(x){
  if(x<3) HS.level <- "Healthy"
  else if(x <5) HS.level <- "Symptomatic"
  else HS.level <- "Infectious"
  return(HS.level)
}

bucket2 <- function(x){
  if(x == 0) HS.level <- "Inactive"
  else if(0 < x & x < 3) HS.level <- "Healthy"
  else if(x <5) HS.level <- "Symptomatic"
  else HS.level <- "Infectious"
  return(HS.level)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
# function to change health status accroding to coin tossing
change <- function(HS, p_up, p_dn){
  if(HS < 6){
    if(rbinom(1, 1, p_up)){HS = HS + 1}
    if(rbinom(1, 1, p_dn)){HS = 1}
  }
  return(HS)
}
#---------------------------------------------------------------------------------------------------------------------#

squareFun <- function(x){
  a <- b  <- ceiling(sqrt(x))
  dt <- data.table(x = rep(1:a, each = b), y = 1:b)
  setkey(dt, x)
  return(dt)
}

# function to make an almost-square data.table for plotting dots
squareFun2 <- function(x){
  # if number has an integer sqrt then just take it and proceed
  if(sqrt(x)%%1 == 0 ){ 
    a <- b <- sqrt(x)
  }
  else{
    a1 <- b1  <- ceiling(sqrt(x))
    
    if(x%%2 == 1) x <- x + 1
    div <- seq_len(x)
    y <- div[x %% div == 0]
    if((length(y) %% 2) == 1){ 
      a2 <- b2 <- y[(length(y)-1)/2 + 1]
    }
    else {
      a2 <- y[length(y)/2] 
      b2 <- y[(length(y)/2 + 1)]
    }
    
    if(abs(a2-b2) <= 3){
      a <- a2
      b <- b2
    }
    else{
      a <- a1
      b <- b1
    }
  }
  dt <- data.table(x = rep(1:a, each = b), y = 1:b)
  setkey(dt, x)
  return(dt)
}

#---------------------------------------------------------------------------------------------------------------------#
# function to simulate population
simPopulation <- function(iter, Npop, pUP, pDN){
  # initial distribution of health statuses
  # Npop <- 100 # number of simulated personas
  dist <- c(rep(1:2, Npop*0.3) , rep(3:4, Npop*0.15),rep(5:6, Npop*0.05))
  
  # start building the data.table
  tmp <- squareFun(Npop)
  population <- data.table( x = tmp[1:Npop, x], y = tmp[1:Npop, y], HS.1 = sample(dist))
  
  # we'll be adding a factor varialbe to show the levels of health status
  population[, level.1 := as.factor(sapply(HS.1, bucket))]
  
  for(i in 2:iter){
    one <- paste0("HS.",i)
    two <- paste0("HS.",i-1)
    three <- paste0("level.", i)
    four <- paste("change", i, i-1, sep = "_")
    population[, as.character(one) := sapply(eval(parse(text=two)), change, p_up = pUP, p_dn = pDN)]
    population[, as.character(four) := as.factor(sapply(diag(outer(eval(parse(text = one)), eval(parse(text= two)), "-")), factorize))]
    population[(eval(parse(text = two)) == 0 & eval(parse(text = one)) == 0) | 
                 (eval(parse(text = two)) == 0 & eval(parse(text = one)) > 0), as.character(four) := "Inactive"]
    population[, as.character(three) := as.factor(sapply(eval(parse(text=one)), bucket))]
  }
  return(population)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
factorize <- function(x){
  if(x < 0) value <- "Recovery"
  else if(x == 0) value <- "Steady"
  else value <- "Sicker"
  return(value)
}

factorize2 <- function(x1, x2){
  if(x1)
    sub <- outer(x1, x2, "-")
  
  if(x < 0) value <- "Recovery"
  else if(x == 0) value <- "Steady"
  else value <- "Sicker"
  return(value)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
Bo <- function(DTW){
  
  DT.tmp <- copy(DTW)
  
  # delete unwanted columns
  cols <- c(DT.tmp[, grep("^x$", colnames(DT.tmp))], DT.tmp[, grep("^y$", colnames(DT.tmp))], 
            DT.tmp[, grep("level", colnames(DT.tmp))], DT.tmp[, grep("change", colnames(DT.tmp))])
  DT.tmp[, (cols) := NULL]
  
  vec <- vector()
  tmp <- data.table()
  for (i in 1:6) vec[i] <- DT.tmp[, sum(HS.1==i)]
  tmp <- rbindlist(list(tmp, as.list(vec)))
  
  for(i in 2:(ncol(DT.tmp)-1)){
    for(j in 1:6) vec[j] <- DT.tmp[, sum(eval(parse(text = paste0('HS.', i))) == j )]
    tmp <- rbindlist(list(tmp, as.list(vec)))    
  }
  
  setnames(tmp, paste0('HS.', 1:ncol(tmp)))
  tmp[, iter := 1:nrow(tmp)]
  return(tmp)
}
#---------------------------------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------------------------------#
makePlot <- function(DT, DTW, level = 1){
  if(DTW[, length(grep("HS.", colnames(DTW)))] < 2){
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.85, paste("There isn't enough time histroy to display this figure.\n", 
                                  "Please allow at least 2 hours to pass before attempting to access this figure."), 
         cex = 1.6, col = "steelblue")
  }
  else{
    set.seed(123)
    
    population <- copy(DTW)
    
    Level <- paste('level', level, sep=".")
    Change <- paste('change', level , level-1 , sep="_")
    
    p <- ggplot(population, aes(x = x, y = y)) + theme_bw()
    p <- p + geom_point(aes_string(fill = Level), shape = 21, size = 12, col = "white") 
    if(level >= 2){
      # here we add the circles Bo wanted
      # make sure legned is not displayed, this is done with the guide = FALSE line
      p <- p + geom_point(shape = 0, aes_string(col = Change), size = 14)
      p <- p + scale_color_manual(values=c("Recovery" = "black","Sicker" = "white","Steady" = "white"), guide = FALSE)
    }
    
    p <- p + theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.position="bottom",
                   legend.key = element_blank(),
                   panel.grid.major=element_blank(),
                   panel.background = element_rect(fill = "white", colour = "grey50", size = 2),
                   panel.grid.minor=element_blank(),
                   legend.title = element_text(colour="black", size=16, face="bold"),
                   legend.text = element_text(colour="black", size = 16, face = "bold")
    )
    
    p <- p + scale_fill_manual(name  = "", breaks = c("Inactive", "Healthy", "Symptomatic", "Infectious"),
                               labels =  c("Inactive ", "Healthy  ", "Symptomatic  ", "Infectious  "),
                               values = c("Inactive" = "#C2E6F5", "Healthy" = "#0096D6", 
                                          "Symptomatic" = "#FAAD9E", "Infectious" = "#990026"))
    
    label <- as.POSIXct(DT[, unique(health_status_snapshot_date)])[level]
    
    p <- arrangeGrob(p, sub = textGrob(as.character(label), x = 0, hjust = -0.1, vjust=0.1, 
                                       gp = gpar(fontface = "italic", fontsize = 20)))
    
    print(p)
  }
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
linePlot <- function(DT, DTW){
  if(DTW[, length(grep("HS.", colnames(DTW)))] < 3){
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.85, paste("There isn't enough time histroy to display this figure.\n", 
                                  "Please allow at least 3 hours to pass before attempting to access this figure."), 
         cex = 1.6, col = "steelblue")
  }
  else{
    trend <- melt(Bo(DTW), id = 'iter')
    trend[, time := as.POSIXct(DT[, unique(health_status_snapshot_date)])]
    
    pline <- ggplot(trend, aes(x = time, y = value, col = variable)) + geom_point() + geom_line(alpha = 0.35) + theme_bw()
    pline <- pline + theme(legend.position = "bottom") + xlab("\nTime (Hours)") + ylab("Count") + facet_wrap(~ variable)
    pline <- pline + geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, formula = 'y ~ ns(x, 2)', 
                                 aes(fill = variable), alpha = 0.115, lty = 2) 
    pline <- pline + scale_color_manual(name = "", breaks = paste0('HS.', 1:6),
                                        values = c( "#70D4FF", "#0096D6", "#FAAD9E", "#F44B2A", "#990026", "#66001A"), 
                                        labels = paste0('Health Status ', 1:6, ' '))
    # now get rid of the other legend
    pline <- pline + scale_fill_manual(name = "", breaks = paste0('HS.', 1:6),
                                       values = rep(c("#30AC30", "#FFCC00", "#FF3030"), each = 2), 
                                       labels = paste0('HS.', 1:6), guide = FALSE)
    pline <- pline + commonTheme + guides(colour = guide_legend(nrow = 2))
    pline <- pline + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.key = element_blank())
    print(pline)
  }
}

# -------------------------------------------------------------------------------------#
# Add percentages at each iteration. 
# make plot of those
# show rate of change plot
# -------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
trendPlot <- function(DT, DTW){
  if(DTW[, length(grep("HS.", colnames(DTW)))] < 2){
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.85, paste("There isn't enough time histroy to display this figure.\n", 
                                  "Please allow at least 2 hours to pass before attempting to access this figure."), 
         cex = 1.6, col = "steelblue")
  }
  else{
    pop.tmp <- DTW[, lapply(.SD, summaryFun), .SDcols = DTW[ , grep("level", colnames(DTW)) ]]
    pop.tmp[, reference := c("Inactive", "Healthy", "Symptomatic", "Infectious")]
    setkey(pop.tmp, reference)
    
    pop.tmp.long <- melt(pop.tmp, id.vars = "reference")
    pop.tmp.long[, time := rep(as.POSIXct(DT[, unique(health_status_snapshot_date)]), each = 4)]
    
    p2 <- ggplot(pop.tmp.long, aes(x = time, y = value, col = reference)) + geom_line(size = 1., alpha = 0.35) 
    p2 <- p2 + geom_point(size = 4) + theme_bw()
    p2 <- p2 + theme(legend.position = "bottom", legend.key = element_blank()) + ylab("Count\n") + xlab("\nTime ")
    p2 <- p2 + ggtitle("Trend of Disease Outbreak Over Time\n")
    p2 <- p2 + commonTheme
    p2 <- p2 +  scale_color_manual(name  = "", breaks = c("Inactive", "Healthy", "Symptomatic", "Infectious"),
                                   labels =  c("Inactive ", "Healthy  ", "Symptomatic  ", "Infectious  "),
                                   values = c("Inactive" = "#C2E6F5", "Healthy" = "#0096D6", 
                                              "Symptomatic" = "#FAAD9E", "Infectious" = "#990026"))
    print(p2)
  }
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
trendPlot2 <- function(DT, DTW){
  if(DTW[, length(grep("HS.", colnames(DTW)))] < 3){
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.85, paste("There isn't enough time histroy to display this figure.\n", 
                                  "Please allow at least 3 hours to pass before attempting to access this figure."), 
         cex = 1.6, col = "steelblue")
  }
  else{
    # apply pre defined summary function to aggregate the change columns
    pop.tmp <- DTW[, lapply(.SD, summaryFun2), .SDcols = DTW[ , grep("change", colnames(DTW)) ]]
    # add column for reference
    pop.tmp[, reference := as.factor(c("Inactive", "Recovery", "Sicker", "Steady"))]
    setkey(pop.tmp, reference)
    
    # convert into long format
    pop.tmp.long <- melt(pop.tmp, id.vars = "reference")
    # notice that since the "change" variable is less by 1 compared to the nubmer of participants
    # we have to exclude the first time stamp, i.e. first change starts with the introduction of 
    # the second participant
    pop.tmp.long[, time := rep(as.POSIXct(DT[, unique(health_status_snapshot_date)][-1]), each = 4)]
    
    p2 <- ggplot(pop.tmp.long, aes(x = time, y = value, col = reference)) + geom_line(size = 1., alpha = 0.35) 
    p2 <- p2 + geom_point(size = 4) + theme_bw()
    p2 <- p2 + theme(legend.position = "bottom", legend.key = element_blank()) + ylab("Count\n") + xlab("\nTime ")
    p2 <- p2 + ggtitle("Trend of Recovery Over Time\n")
    p2 <- p2 + commonTheme
    p2 <- p2 +  scale_color_manual(name  = "", breaks = c("Inactive", "Recovery", "Sicker", "Steady"),
                                   labels = c("Inactive", "Recovered ", "Got Sicker ", "No Change in Health Status "),
                                   values = c("Inactive" = "#C2E6F5", "Recovery" = "#0096D6", "Sicker" = "#822980", "Steady" = "#87898B"))
    print(p2)
  }
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
summaryFun <- function(x){
  tmp <- summary(x)
  if(is.na(tmp["Healthy"])){tmp["Healthy"] <- 0}
  if(is.na(tmp["Inactive"])){tmp["Inactive"] <- 0}
  if(is.na(tmp["Symptomatic"])){tmp["Symptomatic"] <- 0}
  if(is.na(tmp["Infectious"])){tmp["Infectious"] <- 0}
  tmp <- list("Inactive" = tmp[["Inactive"]],
              "Healthy" = tmp[["Healthy"]],
              "Symptomatic" = tmp[["Symptomatic"]],
              "Infectious" = tmp[["Infectious"]]
  )
  return(unlist(tmp))
}

summaryFun2 <- function(x){
  tmp <- summary(x)
  if(is.na(tmp["Inactive"])){tmp["Inactive"] <- 0}
  if(is.na(tmp["Recovery"])){tmp["Recovery"] <- 0}
  if(is.na(tmp["Sicker"])){tmp["Sicker"] <- 0}
  if(is.na(tmp["Steady"])){tmp["Steady"] <- 0}
  tmp <- list("Inactive" = tmp[["Inactive"]],
              "Recovery" = tmp[["Recovery"]],
              "Sicker" = tmp[["Sicker"]],
              "Steady" = tmp[["Steady"]]
  )
  return(unlist(tmp))
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
# function to reset NAs in data.table to 0 values. 
# Taken from Matt Dowle <http://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table>
f_dowle3 = function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT, which(is.na(DT[[j]])), j, 0)
}
#---------------------------------------------------------------------------------------------------------------------#

selectDT <- function(DT, day = NULL, dayTimeOnly = TRUE){
  if(is.null(day)){
    days <- unique(grep("2015", unlist(strsplit(DT[1:nrow(DT), health_status_snapshot_date], split = " ")), value = T))
    DT.tmp <- DT[like(health_status_snapshot_date, paste(days, collapse = "|"))]
  }
  else{
    DT.tmp <- DT[like(health_status_snapshot_date, day)]
  }
  return(DT.tmp)
}

# function to select day time only events
dayTimeOnlyFun <- function(DT, days = NULL, dayStart = 8, dayEnd = 20){
  # get days if null is input
  if(is.null(days)) {
    days <- unique(grep("2015", unlist(strsplit(DT[1:nrow(DT), health_status_snapshot_date], split = " ")), value = T))
  }
  
  # temporary vectors to hold list of days and hours
  v1 <- vector(length = length(days))
  v2 <- vector(length = length(days))
  
  for (i in seq_along(days)) {
    v1[i] <- paste(paste0("\'", days[i]), paste0(formatC(as.numeric(dayStart),width=2,format='f',digits=0,flag='0'), ":00:00\'"))
    v2[i] <- paste(paste0("\'", days[i]), paste0(formatC(as.numeric(dayEnd),width=2,format='f',digits=0,flag='0'), ":00:00\'"))
  }
  
  # temporary data.table to hold list 
  # the following step is necessary for interleaving the two vectors
  idx <- order(c(seq_along(v1), seq_along(v2)))
  tmp.DT <- data.table(time = unlist(c(v1, v2))[idx], day = c(1,0))
  tmp.DT[, condition := as.character(paste("health_status_snapshot_date",">", time))]
  tmp.DT[day == 0 , condition := as.character(paste("health_status_snapshot_date","<", time))]
  
  # delete columns no longer needed
  tmp.DT[, c("time", "day") := NULL]
  
  # temporary vector to contain conditions
  condition <- vector()
  
  # build conditions
  for(i in seq(1, nrow(tmp.DT), 2) ){
    condition[i] <- paste(tmp.DT[i], '&', tmp.DT[i+1])
  }
  
  condition <- condition[!is.na(condition)]
  finalCondition <- paste(condition, collapse = " | ")
  
  # apply conditions to data.table
  value <- DT[eval(parse(text = finalCondition))]
  return(value)
}


#---------------------------------------------------------------------------------------------------------------------#
# common theme for the ggplots
commonTheme <- theme(axis.text.x = element_text(angle=0, hjust=1, size = 14),
                     axis.title.x = element_text(face="bold", colour="black", size=16),
                     axis.text.y = element_text(angle=0, hjust=1, size = 14),
                     axis.title.y = element_text(face="bold", colour="black", size=16),
                     plot.title = element_text(size = 20), 
                     legend.title = element_text(colour="black", size=16, face="bold"),
                     legend.text = element_text(colour="black", size = 16, face = "bold"))
#---------------------------------------------------------------------------------------------------------------------#
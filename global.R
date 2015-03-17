library(data.table)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(RJDBC)
library(RColorBrewer)
library(scales)
library(gridExtra)

#---------------------------------------------------------------------------------------------------------------------#
# function to read data from database
fetchDB <- function(dbTable){
  #Establish connection to Vertica DB
 BDDE_himss <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="C:/Users/abdoa/Downloads/vertica-jdbc-7.1.1-0.jar")
 himss <- dbConnect(BDDE_himss, "jdbc:vertica://localhost/BDDE_himss", username = dbTable, password = "vertica")
  
     # BDDE_himss <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="~shiny2/vertica-jdbc-7.1.1-0.jar")
     # himss <- dbConnect(BDDE_himss, "jdbc:vertica://206.164.65.108/BDDE_himss", username = dbTable, password = "vertica")
  
  # read snapshot health status table as a data.tble
  DT <- fetch(dbSendQuery(himss, "SELECT * FROM health_status_snapshot WHERE participant_id >= 170"), n = -1)
  DT <- as.data.table(DT)
  
  setkey(DT, health_status_snapshot_date)
  return(DT)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
# function to process data.table read from Vertica DB
processDT <- function(DT, simulate = FALSE, addXY = TRUE){
  # the simulate parameter should be invoked if no health status updates occur as time passes
  # make a copy of the data.table to process
  DT.tmp <- copy(DT)
  
  if(simulate){
    # randomize 
    DT.tmp[, health_status_ref_id := sample(1:6, nrow(DT.tmp), replace = TRUE)]
  }
  # remove unwanted columns
  DT.tmp[, c("health_status_timestamp", "time_recorded", "reason_changed") := NULL]
  
  # convert to wide format to match code 
  DTW <- dcast.data.table(DT.tmp, participant_id ~ health_status_snapshot_date, value.var = "health_status_ref_id")
  
  # change columns names
  Names <- paste0("HS.", 1:(ncol(DTW) - 1))
  setnames(DTW, c("participant_id", Names))
  
  DTW[, level.1 := as.factor(sapply(HS.1, bucket))]
  
  # now add new calculated columns necessary for the visualization
  for(i in 2:(ncol(DTW)-2)){
    one <- paste0("HS.",i)
    two <- paste0("HS.",i-1)
    three <- paste0("level.", i)
    four <- paste("change", i, i-1, sep = "_")
    # DTW[, as.character(one) := sapply(eval(parse(text=two)), change, p_up = pUP, p_dn = pDN)]
    DTW[, as.character(four) := as.factor(sapply(diag(outer(eval(parse(text = one)), eval(parse(text= two)), "-")), factorize))]
    DTW[, as.character(three) := as.factor(sapply(eval(parse(text=one)), bucket))]
  }
  
  if(addXY){
    tmp <- squareFun(nrow(DTW))
    DTW[, c("x", "y") := list(tmp[1:nrow(DTW), x], tmp[1:nrow(DTW), y])]
  }
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
  xs <- ceiling(sqrt(x))
  dt <- data.table(x = rep(1:xs, each = xs), y = 1:xs)
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
    # population[, as.character(four) := sapply(eval(parse(text = four)), factorize)]
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
  set.seed(123)
  
  population <- copy(DTW)
  
  Level <- paste('level', level, sep=".")
  Change <- paste('change', level , level-1 , sep="_")
  
  p <- ggplot(population, aes(x = x, y = y))
  p <- p + geom_point(aes_string(fill = Level), shape = 21, size = 12, col = "white") 
  if(level >= 2){
    # here we add the circles Bo wanted
    # make sure legned is not displayed, this is done with the guide = FALSE line
    p <- p + geom_point(shape = 21, aes_string(col = Change), size = 14)
    p <- p + scale_color_manual(values=c("Recovery" = "black","Sicker" = "white","Steady" = "white"), guide = FALSE)
  }
  
  p <- p + theme(axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 legend.position="bottom",
                 panel.grid.major=element_blank(),
                 panel.background = element_rect(fill = "#F0F0F0", colour = "grey50", size = 2),
                 panel.grid.minor=element_blank(),
                 legend.title = element_text(colour="black", size=16, face="bold"),
                 legend.text = element_text(colour="black", size = 16, face = "bold")
  )
  
  p <- p + scale_fill_manual(name  = "", breaks = c("Healthy", "Symptomatic", "Infectious"),
                             labels =  c("Healthy  ", "Symptomatic  ", "Infectious  "),
                             values = c("Healthy" = "#30AC30", "Symptomatic" = "#FFCC00", "Infectious" = "#FF3030"))
  
  label <- as.POSIXct(DT[, unique(health_status_snapshot_date)])[level]
  
  # p <- p + annotate("text", x = 3, y = 6.5, label = as.character(label), size = 8, col = "steelblue")
  p <- arrangeGrob(p, sub = textGrob(as.character(label), x = 0, hjust = -0.1, vjust=0.1, 
                                     gp = gpar(fontface = "italic", fontsize = 20)))
  
  print(p)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
linePlot <- function(DT, DTW, xmin, xmax){
  trend <- melt(Bo(DTW), id = 'iter')
  trend[, time := as.POSIXct(DT[, unique(health_status_snapshot_date)])]
  
  pline <- ggplot(trend, aes(x = time, y = value, col = variable)) + geom_point() + geom_line() + theme_bw()
  pline <- pline + theme(legend.position = "bottom") + xlab("\nTime (Hours)") + ylab("Count")
  pline <- pline + geom_smooth(method = "lm", se = TRUE, fullrange = TRUE, formula = 'y ~ ns(x, 2)', 
                               aes(fill = variable), alpha = 0.115, lty = 2) + facet_wrap(~ variable)
  pline <- pline + scale_color_manual(name = "", breaks = paste0('HS.', 1:6),
                                      values = c("#30AC30", "#217821", "#FFCC00", "#cca300", "#FF3030", "#661313"), 
                                      labels = paste0('Health Status ', 1:6, ' '))
  # now get rid of the other legend
  pline <- pline + scale_fill_manual(name = "", breaks = paste0('HS.', 1:6),
                                     values = rep(c("#30AC30", "#FFCC00", "#FF3030"), each = 2), 
                                     labels = paste0('HS.', 1:6), guide = FALSE)
  pline <- pline + commonTheme + guides(colour = guide_legend(nrow = 2))
  pline <- pline + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(pline)
}

# -------------------------------------------------------------------------------------#
# Add percentages at each iteration. 
# make plot of those
# show rate of change plot
# -------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
trendPlot <- function(DT, DTW){
  pop.tmp <- DTW[, lapply(.SD, summaryFun), .SDcols = DTW[ , grep("level", colnames(DTW)) ]]
  pop.tmp[, reference := c("Healthy", "Symptomatic", "Infectious")]
  setkey(pop.tmp, reference)
  
  pop.tmp.long <- melt(pop.tmp, id.vars = "reference")
  # pop.tmp.long[, time := rep(seq(1, ncol(pop.tmp)-1), each = 3)]
  pop.tmp.long[, time := rep(as.POSIXct(DT[, unique(health_status_snapshot_date)]), each = 3)]
  
  p2 <- ggplot(pop.tmp.long, aes(x = time, y = value, col = reference)) + geom_line(size = 1.25) + geom_point(size = 4) + theme_bw()
  p2 <- p2 + theme(legend.position = "bottom") + ylab("Count\n") + xlab("\nTime ")
  p2 <- p2 + ggtitle("Trend of Disease Outbreak Over Time\n")
  p2 <- p2 + commonTheme
  p2 <- p2 +  scale_color_manual(name  = "", breaks = c("Healthy", "Symptomatic", "Infectious"),
                                 labels =  c("Healthy  ", "Symptomatic  ", "Infectious  "),
                                 values = c("Healthy" = "#30AC30", "Symptomatic" = "#FFCC00", "Infectious" = "#FF3030"))
  print(p2)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
trendPlot2 <- function(DT, DTW){
  # apply pre defined summary function to aggregate the change columns
  pop.tmp <- DTW[, lapply(.SD, summaryFun2), .SDcols = DTW[ , grep("change", colnames(DTW)) ]]
  # add column for reference
  pop.tmp[, reference := as.factor(c("Recovery", "Sicker", "Steady"))]
  setkey(pop.tmp, reference)
  
  # convert into long format
  pop.tmp.long <- melt(pop.tmp, id.vars = "reference")
  # notice that since the "change" variable is less by 1 compared to the nubmer of participants
  # we have to exclude the first time stamp, i.e. first change starts with the introduction of 
  # the second participant
  pop.tmp.long[, time := rep(as.POSIXct(DT[, unique(health_status_snapshot_date)][-1]), each = 3)]
  
  p2 <- ggplot(pop.tmp.long, aes(x = time, y = value, col = reference)) + geom_line(size = 1.25) + geom_point(size = 4) + theme_bw()
  p2 <- p2 + theme(legend.position = "bottom") + ylab("Count\n") + xlab("\nTime ")
  p2 <- p2 + ggtitle("Trend of Disease Outbreak Over Time\n")
  p2 <- p2 + commonTheme
  p2 <- p2 +  scale_color_manual(name  = "", breaks = c("Recovery", "Sicker", "Steady"),
                                 labels =  c("Recovery ", "Sicker ", "Steady "),
                                 values = c("Recovery" = "#30AC30", "Sicker" = "#FF3030", "Steady" = "gray"))
  print(p2)
}
#---------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------#
summaryFun <- function(x){
  tmp <- summary(x)
  if(is.na(tmp["Healthy"])){tmp["Healthy"] <- 0}
  if(is.na(tmp["Symptomatic"])){tmp["Symptomatic"] <- 0}
  if(is.na(tmp["Infectious"])){tmp["Infectious"] <- 0}
  return(tmp)
}

summaryFun2 <- function(x){
  tmp <- summary(x)
  if(is.na(tmp["Recovery"])){tmp["Recovery"] <- 0}
  if(is.na(tmp["Sicker"])){tmp["Sicker"] <- 0}
  if(is.na(tmp["Steady"])){tmp["Steady"] <- 0}
  return(tmp)
}
#---------------------------------------------------------------------------------------------------------------------#

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
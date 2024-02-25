#import libraries
library(IrregLong)
#library(MEMSS)
library(survival)
library(readr)
library(geepack)
library(data.table)

data <- read_csv("data/locations_inspectionscores_forMeri_Feb.csv")


#insepction year and inspection score data
years <- data$inspection_year
scores <- data$INSPECTION_SCORE
print(summary(years))
print(summary(scores))
data$time <- as.integer(data$inspection_year)
data$score <- as.numeric(data$INSPECTION_SCORE)
data <- data[order(data$score, data$time), ]

#a simple scatter plot
plot(years, scores,
     main = "Scatter Plot",   # Title of the plot
     xlab = "years",   # Label for the x-axis
     ylab = "scores",   # Label for the y-axis
     col = "blue",            # Color of the points
     pch = 16,                # Shape of the points (16 for filled circles)
     xlim = c(2008, 2020),          # Limit for x-axis
     ylim = c(0, 100),          # Limit for y-axis
     bg = "lightblue",        # Background color of the points
     cex = 0.5                # Size of the points
)



#abacus plot
abacus.plot(
    n=nrow(data),
    time="time",
    id="score",
    data=data,
    tmin=2005,
    tmax=2025,
    xlab.abacus="Time in hours",
    pch=16,
    col.abacus=gray(0.8))


# counts <- extent.of.irregularity(data,time="inspection_year",id="score",
#    scheduledtimes=NULL, cutpoints=NULL,ncutpts=50, 
#    maxfu=16*24, plot=TRUE,legendx=30,legendy=0.8,
#   formula=Surv(time.lag,time,event)~1,tau=16*24)



# data$Apgar <- as.numeric(data$Apgar)
# i <- iiw.weights(Surv(time.lag,time,event)~Wt + Apgar + 
#                    I(conc.lag>0 & conc.lag<=20) + 
#                 I(conc.lag>20 & conc.lag<=30) + I(conc.lag>30)+
#       cluster(Subject),id="Subject",time="time",event="event",data=data,
#       invariant=c("Subject","Wt","Apgar"),lagvars=c("time","conc"),maxfu=16*24,
#       lagfirst=c(0,0),first=FALSE)
# i$m


# Cousera_Assigment-3

# Part 1: Plot the 30-day mortality rates for heart attack
data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
data[,11] <- as.numeric(data[,11])
hist(data[,11], main="Heart Attack 30-day Death Rate", xlab="30-day Death Rate")

#Part 2:
data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
data[,11] <- as.numeric(data[,11])
data[,17] <- as.numeric(data[,17])
data[,23] <- as.numeric(data[,23])

#Histograms on the same plot window (3 rows, 1 column).
par(mfrow=c(3,1))

#Set x-axis range for all histograms.
xlim <- range(data[,11], data[,17], data[,23], na.rm=TRUE)

#Function returns histogram title and calculated mean death rate for that data outcome index.
main <- function(txt, outcome) {
    return (substitute(paste(txt, " (", bar(x), "=", m, ")"), list(m=round(mean(data[,outcome], na.rm=TRUE), 2), txt=txt)))
}

#Function plots a histogram with the main title, outcome index and x-axis label.
hplot <- function(main_title, outcome, xlab="30-day Death Rate") {
    hist(data[,outcome], xlim=xlim, xlab=xlab, main=main(main_title, outcome))
    abline(v=median(data[,outcome], na.rm=TRUE), lwd=3)
}

#Plot all histograms.
hplot("Heart Attack", 11)
hplot("Heart Failure", 17)
hplot("Pneumonia", 23)

#Part 3 : Ranking hospitals by outcome in a state
data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
data[,11] <- as.numeric(data[,11])

#Number of observations by state.
table(data[,11])

#Subset of data only considering states which contain twenty or more hospitals.
slice <- subset(data, State >= 20)

#Get slice data for death rates by state.
deaths <- slice[,11]
states <- slice$State

#Get states by death rate ordered by their median.
states_median <- with(slice, reorder(states, deaths, median, na.rm=TRUE))

#Set plot labels to be perpendicular to the axis.
par(las=2)

#Boxplot label values.
main_title <- "Heart Attack 30-day Death Rate by State"
y_label <- "30-day Death Rate"

#Boxplot of deaths rates by state.
boxplot(deaths ~ states, ylab=y_label, main=main_title)

#Boxplot median ordered death rates by state.
boxplot(deaths ~ states_median, data=states_median, ylab=y_label, main=main_title)

#Part4: Ranking hospital in all state 
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
hospital <- read.csv("hospital-data.csv", colClasses="character")

#Merge outcome and hospital datasets by provider number.
merged <- merge(outcome, hospital, by="Provider.Number")

#Coerce merged data.
deaths <- as.numeric(merged[,11])
patients <- as.numeric(merged[,15])
owners <- factor(merged$Hospital.Ownership)

#Reference lattice for xyplot.
library(lattice)

#Plot variables
x_label <- "Number of Patients Seen"
y_label <- "30-day Death Rate"
main_title <- "Heart Attack 30-day Death Rate by Ownership"

#XYPlot for relationship between death rate and number of patients seen with linear regression line.
xyplot(deaths ~ patients | owners, 
    allow.multiple=TRUE, 
    xlab=x_label, 
    ylab=y_label, 
    main=main_title,
    type=c("p", "r"))

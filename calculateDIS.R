#' Calculate Daily Impact Score
#'
#' @param fileName path to .csv file with column for counts, labels for each individual day, and time stamps for each count
#'
#' @param counts name of the counts column
#'
#' @param days name of the days column
#'
#' @param timestamp name of the time stamp column
#'
#' @param epoch the length of the epoch your data was collected with (in seconds)
#'
#' @return List including vector of DIS for each day of data as well as mean DIS for the days included in the file
#'
#' @importFrom dplyr count
#'
#' @export

calculateDIS <- function(fileName, counts = "counts", days = "days", timestamp = "TimeStamp", epoch = 15){

  fName <- fileName

  rawData <- read.csv(file = fName)

  uniqueDays <- unique(rawData[, days])

  allData <- data.frame(timestamp = c(rawData[, timestamp]))
  allData$counts <- rawData[, counts]
  allData$days <- rawData[, days]

  increment <- 1
  DIS <- c()


  for(i in uniqueDays){

    data <- subset(allData, allData$days == i)

    #hist(data$counts)

    # create bins
    breaks <- seq(0, 4000, by = 100)

    # tag counts into the above specified bins
    tags <- cut(data$counts, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = c(1:40))

    # create data frame with counts and the tag
    taggedData <- data.frame(data$counts, tags)

    # counts number of data points with each tag
    freqTag <- count(taggedData, tags, name = "freq")
    # omit na values
    freqTag <- na.omit(freqTag)

    # create column in data frame with midpoints of all the bins
    midpoints <- data.frame(tags = c(1:40), midpoint = seq(50, 3950, by = 100))

    # merge midpoints and freqTag based on matching value in "tags" column of both
    bData <- merge(freqTag, midpoints, by.x = "tags", by.y = "tags")

    DIS[increment] <- sum((bData$freq*(bData$midpoint^4))^(1/4))

    increment <- increment +1

  }

  meanDIS <- mean(DIS)

  dataDIS <- list(DIS, meanDIS)
  names(dataDIS) <- c("DIS", "Mean DIS")

  return(dataDIS)

}

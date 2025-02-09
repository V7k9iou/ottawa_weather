### Setting the working directory
setwd("/Users/davidjacques/Documents/Projects/ottawa_weather/")

### Loading libraries
library(pacman)
p_load(dplyr, lubridate, ggplot2)

### Creating folders if they're not there
dir.create(path = "raw_data", showWarnings = FALSE)
dir.create(path = "output", showWarnings = FALSE)


##################################################
### Getting historical climate data for Ottawa ###
##################################################
### Getting the first day of the month for the date range I want to cover
## Environment Canada will return a year's worth of data if you give it Jan 1
## as a parameter for the month and day
DatesClimateData <- seq.Date(
  from = as.Date("1935-01-01"),
  to = as.Date("2022-12-31"),
  by = "1 year"
)

### Checking if raw data file is in folder
RawDataFiles <- list.files(path = "./raw_data", pattern = "ClimateDataRaw.rds")

### Checking if raw data is in folder, downloading if it's not
if(length(RawDataFiles) == 1){
  ClimateDataRaw <- readRDS(file = "./raw_data/ClimateDataRaw.rds")
} else {
  ### An empty list to store downloaded data
  ClimateDataRaw <- list()

  ## Creating slots in list for each year
  for(i in 1:length(DatesClimateData)){
    ClimateDataRaw[[i]] <- NA
  }
  ## Giving each slot in the list the date as a name
  names(ClimateDataRaw) <- DatesClimateData

  ### Setting up progress bar
  pb = txtProgressBar(min = 0, max = length(DatesClimateData), initial = 0) 

  for(i in 1:length(DatesClimateData)){
    ### Setting station ID parameter
    ## You can get the station ID by looking at the URL of a station's historical
    ## data. Here's the URL for the OTTAWA CDA station with Climate ID 6105976
    ## https://climate.weather.gc.ca/climate_data/daily_data_e.html?timeframe=2&Year=2022&Month=4&Day=1&hlyRange=%7C&dlyRange=1889-11-01%7C2022-04-28&mlyRange=1889-01-01%7C2006-12-01&StationID=4333&Prov=ON&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2022&selRowPerPage=25&Line=7&searchMethod=contains&txtStationName=ottawa
    StationID <- 4333

    ### Setting up base URL with identifiable parameters that can be replaced
    ## The parameters are represented as ${stationID}, ${year}, ${month} and ${day}
    BaseURL <- "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=${stationID}&Year=${year}&Month=${month}&Day=${day}&timeframe=2&submit=Download+Data"

    ### Substituting parameters for date i
    URLDate_i <- gsub(x = BaseURL,   pattern = "\\$\\{stationID\\}", replacement = StationID)
    URLDate_i <- gsub(x = URLDate_i, pattern = "\\$\\{year\\}",  replacement = format(x = DatesClimateData[i], "%Y"))
    URLDate_i <- gsub(x = URLDate_i, pattern = "\\$\\{month\\}", replacement = format(x = DatesClimateData[i], "%m"))
    URLDate_i <- gsub(x = URLDate_i, pattern = "\\$\\{day\\}",   replacement = format(x = DatesClimateData[i], "%d"))


    ClimateDataRaw[[i]] <- read.csv(file = URLDate_i, header = TRUE)

    setTxtProgressBar(pb, i)
  }
  ### Cloing progress bar
  close(pb)

  saveRDS(object = ClimateDataRaw, file = "./raw_data/ClimateDataRaw.rds")
}



#####################
### Cleaning data ###
#####################

### Making all the variables that have flag in their name character variables.
## I want to combine my list of dataframes into one big dataframe with all my
## data. To do that the variables all need to be of the same time. Some of the
## variables with flag in their name or logical and others character. I'll make
## them all logical
for(i in 1:length(ClimateDataRaw)){
  PositionFlagColumns <- grep(names(ClimateDataRaw[[i]]), pattern = "Flag")

  ClimateDataRaw[[i]][ ,PositionFlagColumns] <- apply(
    X = ClimateDataRaw[[i]][ ,PositionFlagColumns], 
    MARGIN = 2, 
    FUN = function(OneColumn){as.logical(OneColumn)}
  )

}

### Combining the list of dataframes into one dataframe
ClimateData <- bind_rows(ClimateDataRaw)

### Cleaning up column names
names(ClimateData) <- gsub(x = names(ClimateData), pattern = "\\.", replacement = "")
names(ClimateData) <- gsub(x = names(ClimateData), pattern = "ï", replacement = "")
names(ClimateData) <- gsub(x = names(ClimateData), pattern = "Â", replacement = "")

## I'll get rid of the x and y letters at the end of the coordinate variables because they're ugly
names(ClimateData)[names(ClimateData) == "Longitudex"] <- "Longitude"
names(ClimateData)[names(ClimateData) == "Latitudey"]  <- "Latitude"

### Setting proper variable classes
ClimateData$DateTime <- ymd(ClimateData$DateTime)

### Finding columns with data
## There's no useful information in columns that are just full of NAs, so I'll  
## drop those columns
ColumnNotEmpty <- apply(
  X = ClimateData, 
  MARGIN = 2, 
  FUN = function(OneColumn){!all(is.na(OneColumn))}
)

### Keeping only columns with data
ClimateData <- ClimateData[ ,ColumnNotEmpty]



########################################
### Getting just data for the summer ###
########################################

### Subsetting for days on and between 21 June and 23 September
SummerDays <- c(
  which(ClimateData$Day %in% 21:30 & ClimateData$Month == 6),
  which(ClimateData$Month %in% 7:8),
  which(ClimateData$Day %in% 1:23 & ClimateData$Month == 9) 
)

ClimateSummerData <- ClimateData[SummerDays, ]

### Checking if there is data for Sep 23, 2022
if(is.na(ClimateSummerData$MeanTempC[ClimateSummerData$DateTime == "2022-09-23"])){
  message("2022-09-23 is missing mean temp data")
}



##############################################################
### Calculating average tempuratures in each year's summer ###
##############################################################
SummerMeanTemp <- aggregate(
  MeanTempC ~ Year,
  data = ClimateSummerData,
  FUN = mean
)

### Rouding to 1 significant figure because that's the data's precission
SummerMeanTemp$MeanTempC <- round(x = SummerMeanTemp$MeanTempC, digits = 1)

### Ranking mean tempuratures
## I have to add a minus to that the highest value ranks 1st
SummerMeanTemp$Rank <- rank(-SummerMeanTemp$MeanTempC, ties.method= "min")


### Sorting by mean temperature
SummerMeanTemp <- SummerMeanTemp[
  with(SummerMeanTemp, order(MeanTempC, decreasing = TRUE)),

]

### Resetting row numbers after sorting
row.names(SummerMeanTemp) <- NULL

### Writting out results
write.csv(x = SummerMeanTemp, file = "./output/Ottawa_summer_mean_temp.csv")

### Adding a variable to identify the most recent year
SummerMeanTemp$MostRecent <- NA
SummerMeanTemp$MostRecent[SummerMeanTemp$Year == max(SummerMeanTemp$Year)] <- "Most recent"

### A scatter plot to show mean summer temp in each year
ggplot(data = SummerMeanTemp, aes(x = Year, y = MeanTempC, colour = MostRecent)) +
geom_point() +
scale_colour_manual(values = c("Most recent" = "#FD001A")) +
theme_minimal() +
theme(legend.position = "none")

ggsave(file = "./output/Ottawa_summer_mean_temp.pdf")



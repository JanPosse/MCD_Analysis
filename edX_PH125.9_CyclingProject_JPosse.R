# Munich Cycling Data Analysis Project
# By Jan Posse - JanPosse@gmail.com
# Data provided by the city of Munich opendata.muenchen.de
# Source: dl-de/by-2-0: Landeshauptstadt München – opendata.muenchen.de

##########################################################
# Install necessary packages - if required 
##########################################################

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(scales)) install.packages("scales")
if(!require(ggmap)) install.packages("ggmap")
if(!require(rvest)) install.packages("rvest")
if(!require(timeDate)) install.packages("timeDate")
if(!require(rpart)) install.packages("rpart")
if(!require(rattle)) install.packages("rattle")
if(!require(ranger)) install.packages("ranger")
if(!require(randomForest)) install.packages("randomForest")

##########################################################
# Overview
##########################################################

# In this project I will analyze the open data the city of Munich is providing on bicycle traffic. 
# The city is running 6 stations counting bike traffic on bicycle lanes/ routes.
# Data is available on a daily level including additional information like weather and on a 15 minute interval. 
#
# I will load the data for the years up to 2019 and run several consistency checks. 
# I will then explore and visualize the data with the goal to create a data model to predict traffic 
# for a given day and time period. I will then test the model.
# 
# In a final step I will use the best model to predict the bicycle traffic for the year 2020 to 2022 and 
# compare it to the actual data also available. 
# The hypothesis is that the covid-pandemic had a significant impact on bike usage. 
# Most clearly during lockdowns, but also due to the rise of home office work in general. 

##########################################################
# Create data set and final_holdout_test set 
##########################################################

# The data is available as csv file from the opendata page of the city of munich. 
# There are 2 files per year one containing daily information and weather data and 
# one containing data on 15 minute intervals. I will download the data and join both 
# of the data sets (daily / 15 minutes) into one table.

# Note that data is available from as early as 2008 - however the 15 minutes intervals are only provided since 2017.
# Therefore the analysis on the 15 minutes interval is slightly limited.

# First I load the daily data
# The url for each year is

url_2008_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/53ef8c4b-d411-477f-9cf3-b044a4c1aaaa/download/rad_2008_tage_export_28_02_23_r.csv"
url_2009_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/89dbef6c-c6cb-4bfc-8729-93049e91223d/download/rad_2009_tage_export_28_02_23_r.csv"
url_2010_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/cb720004-bcde-4c0e-babf-64ee995c5232/download/rad_2010_tage_export_28_02_23_r.csv"
url_2011_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/8c752f92-2cb3-4765-aecf-bda349592ab2/download/rad_2011_tage_export_28_02_23_r.csv"
url_2012_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/7304e087-e02d-4ca1-b4da-5c46b27fa223/download/rad_2012_tage_export_28_02_23_r.csv"
url_2013_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/e281aac1-c476-48da-84ce-2bf52bf20a08/download/rad_2013_tage_export_28_02_23_r.csv"
url_2014_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/b78e7a05-33a5-422c-98fa-27c5cf9afe81/download/rad_2014_tage_export_28_02_23_r.csv"
url_2015_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/d5dd6fda-77ee-4ac7-9c7f-af54ccea64e4/download/rad_2015_tage_export_28_02_23_r.csv"
url_2016_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/2f9e99cf-e82d-41fb-990c-67783cf23ab7/download/rad_2016_tage_export_31_01_23_r.csv"
url_2017_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/d584bb5e-021b-43ea-8d32-8ba6c92b8f1e/download/rad_2017_tage_export_31_01_23_r.csv"
url_2018_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/00c5eaf9-d464-433f-8c9b-ce8a2a16db2b/download/rad_2018_tage_export_19_02_23_r.csv"
url_2019_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/f6d559cc-9e30-4307-b005-7b105b967ec0/download/rad_2019_tage_export_09_02_23_r.csv"
url_2020_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/8ae44963-171e-4edf-bf16-f12503717042/download/rad_2020_tage_export_09_02_23_r.csv"
url_2021_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/561fb0d5-2d27-41bb-bda9-a383d6d42ad1/download/rad_2021_tage_export_09_02_23_r.csv"
url_2022_Daily <- "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/05a2178d-3138-4874-a9fd-1ede6f0cedc1/download/rad_2022_tage_export_09_02_23_r.csv"

# To ensure proper data load I load each csv in a separate table

Daily2008 <- readr::read_csv(url_2008_Daily)
Daily2009 <- readr::read_csv(url_2009_Daily)
Daily2010 <- readr::read_csv(url_2010_Daily)
Daily2011 <- readr::read_csv(url_2011_Daily)
Daily2012 <- readr::read_csv(url_2012_Daily)
Daily2013 <- readr::read_csv(url_2013_Daily)
Daily2014 <- readr::read_csv(url_2014_Daily)
Daily2015 <- readr::read_csv(url_2015_Daily)
Daily2016 <- readr::read_csv(url_2016_Daily)
Daily2017 <- readr::read_csv(url_2017_Daily)
Daily2018 <- readr::read_csv(url_2018_Daily)
Daily2019 <- readr::read_csv(url_2019_Daily)
Daily2020 <- readr::read_csv(url_2020_Daily)
Daily2021 <- readr::read_csv(url_2021_Daily)
Daily2022 <- readr::read_csv(url_2022_Daily)

# I then combine the data for the years 2008 to 2019.
# I will union the tables using rbind.
# I perform two checks. 
# 1. Is the table structure the same across the tables?
# 2. Is the numbers of rows across the tables the same?

DataCheck <- c()
RowCount <- 0
for(i in 2008:2019){
  tblName <- paste0("Daily", i)
  DataCheck[i-2007] <- identical(str(Daily2008), str(get({{tblName}})))
  RowCount <- RowCount + nrow(get({{tblName}}))
}

Daily <- data.frame()
for(i in 2008:2019){
  tblName <- paste0("Daily", i)
  Daily <- rbind(Daily, (get({{tblName}})))
}

sum(DataCheck) == length(DataCheck)
RowCount == nrow(Daily)

# Both checks come back as TRUE
# I then combine the daily data for the years 2020 to 2022 with the same checks

DataCheck <- c()
RowCount <- 0
for(i in 2020:2022){
  tblName <- paste0("Daily", i)
  DataCheck[i-2019] <- identical(str(Daily2008), str(get({{tblName}})))
  RowCount <- RowCount + nrow(get({{tblName}}))
}

Daily_2020_2022 <- data.frame()
for(i in 2020:2022){
  tblName <- paste0("Daily", i)
  Daily_2020_2022 <- rbind(Daily_2020_2022, (get({{tblName}})))
}

sum(DataCheck) == length(DataCheck)
RowCount == nrow(Daily_2020_2022)

# Again both checks are successful

# Looking at the data we can see the following structure
str(Daily)

DataDictionary <- data.frame(ColumnName="datum", Type="Date", Description="Date of the measurement")
DataDictionary <- DataDictionary %>% add_row(ColumnName="uhrzeit_start", Type="'hms' num", Description="Time of the start of the measurement")
DataDictionary <- DataDictionary %>% add_row(ColumnName="uhrzeit_ende", Type="'hms' num", Description="Time of the end of the measurement")
DataDictionary <- DataDictionary %>% add_row(ColumnName="zaehlstelle", Type="chr", Description="Name of the position of the counting unit")
DataDictionary <- DataDictionary %>% add_row(ColumnName="richtung_1", Type="num", Description="Number of cyclist in the direction one")
DataDictionary <- DataDictionary %>% add_row(ColumnName="richtung_2", Type="num", Description="Number of cyclist in the direction two")
DataDictionary <- DataDictionary %>% add_row(ColumnName="gesamt", Type="num", Description="Toal number of cyclists")
DataDictionary <- DataDictionary %>% add_row(ColumnName="min.temp", Type="chr", Description="Minimum temperature on the day in degree Celsius")
DataDictionary <- DataDictionary %>% add_row(ColumnName="max.temp", Type="chr", Description="Maximum temperature on the day in degree Celsius")
DataDictionary <- DataDictionary %>% add_row(ColumnName="niederschlag", Type="chr", Description="Precipitation on the day measured in mm")
DataDictionary <- DataDictionary %>% add_row(ColumnName="bewoelkung", Type="num", Description="Cloud cover on the day in %")
DataDictionary <- DataDictionary %>% add_row(ColumnName="sonnenstunden", Type="chr", Description="Sunshine hours on the day")

# We see that some of the data has not been read in properly, e.g. the temperature was converted to a character type,
# but should be numeric. For better usage, I would like to convert all numeric columns that have been read in as character back 
# to numeric. Also I will change the location data to factor as there are only 6 locations in the data set.

# The transformation to numeric produces NAs so I will check and clean the data first
# A look at the NAs shows comma seperated numbers (the standard in Germany)
Daily$min.temp[is.na(as.numeric(Daily$min.temp))]
Daily$max.temp[is.na(as.numeric(Daily$max.temp))]
Daily$niederschlag[is.na(as.numeric(Daily$niederschlag))]
Daily$sonnenstunden[is.na(as.numeric(Daily$sonnenstunden))]

# I can replace them using the following code
# Notice that the same issue applies for the table Daily_2020_2022, but only in niederschlag and sonnenstunden

# Note that there might be negative temperatures (one or none "-"), one or two digits before the decimal and one after
Daily$min.temp <- Daily$min.temp %>% str_replace("^(-?\\d*),(\\d{1})", "\\1.\\2" )
Daily$max.temp <- Daily$max.temp %>% str_replace("^(-?\\d*),(\\d{1})", "\\1.\\2" )

# In very rare circumstances precipitation can be above 100 mm in one day and there is no negative precipitation. 
# So I will have to adjust the regex for this.
Daily$niederschlag <- Daily$niederschlag %>% str_replace("^(\\d+),(\\d{1})", "\\1.\\2" )
Daily_2020_2022$niederschlag <- Daily_2020_2022$niederschlag %>% str_replace("^(\\d+),(\\d{1})", "\\1.\\2" )

# The longest day in Munich (summer solstice) is less than 20 hours and there are no negative sunshine hours. 
# So the regex is slightly different.
Daily$sonnenstunden <- Daily$sonnenstunden %>% str_replace("^(1?\\d),(\\d{1})", "\\1.\\2" )
Daily_2020_2022$sonnenstunden <- Daily_2020_2022$sonnenstunden %>% str_replace("^(1?\\d),(\\d{1})", "\\1.\\2" )

# After replacing the comma we can now convert the columns to numerical
Daily$min.temp <- as.numeric(Daily$min.temp)
Daily$max.temp <- as.numeric(Daily$max.temp)
Daily$niederschlag <- as.numeric(Daily$niederschlag)
Daily$sonnenstunden <- as.numeric(Daily$sonnenstunden)
Daily_2020_2022$niederschlag <- as.numeric(Daily_2020_2022$niederschlag)
Daily_2020_2022$sonnenstunden <- as.numeric(Daily_2020_2022$sonnenstunden)

# The transformation of the locations works fine
Daily$zaehlstelle <- as.factor(Daily$zaehlstelle)
Daily_2020_2022$zaehlstelle <- as.factor(Daily_2020_2022$zaehlstelle)

# Looking at the summary, we notice that even more data seems inconsistent
# 1.  There are days with no total count (gesamt = 0)
# 2.  There are temperatures (min and max) above 100 degree celsius as
# 3.  There are maximum daily sunshine hours of 156
# 4.  There are 186 rows with missing weather data showing NAs (all weather data except cloud cover)

summary(Daily)

# Null counts

  NullCounts <- Daily %>% filter(gesamt==0) %>% group_by(year(datum), month(datum), zaehlstelle) %>% summarize(Count=n())
  sum(NullCounts$Count)
  sum(NullCounts%>%filter(Count>=5)%>%pull(Count))

# The only time I expect 0 counts would be in winter and even then only at selected locations.
# Removing all rows with 0 counts, which come from a location with 5 or more 0 counts in one month, 
# or that are not in winter

  # I will flag the rows I have to delete in the following table
  temp <- Daily %>% filter(gesamt==0) %>% group_by(year(datum), month(datum), zaehlstelle) %>% mutate(Delete = ifelse(n()>=5|`month(datum)` %in% c(4:10), 1, 0)) %>% ungroup()
  temp <- temp%>%select(datum, zaehlstelle, Delete)
  
  # I will do a join with the original table and filter the false 0 rows out and remove the Delete column
  Daily <- Daily %>% left_join(temp, by=c("datum", "zaehlstelle"))
  Daily <- Daily %>% filter(Delete!=1|is.na(Delete)) 
  Daily <- Daily %>% select(-Delete)

# When we filter the data for this cases, we can see 156 cases with odd temperatures.
# However, they fall only on 26 days (all in June 2019) and relate to all 6 locations (6 x 26 = 156)
Daily %>% filter(min.temp > 50) %>% summarize(Count=n())
Daily %>% filter(min.temp > 50) %>% group_by(datum) %>% summarize(Count=n())

# A look in the csv files reveals that the temperature for these days was wrongly formatted with a comma as decimal separator
# Somehow during the read of the csv file this seperator was lost. It seems all of June data is impacted, 
# however, only temperatures, that have decimals in the first place. 

# I can correct this data by dividing by 10.

Daily <- Daily %>% mutate(min.temp = ifelse(min.temp>50, min.temp/10, min.temp))
Daily <- Daily %>% mutate(max.temp = ifelse(max.temp>50, max.temp/10, max.temp))

# Looking at the outliers of sunshine hours - we discover that these are also all within June 2019. It seems to be the same error as with the temperature. 
Daily %>% filter(sonnenstunden > 17) %>% summarize(Count=n())
Daily %>% filter(sonnenstunden > 17) %>% group_by(datum) %>% summarize(Count=n())

# Again I can correct this data by dividing with 10.
# Luckily the minimum sunshine hours in June 2019 were 1.8 so we can easily correct this issue.

Daily <- Daily %>% mutate(sonnenstunden = ifelse(sonnenstunden>17, sonnenstunden/10, sonnenstunden))

summary(Daily)

# In the data set for 2020 to 2022 the same inconsistency is prevalent in 12/2020 und 08/2021
# Unfortunately identifying the values in December is slightly more complicated.

Daily_2020_2022 <- Daily_2020_2022 %>%  
  mutate(min.temp = ifelse(year(datum)==2020 & month(datum)==12 & min.temp != 1 & min.temp != -1 & min.temp != -3, min.temp/10, min.temp))
Daily_2020_2022 <- Daily_2020_2022 %>%  
  mutate(max.temp = ifelse(year(datum)==2020 & month(datum)==12 & max.temp != 5 & max.temp != 6, max.temp/10, max.temp))
Daily_2020_2022 <- Daily_2020_2022 %>% 
  mutate(sonnenstunden = ifelse(year(datum)==2020 & month(datum)==12 & sonnenstunden!=1 & sonnenstunden!=2, sonnenstunden/10, sonnenstunden))


Daily_2020_2022 <- Daily_2020_2022 %>% mutate(min.temp = ifelse(min.temp>50, min.temp/10, min.temp))
Daily_2020_2022 <- Daily_2020_2022 %>% mutate(max.temp = ifelse(max.temp>50, max.temp/10, max.temp))
Daily_2020_2022 <- Daily_2020_2022 %>% 
  mutate(sonnenstunden = ifelse(year(datum)==2021 & month(datum)==08 & sonnenstunden!=1 & sonnenstunden!=9 & sonnenstunden!=11, sonnenstunden/10, sonnenstunden))


# In the summary we can spot 186 NAs in the weather data. Using is.na we can identify 
# that the month of December in the year 2018 is missing weather data for min and max temperature, 
# precipitation and sunshine (31 days x 6 locations = 186 NAs).

Daily %>% group_by(datum) %>% summarize(NAs = sum(is.na(niederschlag))) %>% filter(NAs > 0)

# As it is unclear what the precise source of the weather data is, 
# the data for December 2018 cannot be used for modeling. I will hence remove the month from the data set.

Daily <- Daily %>% filter (!(year(datum)==2018 & month(datum)==12))

# Final logic check on the date
# As we know the data is daily start and end time should be the same for all rows.
Daily %>% group_by(as.factor(uhrzeit_start)) %>% summarize(Count=n())
Daily %>% group_by(as.factor(uhrzeit_ende)) %>% summarize(Count=n())

#We see that there are two end times, I already noticed that earlier, when looking at the CSV files. 
# However it does look like another separator issue and since we know it is daily data, I will ignore 
# start and end date in the analysis.

# We expect for each year 365 days (or 366 days)

tab <- Daily %>% group_by(zaehlstelle, Yr = year(datum)) %>% summarize(Count = n()) 
xtabs(Count ~ zaehlstelle + Yr, data=tab)

# And we do see 365 or 366 years for most location and years, with two major exceptions. The first year of each location. 
# And for 2019 for the location Arnulf. A check with the original CSV shows that there are also there only 334 lines.
# There is no data for July 2019 (31 days). 

### External effects
# From the open data page we also get an information on two external effects.
# 1. At the location Arnulf is a construction site influencing the numbers since early 2021.
# 2. The location Kreuther has been upgraded in April 2020 doubling the width of the counting loop in the ground. 
#
# Both effects do not effect my base data. Nonetheless we should exclude this data for my planned comparison.

Daily_2020_2022 <- Daily_2020_2022 %>% filter(!(zaehlstelle == "Arnulf"&year(datum) >= 2021))
Daily_2020_2022 <- Daily_2020_2022 %>% filter(!(zaehlstelle == "Kreuther"&datum > "2020-03-31"))


# For Detailed analysis I would like to add school holidays and public holidays
# Unfortunately there is no open data for school holidays in Germany.
# However I found the webpage https://kalender-365.de that list school holidays for each year 
# The dates are on separate pages that are named like https://kalender-365.de/schulferien.php?yy=2007 
# The data is then stored in the same table and structure, so that I can use data scraping to collect the holidays.

# First I set up a table to store the data and define the groups
  holidays_timeframe <- data.frame(matrix(ncol = 3, nrow =0))
  colnames(holidays_timeframe) <- c("Timeframe", "year", "name")
  holidays_timeframe$Timeframe <- as.character(holidays_timeframe$Timeframe)
  holidays_timeframe$year <- as.numeric(holidays_timeframe$year)
  holidays_timeframe$name <- as.character(holidays_timeframe$name)
  holidays_name <- c("Herbstferien", "Weihnachtsferien", "Winterferien", "Osterferien", "Pfingstferien", "Sommerferien")

  
for(j in 2008:2022){
  url <- paste0("https://kalender-365.de/schulferien.php?yy=", j)
  html <- read_html(url)
  table <- html_nodes(html, "table")[[1]]
  rows <- html_nodes(table, "tr")
  row <- rows[-1]
  
  for(i in 7:12){
    cols <- html_nodes(row, "td")[[i]]
    holidays_timeframe <- holidays_timeframe %>% add_row(Timeframe = html_text(cols), year = j, name = holidays_name[i-6])
  }
}

# Now we just have to split out the date
  
  holidays_timeframe <- holidays_timeframe %>% mutate(holiday_start = as.Date(str_sub(Timeframe, 1, 8), format = "%d.%m.%y"))
  holidays_timeframe <- holidays_timeframe %>% mutate(holiday_end = as.Date(str_sub(Timeframe, 10, 18), format = "%d.%m.%y"))

# There is one NA in my table, which is correct as there were no "Winterferien" in 2013
# I will remove this row.

  holidays_timeframe <- holidays_timeframe[complete.cases(holidays_timeframe),]

# I will now join the holidays with my daily data

  # First I will extract all dates from Daily and Daily_2020_2022
    
    Days <- Daily %>% distinct(datum)
    Days_2020_2022 <- Daily_2020_2022 %>% distinct(datum)
    
  # Then I will do a cross join with the school holiday table and check if the date falls into any of the holiday ranges
  
    temp <- Days %>% cross_join(holidays_timeframe) %>% mutate(school_holiday = between(datum, holiday_start, holiday_end))
    temp2 <- Days_2020_2022 %>% cross_join(holidays_timeframe) %>% mutate(school_holiday = between(datum, holiday_start, holiday_end))
    
  # By grouping this table and summing the logical column, I have a daily holiday table, I can now join to my original data
    
    temp <- temp %>% group_by(datum) %>% summarize(school_holiday = as.logical(sum(school_holiday)))
    temp2 <- temp2 %>% group_by(datum) %>% summarize(school_holiday = as.logical(sum(school_holiday)))
  
    Daily <- left_join(Daily, temp %>% select (datum, school_holiday), by = "datum")
    Daily_2020_2022 <- left_join(Daily_2020_2022, temp2 %>% select (datum, school_holiday), by = "datum")

# Now for the public holidays
# There is a set of fixed holidays, which are easy to set up.
# For the flexible public holidays like Easter I use the function 'holiday'
# I will create a table of public holidays and left-join it with my Daily data. 
# All rows with NA are then no public holidays.
    
  # First I set up the table
    public_holidays <- data.frame(matrix(ncol = 2, nrow =0))
    colnames(public_holidays) <- c("datum", "name")
    public_holidays$datum <- as.Date(public_holidays$datum)
    public_holidays$name <- as.character(public_holidays$name)
    
  # Then I add the public holidays
    for(i in 2008:2022){
      hdate <- paste0(i, "-01-01")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(hdate), name = "New Years Day")
      hdate <- paste0(i, "-01-06")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(hdate), name = "Epiphany")
      hdate <- paste0(i, "-05-01")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(hdate), name = "Labour Day")
      hdate <- paste0(i, "-08-15")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(hdate), name = "Assumption of Mary")
      hdate <- paste0(i, "-10-03")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(hdate), name = "Unification Day")
      hdate <- paste0(i, "-11-01")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(hdate), name = "All Saints Day")
      hdate <- paste0(i,"-12-25")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(hdate), name = "Christmas Day")
      hdate <- paste0(i, "-12-26")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(hdate), name = "Boxing Day")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(holiday(i, "GoodFriday")), name = "Good Friday")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(holiday(i, "EasterMonday")), name = "Easter Monday")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(holiday(i, "Ascension")), name = "Ascension Day")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(holiday(i, "PentecostMonday")), name = "Pentecost Monday")
      public_holidays <- public_holidays %>% add_row(datum = as.Date(holiday(i, "CorpusChristi")), name = "Corpus Christi")
    }
    
    # Now I can join the public holidays to my Daily data sets any NAs are no public holidays
    Daily <- left_join(Daily, public_holidays, by = "datum")
    Daily_2020_2022 <- left_join(Daily_2020_2022, public_holidays, by = "datum")
    
    Daily <- Daily %>% mutate(public_holiday = !is.na(name)) %>% select(-name)
    Daily_2020_2022 <- Daily_2020_2022 %>% mutate(public_holiday = !is.na(name)) %>% select(-name)

  # Weekday and spread of year, month, day, year-month, and month-day
    
    # To ensure I will get the English abbreviations I change the locale to English
    Sys.setlocale(category = "LC_ALL", locale = "English")
    
    # I will also add the weekday
    
    Daily <- Daily %>% mutate(weekday = wday(datum, label=TRUE))
    Daily_2020_2022 <- Daily_2020_2022 %>% mutate(weekday = wday(datum, label=TRUE))
    
    # And spread out the year, month, day, year-month, and month-day
    
    Daily <- Daily %>% mutate(year = year(datum))
    Daily_2020_2022 <- Daily_2020_2022 %>% mutate(year = year(datum))
    Daily <- Daily %>% mutate(month = month(datum, label=TRUE))
    Daily_2020_2022 <- Daily_2020_2022 %>% mutate(month = month(datum, label=TRUE))
    Daily <- Daily %>% mutate(day = day(datum))
    Daily_2020_2022 <- Daily_2020_2022 %>% mutate(day = day(datum))
    Daily <- Daily %>% mutate(yearmonth = (year(datum))*100+(month(datum)))
    Daily_2020_2022 <- Daily_2020_2022 %>% mutate(yearmonth = year(datum)*100+(month(datum)))
    Daily <- Daily %>% mutate(monthday = month(datum)*100 + day(datum))
    Daily_2020_2022 <- Daily_2020_2022 %>% mutate(monthday = month(datum)*100 + day(datum))

##########################################################
# Explore the data - visualizing  
##########################################################

# First I have a look at the location of the counting units    
                
  #url <- "https://opendata.muenchen.de/dataset/aca4bcb6-d0ff-4634-b5b9-8b5d133ab08e/resource/211e882d-fadd-468a-bf8a-0014ae65a393/download/radzaehlstellen.csv"
  #file <- readr::read_csv(url)
  file <- "radzaehlstellen.csv"

UnitPosition <- readr::read_csv(file)
UnitPosition$richtung_1 <- UnitPosition$richtung_1 %>% str_replace("Süd", "South")
UnitPosition$richtung_1 <- UnitPosition$richtung_1 %>% str_replace("Ost", "East")
UnitPosition$richtung_1 <- UnitPosition$richtung_1 %>% str_replace("Nord", "North")


MunichMap <- get_stamenmap(bbox = c(11.4300, 48.0500, 11.7300, 48.2500), zoom = 12, maptype = "terrain")
ggmap(MunichMap) + geom_point(data = UnitPosition, aes(x = longitude, y = latitude), size = 4) + 
  geom_text(data = UnitPosition, aes(x = longitude, y = latitude, label = zaehlstelle), size=4, hjust=1, vjust = 1.2) +
  geom_text(data = UnitPosition, aes(x = longitude, y = latitude, label = paste0("(", richtung_1,")")), size=4, hjust=1, vjust = 2.2) +
  labs(title="Position of the cyclist counting units [2]") + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())

# Then I will have a look at the count by location and direction

temp1 <- select(Daily, datum, zaehlstelle, richtung_1) %>% mutate(Cyclists = richtung_1) %>% select(-richtung_1) %>% mutate(subgroup = "Direction 1")
temp2 <- select(Daily, datum, zaehlstelle, richtung_2) %>% mutate(Cyclists = richtung_2) %>% select(-richtung_2) %>% mutate(subgroup = "Direction 2")
temp <- bind_rows(temp1, temp2) 
  
temp %>% ggplot(aes(x = zaehlstelle, y = Cyclists, fill = subgroup)) + geom_boxplot() + labs(x= "Location", y = "Daily count of cyclists", fill="Direction")+
  scale_y_continuous(labels = comma)

# There seems to be a strong relation between the directions, except for the Loaction Arnulf

Daily %>% filter(zaehlstelle != "Arnulf") %>% ggplot(aes(x= richtung_1, y=richtung_2)) + geom_point() + geom_smooth(method="lm") +
  scale_y_continuous(limits = c(0, 6000), labels = comma)+scale_x_continuous(limits = c(0, 6000), labels = comma)

# Monthly data by year and location

Daily %>% group_by(year, month, zaehlstelle) %>% summarize(Cyclists=(sum(richtung_1)+sum(richtung_2))/n()) %>%
  ggplot(aes(x=month, y=Cyclists))+geom_col()+facet_grid(zaehlstelle~year)

# Cyclists per month
# When reviewing the monthly data - looking for trends in a year it makes sense to look only at locations, 
# which have values for every month in a year - i will create a logical yearcomp column to filter for these locations.

  fullyear <- Daily %>% group_by(zaehlstelle, year) %>% summarize(n= n_distinct(month))%>%mutate(fullyear = if_else(n==12, TRUE, FALSE)) %>% select(-n)
  Daily <- left_join(Daily, fullyear, by=c("zaehlstelle", "year"))
  
  Daily %>% filter(fullyear==TRUE) %>% group_by(month) %>% 
    summarize(Cyclists=(sum(richtung_1)+sum(richtung_2))/(n_distinct(zaehlstelle)+n_distinct(year)))%>%
    ggplot(aes(x=month, y=Cyclists))+geom_col()+
    scale_y_continuous(limits = c(0, 300000), labels = label_number(suffix = "k", scale = 1e-3))

# Weather
  # Remember that there are NAs in the weather data so we have to use the option na.rm=TRUE to calculate the mean
  temp <- Daily %>% filter(fullyear==TRUE) %>% select(datum, min.temp, max.temp, niederschlag, bewoelkung, sonnenstunden) %>%distinct()%>%
    mutate(month=month(datum)) %>% group_by(month)%>%
    summarize(AvgMinTemp = mean(min.temp, na.rm=TRUE), AvgMaxTemp = mean(max.temp, na.rm=TRUE), AvgPrecipitation = mean(niederschlag, na.rm=TRUE), AvgCloudCover = mean(bewoelkung, na.rm=TRUE), AvgSunshine = mean(sonnenstunden, na.rm=TRUE))
  temp_long <- temp %>% pivot_longer(!month, names_to = "weatherdata", values_to = "average")
  temp_long %>% ggplot()+geom_line(aes(x = month, y = average, color=weatherdata)) + 
    labs(title="Average monthly weather data for Munich")+ 
    scale_x_continuous(breaks=c(1:12), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  #Relationship rain and cyclists
  weather01 <- Daily %>% group_by(datum) %>% 
    summarize(Precipitation=mean(niederschlag, na.rm = TRUE), Cyclists = sum(gesamt)/n_distinct(zaehlstelle))%>%
    ggplot(aes(x=Precipitation, y= Cyclists)) + geom_point() + geom_smooth(method="lm") +  
    scale_y_continuous(limits = c(0, 6000), labels = comma)+
    scale_x_continuous(limits = c(0, 60)) + 
    labs(title="Graph 6: Average Cyclists per location and day and Precipitation")
  
  #Relationship temperature and cyclists
  weather02 <- Daily %>% group_by(datum) %>% 
    summarize(AvgMinTemp=mean(min.temp, na.rm = TRUE), Cyclists = sum(gesamt)/n_distinct(zaehlstelle))%>%
    ggplot(aes(x=AvgMinTemp, y= Cyclists)) + geom_point() + geom_smooth(method="lm") + 
    scale_y_continuous(limits = c(0, 6000), labels = comma)+
    scale_x_continuous(limits = c(-25, 25)) + 
    labs(title="Graph 7: Average Cyclists per location and day and Minimum Temperature")
  
  weather03 <- Daily %>% group_by(datum) %>% 
    summarize(AvgMaxTemp=mean(max.temp, na.rm = TRUE), Cyclists = sum(gesamt)/n_distinct(zaehlstelle))%>%
    ggplot(aes(x=AvgMaxTemp, y= Cyclists)) + geom_point() + geom_smooth(method="lm") +  
    scale_y_continuous(limits = c(0, 6000), labels = comma)+
    scale_x_continuous(limits = c(-10, 40)) + 
    labs(title="Graph 8: Average Cyclists per location and day and Maximum Temperature")

  #Relationship sunshine and cyclists
  weather04 <- Daily %>% group_by(datum) %>% 
    summarize(SunshineHours=mean(sonnenstunden, na.rm = TRUE), Cyclists = sum(gesamt)/n_distinct(zaehlstelle))%>%
    ggplot(aes(x=SunshineHours, y= Cyclists)) + geom_point() +  geom_smooth(method="lm") + 
    scale_y_continuous(limits = c(0, 6000), labels = comma)+
    scale_x_continuous(limits = c(0, 17)) + 
    labs(title="Graph 9: Average Cyclists per location and day and Sunshine Hours")
  
  #Relationship cloud cover and cyclists
  weather05 <- Daily %>% group_by(datum) %>% 
    summarize(CloudCover=mean(bewoelkung, na.rm = TRUE), Cyclists = sum(gesamt)/n_distinct(zaehlstelle))%>%
    ggplot(aes(x=CloudCover, y= Cyclists)) + geom_point() +  geom_smooth(method="lm") + 
    scale_y_continuous(limits = c(0, 6000), labels = comma)+
    scale_x_continuous(limits = c(0, 100)) + 
    labs(title="Graph 10: Average Cyclists per location and day and Cloud Coverage")
  
  grid.arrange(weather01, weather02, weather03, weather04, weather05)
  
  # Reviewing impact of holidays
  
  Daily %>% ggplot(aes(x = weekday, y = gesamt, fill=public_holiday)) + geom_boxplot()+ 
    scale_y_continuous(limits = c(0, 16000), labels = comma)+
    labs(title="Graph 11: Total Cyclists per location and day by Weekday and Public holiday", x="Weekday", y="Total Cyclists per day and location")
  
  Daily %>% ggplot(aes(x = weekday, y = gesamt, fill=school_holiday)) + geom_boxplot()+ 
    scale_y_continuous(limits = c(0, 16000), labels = comma)+
    labs(title="Graph 12: Total Cyclists per location and day by Weekday and School holiday", x="Weekday", y="Total Cyclists per day and location")  
  
##########################################################
# Set up the data models  
##########################################################
# Excluding one full year and selecting the relevant columns
    
    set.seed(1982) # by setting the seed we generate reproducible "random" events
    yearexcluded <- sample(2008:2019, 1)
      
    temp <- Daily %>% filter(year!=yearexcluded) %>% select(zaehlstelle, min.temp, max.temp, niederschlag, bewoelkung, sonnenstunden, year, month, weekday, yearmonth, monthday, school_holiday, public_holiday, gesamt)
    
# creating the training and test_set
  
    # Before creating the training and test_set, I will remove all rows with no total value
  
    set.seed(1982)
    test_index <- createDataPartition(temp$gesamt, time = 1, p = 0.9, list = FALSE)
  
    Daily_train <- temp[test_index, ]
    Daily_test <- temp[-test_index, ]
    
  
# I will measure the model using RMSE and MAPE
    RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
    }
  
    MAPE <- function(true_ratings, predicted_ratings){
      mean(abs((true_ratings-predicted_ratings)/true_ratings))*100
    }

    # For Mape test data I will filter out rows with a total value of 0
    
    Daily_test_MAPE <- Daily_test %>% filter(gesamt!=0)
    
# Mean as basic approach
    
    avg <- mean(Daily_train$gesamt)
    RMSE(Daily_test$gesamt, avg)
    MAPE(Daily_test_MAPE$gesamt, avg)

    Results <- tibble(Method = "Mean", RMSE = RMSE(Daily_test$gesamt, avg), MAPE = MAPE(Daily_test_MAPE$gesamt, avg))
    
# Mean + location
    
    location_avg <- Daily_train %>% group_by(zaehlstelle) %>% summarize(b_l = mean(gesamt - avg)) 
    
    prediction <- avg + Daily_test %>% left_join(location_avg, by='zaehlstelle') %>% pull(b_l)
    predictionMAPE <- avg + Daily_test_MAPE %>% left_join(location_avg, by='zaehlstelle') %>% pull(b_l) 

    RMSE(Daily_test$gesamt, prediction)
    MAPE(Daily_test_MAPE$gesamt, predictionMAPE)
    Results <- Results %>% add_row(Method = "Mean + Location", RMSE = RMSE(Daily_test$gesamt, prediction), MAPE=MAPE(Daily_test_MAPE$gesamt, predictionMAPE))

# Mean + location + Month
    
    month_avg <- Daily_test %>% left_join(location_avg, by='zaehlstelle') %>% 
      group_by(month) %>% summarize(b_m = mean(gesamt - avg - b_l))
    
    prediction <- Daily_test %>% left_join(location_avg, by='zaehlstelle') %>%
      left_join(month_avg, by='month') %>% mutate(pred = avg + b_l + b_m) %>% pull(pred)
    
    predictionMAPE <- Daily_test_MAPE %>% left_join(location_avg, by='zaehlstelle') %>%
      left_join(month_avg, by='month') %>% mutate(pred = avg + b_l + b_m) %>% pull(pred)
    
    Results <- Results %>% add_row(Method = "Mean + Loc + Month", RMSE = RMSE(Daily_test$gesamt, prediction),
                                   MAPE = MAPE(Daily_test_MAPE$gesamt, predictionMAPE))
# Mean + location + Month + temperature

      temperature_avg <- Daily_test %>% 
      left_join(location_avg, by='zaehlstelle') %>% 
      left_join(month_avg, by='month') %>% 
      group_by(min.temp) %>% summarize(b_t = mean(gesamt - avg - b_l - b_m))
    
    # As I did not check if all minimum temperatures in the test set are also present in the training set
    # I have to check for NAs. For unknown temperatures I set the effect to 0 by replacing NAs with 0.
    
    prediction <- Daily_test %>% 
      left_join(location_avg, by='zaehlstelle') %>%
      left_join(month_avg, by='month')%>%
      left_join(temperature_avg, by='min.temp') %>% mutate(b_t = replace_na(b_t, 0)) %>%
      mutate(pred = avg + b_l + b_m + b_t) %>% pull(pred)
    
    predictionMAPE <- Daily_test_MAPE %>% left_join(location_avg, by='zaehlstelle') %>%
      left_join(month_avg, by='month')%>%
      left_join(temperature_avg, by='min.temp') %>% mutate(b_t = replace_na(b_t, 0)) %>%
      mutate(pred = avg + b_l + b_m + b_t) %>% pull(pred)
    
    Results <- Results %>% add_row(Method = "Mean + Loc + Month + Temp", 
                                   RMSE = RMSE(Daily_test$gesamt, prediction),
                                   MAPE = MAPE(Daily_test_MAPE$gesamt, predictionMAPE))

# Mean + location + Month + temperature + weekday 

    weekday_avg <- Daily_test %>% 
      left_join(location_avg, by='zaehlstelle') %>% 
      left_join(month_avg, by='month') %>% 
      left_join(temperature_avg, by='min.temp') %>% mutate(b_t = replace_na(b_t, 0)) %>%
      group_by(weekday) %>% summarize(b_wdy = mean(gesamt - avg - b_l - b_m - b_t))
    
    prediction <- Daily_test %>% left_join(location_avg, by='zaehlstelle') %>%
      left_join(month_avg, by='month')%>%
      left_join(temperature_avg, by='min.temp') %>% mutate(b_t = replace_na(b_t, 0)) %>%
      left_join(weekday_avg, by='weekday') %>%
      mutate(pred = avg + b_l + b_m + b_t + b_wdy) %>% pull(pred)
    
    predictionMAPE <- Daily_test_MAPE %>% left_join(location_avg, by='zaehlstelle') %>%
      left_join(month_avg, by='month')%>%
      left_join(temperature_avg, by='min.temp') %>% mutate(b_t = replace_na(b_t, 0)) %>%
      left_join(weekday_avg, by='weekday') %>%
      mutate(pred = avg + b_l + b_m + b_t + b_wdy) %>% pull(pred)
    
    Results <- Results %>% add_row(Method = "Mean + Loc + Month + Temp + Wday", 
                                   RMSE = RMSE(Daily_test$gesamt, prediction),
                                   MAPE = MAPE(Daily_test_MAPE$gesamt, predictionMAPE))

# Mean + location + Month + temperature + weekday + school holiday

    schoolholiday_avg <- Daily_test %>% 
      left_join(location_avg, by='zaehlstelle') %>% 
      left_join(month_avg, by='month') %>% 
      left_join(temperature_avg, by='min.temp') %>% mutate(b_t = replace_na(b_t, 0)) %>%
      left_join(weekday_avg, by='weekday') %>%  
      group_by(school_holiday) %>% summarize(b_shl = mean(gesamt - avg - b_l - b_m - b_t - b_wdy))
    
    prediction <- Daily_test %>% 
      left_join(location_avg, by='zaehlstelle') %>%
      left_join(month_avg, by='month')%>%
      left_join(temperature_avg, by='min.temp') %>% mutate(b_t = replace_na(b_t, 0)) %>%
      left_join(weekday_avg, by='weekday') %>%
      left_join(schoolholiday_avg, by='school_holiday') %>%
    mutate(pred = avg + b_l + b_m + b_t + b_wdy + b_shl) %>% pull(pred)
    
    predictionMAPE <- Daily_test_MAPE %>% 
      left_join(location_avg, by='zaehlstelle') %>%
      left_join(month_avg, by='month')%>%
      left_join(temperature_avg, by='min.temp') %>% mutate(b_t = replace_na(b_t, 0)) %>%
      left_join(weekday_avg, by='weekday') %>%
      left_join(schoolholiday_avg, by='school_holiday') %>% 
    mutate(pred = avg + b_l + b_m + b_t + b_wdy + b_shl) %>% pull(pred)
    
    Results <- Results %>% add_row(Method = "Mean + Loc + Month + Temp + Wday + SchoolHld", 
                                   RMSE = RMSE(Daily_test$gesamt, prediction),
                                   MAPE = MAPE(Daily_test_MAPE$gesamt, predictionMAPE))  
    
# Quick review of initial results
    
    Results
        
# We can see a decreasing RMSE but an increase in the MAPE after adding the weekday. The MAPE as an indicator has several shortcomings, 
# for example underforecasts are treated better than overforecasts [7]. 
# I will focus on optimizing RMSE and consider the MAPE just as an way to explain the RMSE
  
# Regression Tree
    
  RegressionTree <- train(gesamt~ ., method="rpart", tuneGrid = data.frame(cp = seq(0, 0.01, len =25)), data=Daily_train)
  pruned_RegressionTree <- prune(RegressionTree$finalModel, cp = 0.007)
  
  fancyRpartPlot(pruned_RegressionTree,yesno=2,split.col="black", main = "Regression Tree visualization", caption="",branch.col="black")
  
  prediction <- predict(RegressionTree, Daily_test)
  predictionMAPE <- predict(RegressionTree, Daily_test_MAPE)
  
  Results <- Results %>% add_row(Method = "RegressionTree", 
                                 RMSE = RMSE(Daily_test$gesamt, prediction),
                                 MAPE = MAPE(Daily_test_MAPE$gesamt, predictionMAPE))

# Random Forest
  
  # Random Forest/ Ranger optimization
  
  # Creating a grid with the parameters to test
  Optimization_grid <- expand_grid(
    mtry = c(1:13),
    min.node.size = c(1, 3, 5, 10, 20), 
    rmse = NA)
  
  # Testing the parameters on a small random forest
  for(i in seq_len(nrow(Optimization_grid))) {
    TestForest <- ranger(
      formula         = gesamt ~ ., 
      data            = Daily_train, 
      num.trees       = 150,
      mtry            = Optimization_grid$mtry[i],
      min.node.size   = Optimization_grid$min.node.size[i],
      seed            = 1982,
      respect.unordered.factors = 'order')
    Optimization_grid$rmse[i] <- sqrt(TestForest$prediction.error)
  }
  
  Optimization_grid %>% ggplot(aes(y=rmse, x=mtry))+
    geom_point()+
    labs(title = "graph 14: Impact of mtry on the model rmse")
  
  Optimization_grid %>% filter(mtry==10) %>% ggplot(aes(y=rmse, x=min.node.size))+
    geom_point() +
    labs(title = "graph 15: Impact of the minimum node size on the model rmse")

  # Calculating the final model
  
  train_rf <- ranger(gesamt ~ ., data=Daily_train, num.tree=300, mtry = 10, replace=TRUE, min.node.size= 5, sample.fraction = 1, seed=1982, importance = "permutation")
  
  prediction <- predictions(predict(train_rf, data=Daily_test))
  predictionMAPE <- predictions(predict(train_rf, data=Daily_test_MAPE))
  
  Results <- Results %>% add_row(Method = "RandomForest", 
                                 RMSE = RMSE(Daily_test$gesamt, prediction),
                                 MAPE = MAPE(Daily_test_MAPE$gesamt, predictionMAPE))
  
  Results

  # Reviewing the variable importance
  
  variableImportance <- importance_pvalues(train_rf, formula = gesamt~., data=Daily_train, method = "altmann")

  variableImportance %>% as.data.frame() %>%
    rownames_to_column() %>% 
    arrange(importance) %>%
    mutate(rowname = fct_inorder(rowname)) %>%
    ggplot()+
    geom_col(aes(x = rowname, y = importance))+
    coord_flip()+  
    labs(y="Variable importance overall", x="Variable", title = "graph 15: Variable importance of the final model")+
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))
     
 #train_rf_final <- randomForest( gesamt~., data=Daily_train, ntree=150, mtry= 46)  


# Visual review of results  
    
  GraphData <- Daily %>% select(zaehlstelle, min.temp, max.temp, niederschlag, bewoelkung, sonnenstunden, year, month, weekday, yearmonth, monthday, school_holiday, public_holiday, gesamt)
  GraphData <- GraphData %>% mutate(prediction = predictions(predict(train_rf, data=GraphData)))

  GraphData %>% filter(year==2014, zaehlstelle=="Erhardt") %>% group_by(month) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction))%>%
    ggplot(aes(x=month)) + 
    geom_col(aes(y=actual, fill ="Actual"), width=0.4, position=position_nudge(x = 0.2, y = 0)) + 
    geom_col(aes(y=prediction, fill = "Prediction"),width=0.4, position=position_nudge(x = -0.2, y = 0)) +
    scale_fill_manual(values=c("royalblue4", "darksalmon"), name="Values")+
    labs(x="Month", y="Total cyclist counted", title = "graph 17: Actual and Predicted Cyclist Count for location Erhardt and 2014")+
    scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3))
  
  GraphData %>% filter(year==2018, zaehlstelle=="Erhardt") %>% group_by(month) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction))%>%
    ggplot(aes(x=month)) + 
    geom_col(aes(y=actual, fill ="Actual"), width=0.4, position=position_nudge(x = 0.2, y = 0)) + 
    geom_col(aes(y=prediction, fill = "Prediction"),width=0.4, position=position_nudge(x = -0.2, y = 0)) +
    scale_fill_manual(values=c("royalblue4", "darksalmon"), name="Values")+
    labs(x="Month", y="Total cyclist counted", title = "graph 18: Actual and Predicted Cyclist Count for location Erhardt and 2014")+
    scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3))
  
  GraphData %>% group_by(year) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction))%>%
    ggplot(aes(x=year)) + 
    geom_col(aes(y=actual, fill ="Actual"), width=0.4, position=position_nudge(x = 0.2, y = 0)) + 
    geom_col(aes(y=prediction, fill = "Prediction"),width=0.4, position=position_nudge(x = -0.2, y = 0)) +
    scale_fill_manual(values=c("royalblue4", "darksalmon"), name="Values")+
    labs(x="Year", y="Total cyclist counted", title = "graph 19: Predicted and actual Cyclist Count per year") +
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))+
    scale_x_continuous(breaks=c(2008:2019))

  GraphData %>% group_by(year) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction), error=sum(gesamt)-sum(prediction), errorPerc = (sum(gesamt)-sum(prediction))/sum(gesamt))
  
##########################################################
# Bicycle traffic 2020 to 2022 
##########################################################  
    
  Graph_2020 <- Daily_2020_2022 %>% select(zaehlstelle, min.temp, max.temp, niederschlag, bewoelkung, sonnenstunden, year, month, weekday, yearmonth, monthday, school_holiday, public_holiday, gesamt)
  Graph_2020 <- Graph_2020 %>% mutate(prediction = predictions(predict(train_rf, data=Graph_2020)))
  
  Graph_2020 %>% filter(year==2020) %>% group_by(month) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction))%>%
    ggplot(aes(x=month)) + 
    geom_col(aes(y=actual, fill ="Actual"), width=0.4, position=position_nudge(x = 0.2, y = 0)) + 
    geom_col(aes(y=prediction, fill = "Prediction"),width=0.4, position=position_nudge(x = -0.2, y = 0)) +
    scale_fill_manual(values=c("royalblue4", "darksalmon"), name="Values")+
    labs(x="Month", y="Total cyclist counted", title="graph 20: Actual and Predicted Cyclist Count for 2020")+
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))

  Graph_2020 %>% filter(year==2021) %>% group_by(month) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction))%>%
    ggplot(aes(x=month)) + 
    geom_col(aes(y=actual, fill ="Actual"), width=0.4, position=position_nudge(x = 0.2, y = 0)) + 
    geom_col(aes(y=prediction, fill = "Prediction"),width=0.4, position=position_nudge(x = -0.2, y = 0)) +
    scale_fill_manual(values=c("royalblue4", "darksalmon"), name="Values")+
    labs(x="Month", y="Total cyclist counted", title="graph 21: Actual and Predicted Cyclist Count for 2021")+
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))
  
  Graph_2020 %>% filter(year==2022) %>% group_by(month) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction))%>%
    ggplot(aes(x=month)) + 
    geom_col(aes(y=actual, fill ="Actual"), width=0.4, position=position_nudge(x = 0.2, y = 0)) + 
    geom_col(aes(y=prediction, fill = "Prediction"),width=0.4, position=position_nudge(x = -0.2, y = 0)) +
    scale_fill_manual(values=c("royalblue4", "darksalmon"), name="Values")+
    labs(x="Month", y="Total cyclist counted", title="graph 22: Actual and Predicted Cyclist Count for 2022")+
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))

  Graph_2020 %>% group_by(year) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction))%>%
    ggplot(aes(x=year)) + 
    geom_col(aes(y=actual, fill ="Actual"), width=0.4, position=position_nudge(x = 0.2, y = 0)) + 
    geom_col(aes(y=prediction, fill = "Prediction"),width=0.4, position=position_nudge(x = -0.2, y = 0)) +
    scale_fill_manual(values=c("royalblue4", "darksalmon"), name="Values")+
    labs(x="Month", y="Total cyclist counted", title="graph 23: Actual and Predicted Cyclist Count per year")+
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))+
    scale_x_continuous(breaks=c(2020:2022))
  
  Graph_2020 %>% group_by(year) %>%
    summarize(actual=sum(gesamt), prediction=sum(prediction), error=sum(gesamt)-sum(prediction), errorPerc = (sum(gesamt)-sum(prediction))/sum(gesamt))
  
# Before ending I set the Locale back to standard (changed before for the days/months)
    Sys.setlocale(category = "LC_ALL", locale = "")

# I also save the major tables used as csv for later reference
    
    write.csv(Daily, "Daily.csv")
    write.csv(Daily_2020_2022, "Daily_2020_2022.csv") 
    write.csv(DataDictionary, "DataDictionary.csv")
    write.csv(holidays_timeframe, "holidays_timeframe.csv")
    write.csv(public_holidays, "public_holidays.csv")
    
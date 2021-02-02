#####################################
## PART 1: Import Required Libraries
#####################################
library(pdftools)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rgdal)
library(maptools)
library(rgeos)
library(ggpubr)
library(RCurl)
library(XML)
library(stringr)
library(stringi)
library(grid)
library(RColorBrewer)
library(scales)
#================
# END of PART 1
#================

####################################
## PART 2: Declare a few Constants
####################################
provinceList <- c("AB", "BC", "MB", "NB", "NF", "NS", "NT", "NU", "ON", "PI", "PQ", "SK", "YT")
daysOfWeekList <- c("Monday",
                     "Tuesday",
                     "Wednesday",
                     "Thursday",
                     "Friday",
                     "Saturday",
                     "Sunday")

shapesList <- c("ps", "rectangle", "round", "sphere", "square", "triangle", "boomerang", "cigar", "diamond", "disc", "fireball", "irregular", "oval")
provinceAndTypePattern <- paste0(
  paste(paste("~", provinceList, sep=""), collapse = "~(([a-zA-Z]{2,2})|([a-zA-Z]{1,1}\\d{1,1}))~|"), 
  "~(([a-zA-Z]{2,2})|([a-zA-Z]{1,1}\\d{1,1}))")

strngnessRelbltySrcConclsnPattern <- "(\\d~\\d~[a-zA-Z]{2,}~[a-zA-Z])~"

## Data for 1992 does not have the 'source' field populated
strngnessRelbltySrcConclsnPattern_92 <- "(\\d~\\d~[a-zA-Z])~"
## Data for 1993 does not have reliability populated
strngnessRelbltySrcConclsnPattern_93 <- "(\\d~[a-zA-Z]{2,}~[a-zA-Z])~"

beginsWithYrMthDatePattern <- "^(\\d{4,4}~\\d{1,2}~\\d{1,2})"

# Folder paths to all the data and charts
ufoDataFolderPath <- "C:/Users/clockwork/Documents/UofT - Fundamentals of Data Science/SCS_3250_003 (Foundations of Data Science)/Term Project/UFO Sightings/Data"
supplementaryDataFolderPath <- "C:/Users/clockwork/Documents/UofT - Fundamentals of Data Science/SCS_3250_003 (Foundations of Data Science)/Term Project/UFO Sightings/Supplementary Data"
mapDataFolderPath <- "C:/Users/clockwork/Documents/UofT - Fundamentals of Data Science/SCS_3250_003 (Foundations of Data Science)/Term Project/UFO Sightings/CN_MAP_SHP"
chartsFolderPath <- "C:/Users/clockwork/Documents/UofT - Fundamentals of Data Science/SCS_3250_003 (Foundations of Data Science)/Term Project/UFO Sightings/Charts/"

# Wikipedia URL to list of movies featuring Aliens
wikipediaETMoviesListURL <- "https://en.wikipedia.org/wiki/List_of_films_featuring_extraterrestrials"
# IMDB URL pattern string
imdbURLPattern = "http://www.imdb.com/title/tt\\d{1,}/"



#####
## Define a function to return the province or territory code for a given province or territory.
###
getProvinceCode <- function(provinceName){
  provinceCode <- ifelse(provinceName == "Alberta", "AB",
                         ifelse(provinceName == "British Columbia", "BC",
                                ifelse(provinceName == "Manitoba", "MB",
                                       ifelse(provinceName == "New Brunswick", "NB",
                                              ifelse(provinceName == "Newfoundland and Labrador", "NF", 
                                                     ifelse(provinceName == "Nova Scotia", "NS",
                                                            ifelse(provinceName == "Nunavut", "NU",
                                                                   ifelse(provinceName == "Ontario", "ON",
                                                                          ifelse(provinceName == "Prince Edward Island", "PI",
                                                                                 ifelse(provinceName == "Quebec", "PQ",
                                                                                        ifelse(provinceName == "Saskatchewan", "SK",
                                                                                               ifelse(provinceName == "Yukon", "YT", 
                                                                                                      ifelse(provinceName == "Northwest Territories", "NT", NA
                                                                                                      )))))))))))))
  
  return(provinceCode)
}

###############################################################################
## Define a function to generate a standard theme to use for all charts in
## the analysis so that they all have the same font sizes and other 
## formatting.
#################################################################
std_chart_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text, face = "bold")) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text, face = "bold")) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0, face = "bold")) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25, face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#================
# END of PART 2
#================


################################################
## PART 3: Read in all the required data
################################################
## (1) UFO Sightings
allUFOSightingsData <- data.frame(year = numeric(0),
                                  month = numeric(0),
                                  day = numeric(0),
                                  hour = numeric(0),
                                  location = character(0),
                                  province = character(0),
                                  type = character(0),
                                  duration = numeric(0),
                                  color = character(0),
                                  witness = numeric(0),
                                  shape = character(0),
                                  strangeness = numeric(0),
                                  reliablity = numeric(0),
                                  source = character(0),
                                  conclusion = character(0))
allUFOSightingsData <- data.frame(lapply(allUFOSightingsData, as.character), stringsAsFactors=FALSE)
fileList <- dir(ufoDataFolderPath)
dataRowCounter = 0

for(fileCounter in 1:length(fileList)){
  nextFileName <- fileList[fileCounter]
  print(paste0("Reading file ", fileCounter, " of ", length(fileList)))
  
  # Read file into single line of text
  rawTxt <- pdf_text(pdf = paste0(ufoDataFolderPath, "/", nextFileName))  
  # Convert to vector
  rawTxtPages <- unlist(rawTxt)
  
  for(pageCounter in 1:length(rawTxtPages)){
      print(paste0("--->Reading page ", pageCounter, " of ", length(rawTxtPages)))
      # Break on new-line/carriage return and split each page into lines
      lines <- unlist(str_split(string = rawTxtPages[pageCounter], pattern = "\\r\\n"))
      #print(lines[3])
      for(lineCounter in 1:length(lines)){
        #print(paste0("------>Reading line ", lineCounter, " of ", length(lines)))
        nextLine <- trimws(lines[lineCounter])
        nextLine <- str_replace_all(string = nextLine, pattern = "\\s{1,}", replacement = "~")
        #print(nextLine)
        if((grepl(x = nextLine, pattern = beginsWithYrMthDatePattern) == TRUE) & (grepl(pattern = provinceAndTypePattern, x = nextLine) == TRUE)){
            #possibleMultiWordCityNm <- (str_extract(nextLine, "([A-Za-z]{3,}\\s[A-Za-z]{2,}(\\s[A-Za-z]*))"))
            #nextLine <- (str_replace(string = nextLine, pattern = possibleMultiWordCityNm, replacement = (str_replace(string = possibleMultiWordCityNm, pattern = "\\s", replacement = "_"))))
            
            #print(nextLine)   
            lineParts <- unlist(str_split(string = nextLine, pattern = "~"))
            dataRowCounter = dataRowCounter + 1
            allUFOSightingsData[dataRowCounter, 1] <- lineParts[1]
            allUFOSightingsData[dataRowCounter,  2] <- lineParts[2]
            allUFOSightingsData[dataRowCounter,  3] <- lineParts[3]
            
            # Hour & Location  
            locationStartIndex <- 5
            if(grepl(x = lineParts[4], pattern = "\\D")){
              #print(lineParts[4])
              locationStartIndex <- 4
            }
            else{
              allUFOSightingsData[dataRowCounter,  4] <- lineParts[4]
            }
            
            partIncrement = 0
            locationStr = ""
            while(!(lineParts[locationStartIndex + partIncrement] %in% provinceList)){
              #print(paste0((locationStartIndex+partIncrement), ": ", lineParts[locationStartIndex+partIncrement]))
              locationStr = paste0(locationStr, " ", lineParts[locationStartIndex + partIncrement])
              partIncrement = partIncrement + 1
            }
            allUFOSightingsData[dataRowCounter,  5] <- locationStr
            provinceIndex = locationStartIndex + partIncrement
            
            # Province
            allUFOSightingsData[dataRowCounter,  6] <- lineParts[provinceIndex]
            
            # Type
            allUFOSightingsData[dataRowCounter,  7] <- lineParts[provinceIndex + 1]
            
            #Strangeness, Reliability, Source, Conclusion
            if(lineParts[1] == "1992"){
              patternToUse <- strngnessRelbltySrcConclsnPattern_92
            }
            else if(lineParts[1] == "1993"){
              patternToUse <- strngnessRelbltySrcConclsnPattern_93
            }
            else{
              patternToUse <- strngnessRelbltySrcConclsnPattern
            }
            
            lastFourPartsStr <- str_extract(string = nextLine, pattern = patternToUse)
            lastFourPartsList <- unlist(str_split(string = lastFourPartsStr, pattern = "~"))
            allUFOSightingsData[dataRowCounter,  12] <- lastFourPartsList[1] 
            allUFOSightingsData[dataRowCounter,  13] <- lastFourPartsList[2]
            allUFOSightingsData[dataRowCounter,  14] <- lastFourPartsList[3]
            allUFOSightingsData[dataRowCounter,  15] <- lastFourPartsList[4]
            
            # 4 middle fields: duration, color, shape, witnesses
            #print(provinceAndTypePattern)
            otherPartsStartPos <- (str_locate(string = nextLine, pattern = provinceAndTypePattern)[,1]) + 6
            otherPartsEndPos <- str_locate(string = nextLine, pattern = lastFourPartsStr)[,1] - 2
            otherPartsStr = tolower(substr(x = nextLine, start = otherPartsStartPos, stop = otherPartsEndPos))
            
            # Set Shape
            shapeString <- (str_extract(string = otherPartsStr, pattern = paste(shapesList, collapse = "|")))
            allUFOSightingsData[dataRowCounter,  11] <- shapeString
            # Any words remaining in the string should be color
            colorString <- str_extract(string = str_replace(string = otherPartsStr, pattern = shapeString, replacement = ""), 
                        pattern = "\\D{2,}")
            colorString = str_replace_all(string = colorString, pattern = "~", replacement = "")
            allUFOSightingsData[dataRowCounter,  9] <- colorString
            
        }
      }
  }
  print(paste0("-->Done. Read ", length(rawTxtPages), " pages!"))
}


## (2) Read Canada map shapes
canMapCoords <- readOGR(mapDataFolderPath, "gpr_000b11a_e")
canMapCoords.DF <- fortify(canMapCoords, region = "PRUID")
canMapCoords.DF$PRUID <- canMapCoords.DF$id
canMapCoords.DF$id <- NULL
# Read province names and ids to a list
provinceDetails <- as.data.frame(canMapCoords)

## (3) Read Canada Population data
canadaPopulation_2003To2016 <- read.csv(paste0(ufoDataFolderPath,"/Canada_Population_by_province_2003-2016.csv"), stringsAsFactors = FALSE)

## (4) Read Heavy drinking statistics data
canadaHeavyDrinking_2003To2016 <- read.csv(paste0(ufoDataFolderPath,"/Heavy_Drinking_Stats_CAN_2003-2012.csv"), stringsAsFactors = FALSE)

## (5) Read Cannabis usage statistics data
canadaCannabisUsg_2011To2012 <- read.csv(paste0(supplementaryDataFolderPath,"/Cannabis_Use_Stats_CAN_2011-2012.csv"), stringsAsFactors = FALSE)

## (6) Read movie titles featuring Extra Terrestrials
##     Then for each movie, read the corresponding Wikipedia page and extract IMDB title
##     Then on each IMDB page, read the ratings string.
opts <- list(
  ssl.verifypeer = FALSE
)

options(RCurlOptions = opts)
webPage <- getURL(wikipediaETMoviesListURL)
pagetree <- htmlTreeParse(webPage, useInternalNodes = TRUE)
nodes <- getNodeSet(pagetree, "//table[@class = 'wikitable sortable']/tr")

allMoviesData <- data.frame(
  title = character(0),
  year = numeric(0),
  wikiURL = character(0),
  imdbURL = character(0),
  imdbRating = character(0)
)
allMoviesData <- data.frame(lapply(allMoviesData, as.character), stringsAsFactors=FALSE)

rowCounter = 0

for(node in nodes){
  #print (node)
  rowCounter = rowCounter + 1
  nextFName <- (xpathSApply(node, './/td', xmlValue))[1]
  nextFYear <- (xpathSApply(node, './/td', xmlValue))[2]
  nextURLPart <- paste((xpathSApply(node, './/a', xmlGetAttr, 'href'))[1], collapse='')
  
  allMoviesData[rowCounter, 1] <- paste(unlist(nextFName), collapse='')
  allMoviesData[rowCounter, 2] <- paste(unlist(nextFYear), collapse='')
  
  if(!(nextURLPart == "NULL")){
    nextURL <- paste0("https://en.wikipedia.org", nextURLPart)
    allMoviesData[rowCounter, 3] <- nextURL
    
    nextWikiPage <- getURL(nextURL, followlocation=TRUE)
    nextWikiPageTree <- htmlTreeParse(nextWikiPage, useInternalNodes = TRUE)
    
    nextIMDBURL <- str_extract(string = (xpathSApply(nextWikiPageTree, "//li//a[@rel='nofollow' and @class='external text' and contains(@href,'imdb.com/title')]", xmlGetAttr, 'href'))[1], pattern = imdbURLPattern)
    if(length(nextIMDBURL) > 0){
      nextIMDBURLString <- paste(unlist(nextIMDBURL), collapse='')
      allMoviesData[rowCounter, 4] <- nextIMDBURLString
      if(!nextIMDBURLString == "NA" & !is.na(nextIMDBURLString)){
        print("Fetching imdb info...")
        nextIMDBPage <- getURL(nextIMDBURL)
        print("...Done. Parsing...")
        nextIMDBPageTree <- htmlTreeParse(nextIMDBPage, useInternalNodes = TRUE)
        print("Done!")
        nextRatingString <- (xpathSApply(nextIMDBPageTree, "//div[@class = 'ratingValue']/strong", xmlGetAttr, 'title'))[1]
        allMoviesData[rowCounter, 5] <-  paste(unlist(nextRatingString), collapse='')
      }
    }
  }
  print(paste0("Completed ", rowCounter, " of ", length(nodes)))
}


#================
# END of PART 3
#================

###################################
## PART 4: do some pre-processing
###################################

## 4(A) Province details.
# Add a new code for Province Code
provinceDetails$PROV_CD <- getProvinceCode(provinceDetails$PRENAME)
# Convert Province Id to character to faciliate joins
provinceDetails <- provinceDetails %>% mutate(PRUID = as.character(PRUID))
# Add 2 columns for province centroid coordinates to faciliate plotting on a map
provinceCentroids <- canMapCoords.DF %>%
  group_by(PRUID) %>%
  summarise(lat_prov_Centre = mean(range(lat)), long_prov_Centre = mean(range(long)))
# Add these two columns to the provinceDetails dataframe
provinceDetails <- provinceDetails %>%
  inner_join(provinceCentroids)
## Join provinceList to the canada map coords data frame
canMapCoords_withProvCodes <- 
  provinceDetails %>%
  select(PRUID, PROV_CD, lat_prov_Centre, long_prov_Centre) %>%
  inner_join(canMapCoords.DF)
  
## 4(B) Population details
# Add a new column for Province Code to faciliate joins
canadaPopulation_2003To2016$Province = trimws(canadaPopulation_2003To2016$Province)
canadaPopulation_2003To2016$PROV_CD <- getProvinceCode(trimws(canadaPopulation_2003To2016$Province))

## 4(C) Heavy drinking stats data
# Add a new column for Province Code to faciliate joins
canadaHeavyDrinking_2003To2016$Province <- trimws(canadaHeavyDrinking_2003To2016$Province)
canadaHeavyDrinking_2003To2016$PROV_CD <- getProvinceCode(trimws(canadaHeavyDrinking_2003To2016$Province))
# Add poplation and per-capita heavy drinkers percentage
canadaHeavyDrinking_2003To2016 <- canadaHeavyDrinking_2003To2016 %>%
  inner_join(canadaPopulation_2003To2016, by = c("Year" = "Year", "PROV_CD" = "PROV_CD")) %>%
  mutate(PERCENT_HEAVY_DRINKERS = (Heavy_Drinkers_Count*100/Population)) %>%
  select(Year, PROV_CD, Population, PERCENT_HEAVY_DRINKERS)

## 4(D) UFO Sightings Data
allUFOSightingsData_CLND <- allUFOSightingsData
allUFOSightingsData_CLND <- allUFOSightingsData_CLND %>% mutate(
                                      year = as.numeric(year),
                                      month = as.numeric(month),
                                      day = as.numeric(day),
                                      hour = as.numeric(hour))
# Add a day of week column
allUFOSightingsData_CLND <- allUFOSightingsData_CLND %>% 
  mutate(DAY_OF_WEEK = weekdays(as.Date(paste0(year,"-", month,"-", day))))

# Add a 'cleaned' up Time of Day column for values that have length = 3 or 4 only
# as it is difficult to guess what the 1/2 length values mean
allUFOSightingsData_CLND <- allUFOSightingsData_CLND %>%
  mutate(TM_OF_DAY = ifelse(str_length(hour) == 3, round(as.numeric(substring(hour, 1, 1)) + as.numeric(substring(hour, 2, 3))/60, digits = 2), 
                            ifelse(str_length(hour) == 4, round(as.numeric(substring(hour, 1, 2)) + as.numeric(substring(hour, 3, 4))/60, digits = 2), NA)
                            ))

allUFOSightingsData_CLND$TM_RNG_BY_HR = cut.default(x = allUFOSightingsData_CLND$TM_OF_DAY, include.lowest = TRUE, breaks = seq(from = 0, to = 24))

# Add a 'cleaned-up' color field:
# (1) if there is more than one color, standardize to 'multi' - also fix spellings for 'muli'
#     and change 'multiple' to multi
allUFOSightingsData_CLND <- allUFOSightingsData_CLND %>%
  mutate(COLOR_STDIZED = ifelse(grepl(x = color, pattern = "^\\W|andtriangle|ellie"), NA,
                           ifelse(grepl(x = color, pattern = "muli|multiple|muti|multi|\\W"), "multi",
                               ifelse(grepl(x = color, pattern = "red"), "red",
                                 ifelse(grepl(x = color, pattern = "white|wihte|beige"), "white",
                                   ifelse(grepl(x = color, pattern = "bronze|copper"), "orange",
                                     ifelse(grepl(x = color, pattern = "gray"), "grey",
                                       ifelse(grepl(x = color, pattern = "tan"), "brown",
                                         ifelse(grepl(x = color, pattern = "mauve"), "purple",                 
                                           ifelse(grepl(x = color, pattern = "gold|amber"), "yellow",                 
                                             ifelse(grepl(x = color, pattern = "turquoise"), "blue", 
                                                    ifelse(grepl(x = color, pattern = "dark"), "black", color
                                                     ))))))))))))
         
## 4(E) Cannabis usage details.
# Add a new code for Province Code
canadaCannabisUsg_2011To2012$Province <- trimws(canadaCannabisUsg_2011To2012$Province)
canadaCannabisUsg_2011To2012$PROV_CD <- getProvinceCode(canadaCannabisUsg_2011To2012$Province)

## 4(F) Movies feat. aliens: parse the ratings string and extract the rating and num votes into 
##      separate fields.
allMoviesData$AVG_IMDB_RATING <- as.numeric(stri_extract_first(allMoviesData$imdbRating, regex = "\\d{1,}(.|,)\\d+"))
allMoviesData$TOT_IMDB_VOTES <- as.numeric(str_replace_all(string = stri_extract_last(allMoviesData$imdbRating, regex = "\\d{1,}(.|,)\\d+"), pattern = ",", replacement = ""))
allMoviesData <- allMoviesData %>% 
  mutate(year = as.numeric(year))
  
#================
# END of PART 4
#================

##########################################
## PART 5: Do some analysis
#########################################

## 5A: Basic Analysis

## 5B: Temporal patterns
# (1) By year
allUFOSightingsData_CLND %>%
  mutate(TOP_1 = ifelse((year == 2012), 'Y', 'N')) %>%
  group_by(year, TOP_1) %>%
  summarise(tot_sightings = n()) %>%
  select(year, tot_sightings, TOP_1) %>%
  ggplot(aes(x = year, y = tot_sightings)) +
  geom_bar(stat = "identity", aes(fill=TOP_1))+
  scale_fill_manual(values = c("Y" = "#6433ff", "N" = "#FF5733"))+
  geom_smooth(method = "lm", se = FALSE)+
  scale_x_discrete(limits = seq(1989, 2016))+
  std_chart_theme()+
  theme(axis.text.x = element_text(face="bold", angle=90, hjust=1))+
  xlab("Year")+
  ylab("Total UFO Sightings")

ggsave(paste0(chartsFolderPath,"P1_TP_BY_YR.png"), dpi=300, width=4, height=3)

# (1A) Color
allUFOSightingsData_CLND %>%
  filter(!is.na(COLOR_STDIZED) & str_length(COLOR_STDIZED) > 2) %>%
  group_by(COLOR_STDIZED, year) %>%
  summarise(TOT_CNT = n()) %>%
  arrange(year, desc(TOT_CNT)) %>%
  ggplot()+
  geom_area(position = "fill", aes(x = year, y = TOT_CNT, fill = COLOR_STDIZED), alpha=0.6)+
  scale_x_discrete(limits = seq(1989, 2016))+
  std_chart_theme()+
  theme(legend.position="right", axis.text.x = element_text(face="bold", angle=90, hjust=1))+
  scale_fill_manual(name = "UFO Color", values = c("black" = "black", "blue" = "blue", "brown" = "brown", "green" = "green", "grey" = "grey", "orange" = "orange", "pink" = "pink", "purple" = "purple", "red" = "red", "silver" = "#ece9e8", "white" = "white", "yellow" = "yellow", "multi" = "#d5cf08", "metallic" = "#8a8a82"))+
  xlab("Year")+
  ylab("Proportion of Total")
ggsave(paste0(chartsFolderPath,"P1A_COL_BY_YR.png"), dpi=300, width=4, height=4)

# 1(B) UFO Shapes
allUFOSightingsData_CLND %>%
  filter(!is.na(shape)) %>%
  group_by(shape, year) %>%
  summarise(TOT_CNT = n()) %>%
  arrange(year, desc(TOT_CNT)) %>%
  ggplot()+
  geom_area(position = "fill", aes(x = year, y = TOT_CNT, fill = shape), alpha=0.6)+
  scale_x_discrete(limits = seq(1989, 2016))+
  std_chart_theme()+
  theme(legend.position="right", axis.text.x = element_text(face="bold", angle=90, hjust=1))+
  scale_fill_discrete(name = "UFO Shape")+
  xlab("Year")+
  ylab("Proportion of Total")
ggsave(paste0(chartsFolderPath,"P1B_SHPL_BY_YR.png"), dpi=300, width=4, height=3.8)

# (2) By month of year
allUFOSightingsData_CLND %>% 
  mutate(TOP_2 = ifelse((month == 7 | month == 8), 'Y', 'N')) %>%
  filter(month <= 12) %>%
  group_by(month, TOP_2) %>%
  summarise(tot_sightings = n()) %>%
  select(month, tot_sightings, TOP_2) %>%
  ggplot(aes(x = month, y = tot_sightings)) +
  geom_bar(stat = "identity", aes(fill=TOP_2))+
  scale_fill_manual(values = c("Y" = "#6433ff", "N" = "#FF5733"))+
  scale_x_discrete(limits = month.abb)+
  xlab("")+
  ylab("")+
  theme_bw()+
  #theme(legend.position="none", axis.text.y = element_text(size = 11, face="bold"), axis.text.x = element_text(size = 11, face="bold"))+
  labs(x = "Month of year", y = "Total UFO Sightings", fill = NULL)+
  std_chart_theme()

ggsave(paste0(chartsFolderPath,"P2_TP_BY_MTH.png"), dpi=300, width=4, height=3)

# (3) By day of week
allUFOSightingsData_CLND %>% 
  filter(!is.na(DAY_OF_WEEK)) %>%
  mutate(TOP_2 = ifelse((DAY_OF_WEEK == 'Saturday' | DAY_OF_WEEK == 'Sunday'), 'Y', 'N')) %>%
  group_by(DAY_OF_WEEK, TOP_2) %>%
  summarise(tot_sightings = n()) %>%
  select(DAY_OF_WEEK, tot_sightings, TOP_2) %>%
  ggplot(aes(x = DAY_OF_WEEK, y = tot_sightings, group = 1)) +
  geom_bar(stat = "identity", aes(fill=TOP_2))+
  scale_fill_manual(values = c("Y" = "#6433ff", "N" = "#bab6ba"))+
  scale_x_discrete(limits = daysOfWeekList)+
  xlab("Day of week")+
  ylab("Total UFO Sightings")+
  theme(legend.position="none", axis.text.y = element_text(size = 11, face="bold"), axis.text.x = element_text(size = 11, face="bold"))+
  std_chart_theme()
ggsave(paste0(chartsFolderPath,"P3_TP_BY_DoW.png"), dpi=300, width=4, height=3)

# (4) Hour of day
allUFOSightingsData_CLND %>% 
  filter(!is.na(TM_RNG_BY_HR)) %>%
  mutate(TOP_3 = ifelse((TM_RNG_BY_HR == '(20,21]' | TM_RNG_BY_HR == '(21,22]' | TM_RNG_BY_HR == '(22,23]'), 'Y', 'N')) %>%
  group_by(TM_RNG_BY_HR, TOP_3) %>%
  summarise(tot_sightings = n()) %>%
  select(TM_RNG_BY_HR, tot_sightings, TOP_3) %>%
  ggplot(aes(x = TM_RNG_BY_HR, y = tot_sightings, group = 1)) +
  geom_bar(stat = "identity", aes(fill=TOP_3))+
  scale_fill_manual(values = c("Y" = "#6433ff", "N" = "#FFC733"))+
  xlab("Time of day (1-hr ranges)")+
  ylab("Total UFO Sightings")+
  std_chart_theme()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face="bold"))
ggsave(paste0(chartsFolderPath,"P4_TP_BY_HoD.png"), dpi=300, width=4, height=3)  


## 5(C) Geographical Patterns  

# (1) Total sighting volumes by province
totalSightingsByProvince <- 
  allUFOSightingsData_CLND %>% 
  group_by(province) %>%
  summarise(TOT_SIGHTINGS = n())

totalSightingsByProvince_withCoord <- 
  totalSightingsByProvince %>%
  inner_join(canMapCoords_withProvCodes, by = c("province" = "PROV_CD"))

ggplot() +
  geom_polygon(data = totalSightingsByProvince_withCoord, color = "grey10", size = 0.2, aes(x = long, y = lat, group = group, fill = cut.default(TOT_SIGHTINGS, breaks = c(0, 100, 500, 1000, 2000, 6000), include.lowest = TRUE))) +
  geom_text(data=provinceDetails, aes(x=long_prov_Centre, y=lat_prov_Centre, label=PREABBR, fontface="bold"), size=3) +
  coord_equal() +
  scale_fill_brewer(palette = "Reds", name = "Total UFO Sightings", labels = c("0 - 100", "101 - 500", "501 - 1000", "1001 - 2000", "2001 - 6000")) +
  theme_void() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = NA, colour = "#cccccc"))+
  # Set the labels to NULL so that no space is allocated for them
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Geospatial Patterns",
       subtitle = "Total sightings volumes (1989 - 2016) by Province")+
  theme(text = element_text(family = "Arial Narrow", size = 8),
        plot.title = element_text(size = 12, face = "bold"),
        plot.margin = unit(c(0, 0.25, 0.0, 0.25), "in"),
        panel.border = element_rect(fill = NA, colour = "#cccccc"),
        legend.text = element_text(size = 8))

# (2) Sightings per 100K population
sightingsByProvince_2003_2016_perCapita <- 
  allUFOSightingsData_CLND %>% 
  filter(year >= 2003 & year <= 2016) %>%
  group_by(year, province) %>%
  summarise(TOT_SIGHTINGS = n()) %>%
  inner_join(provinceDetails, by = c("province" = "PROV_CD")) %>%
  inner_join(canadaPopulation_2003To2016, by = c("year" = "Year","province" = "PROV_CD")) %>%
  mutate(SIGHTINGS_PER_CAPITA = (TOT_SIGHTINGS*100000/Population)) %>%
  group_by(province) %>%
  summarise(TOT_SIGHTINGS_PER_CAPITA = sum(SIGHTINGS_PER_CAPITA), TOT_SIGHTINGS_PER_CAPITA_RNDED = ceiling(TOT_SIGHTINGS_PER_CAPITA))

sightingsByProvince_2003_2016_perCapita_withCoord <- 
  sightingsByProvince_2003_2016_perCapita %>%
  inner_join(canMapCoords_withProvCodes, by = c("province" = "PROV_CD"))  

ggplot() +
  geom_polygon(data = sightingsByProvince_2003_2016_perCapita_withCoord, color = "grey10", size = 0.2, aes(x = long, y = lat, group = group, fill = cut.default(TOT_SIGHTINGS_PER_CAPITA_RNDED, breaks = c(0, 20, 25, 40, 50, 60, 75), include.lowest = TRUE))) +
  geom_text(data=provinceDetails, aes(x=long_prov_Centre, y=lat_prov_Centre, label=PREABBR, fontface="bold"), size=3) +
  coord_equal() +
  scale_fill_brewer(palette = "Reds", name = "UFO Sightings per 100K pop.", labels = c("0 - 20", "21 - 25", "26 - 40", "41 - 50", "51 - 60", "61 - 75")) +
  theme_void() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = NA, colour = "#cccccc"))+
  # Set the labels to NULL so that no space is allocated for them
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Geospatial Patterns",
       subtitle = "Sightings per 100K population (2003 to 2016) by Province")+
  theme(text = element_text(family = "Arial Narrow", size = 8),
        plot.title = element_text(size = 12, face = "bold"),
        plot.margin = unit(c(0, 0.25, 0.0, 0.25), "in"),
        panel.border = element_rect(fill = NA, colour = "#cccccc"),
        legend.text = element_text(size = 8))

## 5(D) Influences of other specific external factors
# (1) Per Capita heavy drinking vs per capita sightings for years 2003 - 2012
perCapitaSightingsAndHeavyDrinking <-   
  allUFOSightingsData_CLND %>% 
  filter(year >= 2003 & year <= 2012) %>%
  group_by(year, province) %>%
  summarise(TOT_SIGHTINGS = n()) %>%
  inner_join(provinceDetails, by = c("province" = "PROV_CD")) %>%
  inner_join(canadaPopulation_2003To2016, by = c("year" = "Year","province" = "PROV_CD")) %>%
  mutate(SIGHTINGS_PER_CAPITA = (TOT_SIGHTINGS*100000/Population)) %>%
  inner_join(canadaHeavyDrinking_2003To2016, by = c("province" = "PROV_CD", "year" = "Year")) %>%
  select(SIGHTINGS_PER_CAPITA, PERCENT_HEAVY_DRINKERS)


ggscatter(perCapitaSightingsAndHeavyDrinking, 
          x = "PERCENT_HEAVY_DRINKERS", 
          y = "SIGHTINGS_PER_CAPITA",
          color = "black",
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.coeff.args = list(method = "pearson", size= 4, color = "blue", label.sep = "\n"),
          xlab = "Heavy Drinkers (% of total pop.)", 
          ylab = "Sightings per 100K pop.")+
  std_chart_theme()

ggsave(paste0(chartsFolderPath,"P5_AlcConsumpt.png"), dpi=300, width=4, height=3)  


# (2) Per Capita cannabis usage vs per capita sightings for years 2011 and 2012
perCapitaSightingsAndCannabisUsage <-   
  allUFOSightingsData_CLND %>% 
  filter(year >= 2011 & year <= 2012) %>%
  group_by(year, province) %>%
  summarise(TOT_SIGHTINGS = n()) %>%
  inner_join(provinceDetails, by = c("province" = "PROV_CD")) %>%
  inner_join(canadaPopulation_2003To2016, by = c("year" = "Year","province" = "PROV_CD")) %>%
  mutate(SIGHTINGS_PER_CAPITA = (TOT_SIGHTINGS*100000/Population)) %>%
  inner_join(canadaCannabisUsg_2011To2012, by = c("province" = "PROV_CD", "year" = "Year")) %>%
  select(SIGHTINGS_PER_CAPITA, Cannabis_Users_Percent_Pop)

ggscatter(perCapitaSightingsAndCannabisUsage, 
          x = "Cannabis_Users_Percent_Pop", 
          y = "SIGHTINGS_PER_CAPITA",
          color = "black",
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.coeff.args = list(method = "pearson", size= 4, color = "blue", label.sep = "\n"),
          xlab = "Cannabis users (% of total pop.)", 
          ylab = "Sighings per 100K pop.")+
  std_chart_theme()
ggsave(paste0(chartsFolderPath,"P6_CannaConsumpt.png"), dpi=300, width=4, height=3)  

# (3) Sum-product of movie ratings to number of votes of movies featuring extra terrestrials 
#     by year to total sightings for the year.

## (i) Number of votes only - this would only be a measure of the movie's popularity
movieVoteSummary <- allMoviesData %>%
  filter(!is.na(AVG_IMDB_RATING) & !is.na(TOT_IMDB_VOTES)) %>%
  group_by(year) %>%
  summarise(TOT_NUM_VOTES = sum(TOT_IMDB_VOTES))

movieVoteRatingProdSumSummary <- allMoviesData %>%
  filter(!is.na(AVG_IMDB_RATING) & !is.na(TOT_IMDB_VOTES)) %>%
  mutate(RTNG_VOTE_PROD = (AVG_IMDB_RATING * TOT_IMDB_VOTES)) %>%
  group_by(year) %>%
  summarise(RTNG_VOTE_SUM_PROD = sum(RTNG_VOTE_PROD))

countOfUFOSightingsByYr <- allUFOSightingsData_CLND %>%
  group_by(year) %>%
  summarise(TOT_SIGHTINGS = n())

## (i) Plot total ufo sightings to sum of total imdb votes for the year
movieVoteSummary %>%
  inner_join(countOfUFOSightingsByYr, by = c("year" = "year")) %>%
  ggscatter(x = "TOT_NUM_VOTES", 
            y = "TOT_SIGHTINGS", 
            color = "black",
            add = "reg.line",
            add.params = list(color = "blue", fill = "lightgray"),
            conf.int = TRUE, 
            cor.coef = TRUE, 
            cor.coeff.args = list(method = "pearson", size= 4, color = "blue", label.sep = "\n"),
            #cor.method = "pearson",
            font.label = c(8, "bold", "red"),
            xlab = "Total IMDB votes for the year", 
            ylab = "Total UFO sightings")+
  scale_x_continuous(labels = comma)+
  std_chart_theme()
ggsave(paste0(chartsFolderPath,"P7_IMDB_1.png"), dpi=300, width=4, height=3)

movieVoteRatingProdSumSummary %>%
  inner_join(countOfUFOSightingsByYr, by = c("year" = "year")) %>%
  ggscatter(x = "RTNG_VOTE_SUM_PROD", 
            y = "TOT_SIGHTINGS", 
            color = "black",
            add = "reg.line",
            add.params = list(color = "blue", fill = "lightgray"),
            conf.int = TRUE, 
            cor.coef = TRUE, 
            cor.coeff.args = list(method = "pearson", size= 4, color = "blue", label.sep = "\n"),
            #cor.method = "pearson",
            font.label = c(8, "bold", "red"),
            xlab = "SUM[{IMDB Rating} x {# Votes}] for the year", 
            ylab = "Total UFO sightings")+
  scale_x_continuous(labels = comma)+
  std_chart_theme()
ggsave(paste0(chartsFolderPath,"P7_IMDB_2.png"), dpi=300, width=4, height=3)

#================
# END of PART 5
#================  
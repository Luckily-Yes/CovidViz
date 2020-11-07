
require(ggplot2)
require(viridis)
require(grid)
require(gridExtra)
require(stringi)
require(stringr)
require(data.table)
require(rjson)

if (Sys.getenv("MyRProjects") == "") {
  stop ("Need to define environment variable MyRProjects")
}

# define base path 
basepath = paste0(Sys.getenv("MyRProjects"),"\\CovidStringency")

# define, which countries to plot
CountryCodes = c("NZL","AUS","DEU","ITA","FRA","BRA", "NLD","GBR","ARG")

DelayDatesToPlot = c(0,7,10,14,18,21)


## Get 1st data set: Stringency data from https://covidtrackerapi.bsg.ox.ac.uk

# create query string for getting stringency data
jsonquery = paste0("https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range/2020-01-01/", Sys.Date())

# perform query
if (!exists ("StringencyRawData"))  {
  message("Query data from ", jsonquery)
  StringencyRawData = rjson::fromJSON(file=jsonquery, method="C")
}

# extract relevant data from json (not nice...)
unlisteddata = unlist(StringencyRawData$data)
StringencyData = data.frame(key = names(unlisteddata), value = unlisteddata, stringsAsFactors = FALSE)

StringencyData$Date = as.Date(stringr::str_extract(StringencyData$key,"^\\d\\d\\d\\d-\\d\\d-\\d\\d"))
StringencyData$CountryCode = stringi::stri_sub(StringencyData$key,12,14)
StringencyData$datakey = stringi::stri_sub(StringencyData$key,16,30)

# generate Table containing only stringency, date, country
StringencyData_Subset = StringencyData[StringencyData$datakey == "stringency", c("Date", "CountryCode","value")]
StringencyData_Subset$value = as.numeric(StringencyData_Subset$value)

## Get 2nd data set: New cases data from OurWorldInData.org

# get OurWorldInData data from github
if (!exists("owidorig")) {
  owidquery = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
  message("Query data from ", owidquery)
  owidorig = read.csv(owidquery)
}

# loop over different DelayDates, that is, the shift between the Stringency data and Case data
for (DelayDate in DelayDatesToPlot) {
  
  OurWorldInDataRaw = owidorig
  OurWorldInDataRaw$CountryCode = as.character(OurWorldInDataRaw$iso_code)
  OurWorldInDataRaw$Date = as.Date( OurWorldInDataRaw$date)
  
  
  OurWorldInDataRaw_Subset = OurWorldInDataRaw[,c("Date","new_cases_smoothed_per_million","CountryCode")]
  OurWorldInDataRaw_Subset$oDate = OurWorldInDataRaw_Subset$Date
  OurWorldInDataRaw_Subset$Date = as.Date(OurWorldInDataRaw_Subset$oDate - DelayDate)
  
  # merge the data by date and Country
  MergedData = merge(StringencyData_Subset, OurWorldInDataRaw_Subset, by=c("Date","CountryCode"))
  MergedData = MergedData[order(MergedData$CountryCode, MergedData$Date),]
  MergedData$MonthBegin = MergedData$Date
  
  # define dates for date markers, and add to the data frame where appropriate
  ValDate = as.Date(c("2020-03-01","2020-04-01","2020-06-01","2020-08-01","2020-09-01","2020-10-01"))
  MergedData$MonthBegin [!(MergedData$MonthBegin %in% ValDate)] = NA
  MergedData$EinzelDaten = as.factor(MergedData$MonthBegin)
  
  MergedData$Datum = as.Date(MergedData$Date)
  
  
  # multiply cases by 10 to get cases/100000 
  MergedData$NeuInfektionen = MergedData$new_cases_smoothed_per_million *10
  
  # add data point for Mondays visualization
  MergedData$NeuAmMontag = ifelse(strftime(MergedData$Date,"%u") == 1,MergedData$NeuInfektionen ,NA)
  MergedData$Date = as.POSIXct(MergedData$Date)
  
  # Limit plot data to selected countries  
  CountryCodes = unique(CountryCodes[order(CountryCodes)])
  sub = MergedData[MergedData$CountryCode %in% CountryCodes,]
  
  p= (ggplot(sub, aes(y=NeuInfektionen, x=value))
      + geom_path(aes( group=CountryCode,  col=Datum))
      + scale_color_viridis(trans = "date",direction = -1)
      + geom_point(aes(pch = EinzelDaten, col=Datum))
      + geom_point(aes(y = NeuAmMontag, col=Datum ))
      + facet_wrap( ~  CountryCode,  scales="free_y", labeller = "label_both")
      + xlab ("Staerke der Massnahmen")
      + ylab ("Neuinfektionen / 100.000")
      + scale_y_log10()
      #+ ggtitle(paste0("Alternative Auswertung von Daten von OurWorldInData.org zum Thema Covid-Neuinfektionen in Abh?ngigkeit von Massnahmen\n Neue F?lle / 100.000 / Tag gegen Response Index aufgetragen, mit ",DelayDate, " Tagen Versatz\n Datenquellen: a) https://github.com/OurWorldInDataRaw/covid-19-data/tree/master/public/data\nb) https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker\n (Oxford COVID-19 Government Response Tracker, Blavatnik School of Government.)\n Zur Berechnung der Werte siehe OurWorldInData.org.\nDer Autor dieser Darstellung ist unabhaengig von OurWorldInData.org"))
  )
  grid.arrange(
    p,
    top = "Title of the page",
    bottom = textGrob(
      "Datenquellen: \na) https://github.com/OurWorldInDataRaw/covid-19-data/tree/master/public/data\nb) https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker\n (Oxford COVID-19 Government Response Tracker, Blavatnik School of Government.)\n Zur Berechnung der Werte siehe OurWorldInData.org.\nDer Autor dieser Darstellung ist unabhaengig von OurWorldInData.org",
      gp = gpar(fontface = 3, fontsize = 9),
      hjust = 0,
      x = 0
    )
  )
  print(p)
  
  
  CountryCodeList = paste0(CountryCodes,collapse="_")
  
  dir.create(paste0(basepath,"\\Plots"))
  pngfilename = paste0(basepath,"\\Plots\\Plot_",Sys.Date(),"-",CountryCodeList,"-Delay_",DelayDate,".png")
  png(pngfilename , width = 1300, height = 700)
  print(p)
  cat("wrote ",pngfilename,"\n")
  dev.off()
  
  pdffilename = paste0(basepath,"\\Plots\\Plot_",Sys.Date(),"-",CountryCodeList,"-Delay_",DelayDate,".pdf")
  pdf(pdffilename , width = 16, height = 11)
  print(p)
  cat("wrote ",pdffilename,"\n")
  dev.off()
  
}


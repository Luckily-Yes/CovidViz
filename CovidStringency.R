# CovidStringency.R V 0.1

require(ggplot2)
require(viridis)
require(grid)
require(gridExtra)
require(stringi)
require(stringr)
require(data.table)
require(rjson)

# define base path 

if (Sys.getenv("MyRProjects") == "") {
  stop ("Need to define environment variable MyRProjects")
  basepath = paste0(Sys.getenv("TEMP"),"\\CovidViz")
} else {
  basepath = paste0(Sys.getenv("MyRProjects"),"\\CovidViz")
}

if (! dir.exists(basepath)) {
  dir.create(basepath)
}

# define, which countries to plot
CountryCodes = c("NZL","AUS","DEU","ITA","FRA","BRA", "NLD","GBR","ARG")

DelayDatesToPlot = c(14)


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

StringencyData$StringencyDate = as.Date(stringr::str_extract(StringencyData$key,"^\\d\\d\\d\\d-\\d\\d-\\d\\d"))
StringencyData$CountryCode = stringi::stri_sub(StringencyData$key,12,14)
StringencyData$datakey = stringi::stri_sub(StringencyData$key,16,30)

# generate Table containing only stringency, date, country
StringencyData_Subset = StringencyData[StringencyData$datakey == "stringency", c("StringencyDate", "CountryCode","value")]
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
  
  StringencyData_Subset$Date = as.Date(StringencyData_Subset$StringencyDate + DelayDate)
  
  OurWorldInDataRaw = owidorig
  OurWorldInDataRaw$CountryCode = as.character(OurWorldInDataRaw$iso_code)
  OurWorldInDataRaw$Date = as.Date( OurWorldInDataRaw$date)
  OurWorldInDataRaw$CasesDate = OurWorldInDataRaw$Date
  
  OurWorldInDataRaw_Subset = OurWorldInDataRaw[,c("Date","new_cases_smoothed_per_million","CountryCode","CasesDate")]
  OurWorldInDataRaw_Subset$Date = as.Date(OurWorldInDataRaw_Subset$CasesDate)
  
  # merge the data by date and Country
  MergedData = merge(StringencyData_Subset, OurWorldInDataRaw_Subset, by=c("Date","CountryCode"))
  MergedData = MergedData[order(MergedData$CountryCode, MergedData$Date),]
  MergedData$MonthBegin = MergedData$Date
  
  # add additional data point of last data set for gray point.
  LastDataPoint = merge(StringencyData_Subset, OurWorldInDataRaw_Subset, by.x=c("StringencyDate","CountryCode"), by.y=c("CasesDate","CountryCode"))
  LastDataPoint = LastDataPoint[LastDataPoint$StringencyDate == max(LastDataPoint$StringencyDate), c("StringencyDate", "value", "new_cases_smoothed_per_million","CountryCode")]
  names(LastDataPoint)[1] = "Datum"
  names(LastDataPoint)[2] = "value_IdentityDate"
  MergedData = data.table::rbindlist(list(MergedData, LastDataPoint), fill=T )
  
  # define dates for date markers, and add to the data frame where appropriate
  ValDate = as.Date(c("2020-04-06","2020-05-01","2020-06-08","2020-07-09","2020-10-05"))
  ValDate = c(ValDate, as.Date(max(MergedData$Date, na.rm = T)))
  MergedData$MonthBegin [!(MergedData$MonthBegin %in% ValDate)] = NA
  MergedData$Datum_NeuInfektionen = as.factor(MergedData$MonthBegin)
  
  MergedData$Datum = as.Date(MergedData$Date)
  
  
  # multiply cases by 10 to get cases/100000 
  MergedData$NeuInfektionen = MergedData$new_cases_smoothed_per_million *10
  LastDataPoint$NeuInfektionen = LastDataPoint$new_cases_smoothed_per_million *10
  
  # add data point for Mondays visualization
  MergedData$NeuAmMontag = ifelse(strftime(MergedData$Date,"%u") == 1,MergedData$NeuInfektionen ,NA)
  MergedData$Date = as.POSIXct(MergedData$Date)
  
  # Limit plot data to selected countries  
  CountryCodes = unique(CountryCodes[order(CountryCodes)])
  sub = MergedData[MergedData$CountryCode %in% CountryCodes,]
  
  p= (ggplot(sub, aes(y=NeuInfektionen, x=value))
      + geom_path(aes( group=CountryCode,  col=Datum))
      + scale_color_viridis(trans = "date",direction = -1)
      + geom_point(aes(x = value_IdentityDate, y= NeuInfektionen), col="gray")
      + geom_point(aes(y = NeuAmMontag, col=Datum))
      + geom_point(aes(pch = Datum_NeuInfektionen, col=Datum))
      + facet_wrap( ~  CountryCode,  scales="free_y", labeller = "label_both")
      + xlab ("Stärke der Massnahmen")
      + ylab ("Neuinfektionen / 100.000")
      + scale_y_log10()
      + ggtitle(paste0("Neue Fälle / 100.000 / Tag gegen Response Index aufgetragen, mit ",DelayDate, " Tagen Versatz"))
  )
  print(p)
  
  CountryCodeList = paste0(CountryCodes,collapse="_")
  
  CaptionText = "Datenquellen: \na) https://github.com/OurWorldInDataRaw/covid-19-data/tree/master/public/data\nb) https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker\n (Oxford COVID-19 Government Response Tracker, Blavatnik School of Government.)\n Zur Berechnung der Werte siehe OurWorldInData.org, dies ist eine modifizierte Darstellung von \n https://ourworldindata.org/grapher/government-response-stringency-index-vs-biweekly-change-in-confirmed-covid-19-cases?minPopulationFilter=10000000&time=2020-01-27..2020-11-08&country=DEU~AUS\nDiesen Graphen selbst erzeugen via https://github.com/Luckily-Yes/CovidViz/blob/master/CovidStringency.R\nDer Autor dieser Darstellung ist unabhängig von OurWorldInData.org\n\n"
  CaptionText = paste0(CaptionText, "Was ist zu sehen? Alle Staaten fangen links unten an (keine Fälle, keine Massnahmen)\n Dann steigen die Zahlen: Kurve geht nach oben. \n Dann werden Massnamhen eingeführt: Kurve geht nach rechts\n Daraufhin sinken die Neuinfektionen: Kurve geht nach unten\n Massnahmen werden reduziert: Kurve geht nach links.")
  dir.create(paste0(basepath,"\\Plots"))
  pngfilename = paste0(basepath,"\\Plots\\Plot_",Sys.Date(),"-",CountryCodeList,"-Delay_",DelayDate,".png")
  png(pngfilename , width = 1300, height = 1000)
  grid.arrange(
    p,
    top = "Alternative Auswertung von Daten von OurWorldInData.org zum Thema Covid-Neuinfektionen in Abhängigkeit von Massnahmen",
    bottom = textGrob(
      CaptionText,
      gp = gpar(fontface = 3, fontsize = 9),
      hjust = 0,
      x = 0.05
    )
  )
  
  cat("wrote ",pngfilename,"\n")
  dev.off()
  
  file.copy(pngfilename, paste0(basepath,"\\Plots\\Stringency_vs_NewCases_",Sys.Date(),".png"), overwrite = T)
  
  pdffilename = paste0(basepath,"\\Plots\\Plot_",Sys.Date(),"-",CountryCodeList,"-Delay_",DelayDate,".pdf")
  pdf(pdffilename , width = 16, height = 11)
  grid.arrange(
    p,
    top = "Alternative Auswertung von Daten von OurWorldInData.org zum Thema Covid-Neuinfektionen in Abhängigkeit von Massnahmen",
    bottom = textGrob(
      CaptionText,
      gp = gpar(fontface = 3, fontsize = 9),
      hjust = 0,
      x = 0.05
    )
  )
  cat("wrote ",pdffilename,"\n")
  dev.off()
  file.copy(pdffilename, paste0(basepath,"\\Plots\\Stringency_vs_NewCases_",Sys.Date(),".pdf"), overwrite = T)
  
}


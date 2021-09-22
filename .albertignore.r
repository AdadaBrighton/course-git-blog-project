
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# Your solution code goes here

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

#mynydate = ny[,c("Start.Time")]
#mynydate1 = as.Date(mynydate)
#head(mynydate1)
#class(mynydate1)
#ny_month = format(mynydate1,"%Y-%m")
#ny_month

#mynyColumns = ny[,c("X","Start.Time")]
#head(mynyColumns)
#mynyColumns[,2] = as.Date(mynyColumns[,2])
#names(mynyColumns) = c("X","startdate")
#mynyColumns[,2] = format(mynyColumns[,2],"%Y-%m")
#names(mynyColumns) = c("X","startdate")
#mynyColumns
#library("plyr")
#countsny = ddply(mynyColumns, .(mynyColumns$startdate), nrow)
#names(countsny) = c("startdate","freq")
#countsny

mynyColumns = ny[,c("X","Start.Time")]
head(mynyColumns)
mynyColumns$Start.Time = format(as.Date(mynyColumns$Start.Time),"%Y-%m")
#names(mynyColumns) = c("X","startdate")
#mynyColumns[,2] = format(mynyColumns[,2],"%Y-%m")
names(mynyColumns) = c("X","startdate")

library("plyr")
countsny = ddply(mynyColumns, .(mynyColumns$startdate), nrow)
names(countsny) = c("startdate","freq")
class(countsny$freq)
countsny = countsny[!countsny$startdate=="NA",]

library(ggplot2)
nygraph = ggplot(countsny, aes(x = startdate , y = freq)) +
    geom_bar(stat = "identity", fill = "coral") + theme_classic()

nygraph1 = nygraph + geom_text(aes(label = freq), vjust = 1.5, color = "white", size = 3) + theme_classic()
nygraph2 = nygraph1 + labs(title = "Most Common Month in New York", caption = "Data source: Explore_bikeshare_data")
nygraph2

mywashColumns = wash[,c("X","Start.Time")]
head(mywashColumns)
mywashColumns$Start.Time = format(as.Date(mywashColumns$Start.Time),"%Y-%m")
#names(mywashColumns) = c("X","startdate")
#mywashColumns[,2] = format(mywashColumns[,2],"%Y-%m")
names(mywashColumns) = c("X","startdate")

library("plyr")
countswash = ddply(mywashColumns, .(mywashColumns$startdate), nrow)
names(countswash) = c("startdate","freq")
class(countswash$freq)
countswash = countswash[!countswash$startdate=="NA",]

library(ggplot2)
washgraph = ggplot(countswash, aes(x = startdate , y = freq)) +
    geom_bar(stat = "identity", fill = "blue") + theme_classic()

washgraph1 = washgraph + geom_text(aes(label = freq), vjust = 1.5, color = "white", size = 3) + theme_classic()
washgraph2 = washgraph1 + labs(title = "Most Common Month in Washington", caption = "Data source: Explore_bikeshare_data")
washgraph2

myChiColumns = chi[,c("X","Start.Time")]
head(myChiColumns)
myChiColumns$Start.Time = format(as.Date(myChiColumns$Start.Time),"%Y-%m")
#names(myChiColumns) = c("X","startdate")
#myChiColumns[,2] = format(myChiColumns[,2],"%Y-%m")
names(myChiColumns) = c("X","startdate")

library("plyr")
countsChi = ddply(myChiColumns, .(myChiColumns$startdate), nrow)
names(countsChi) = c("startdate","freq")
class(countsChi$freq)
countsChi[rowSums(is.na(countsChi)) != ncol(countsChi), ] 

library(ggplot2)
Chigraph = ggplot(countsChi, aes(x = startdate , y = freq)) +
    geom_bar(stat = "identity", fill = "Red") + theme_classic()

Chigraph1 = Chigraph + geom_text(aes(label = freq), vjust = 1.5, color = "white", size = 3) + theme_classic()
Chigraph2 = Chigraph1 + labs(title = "Most Common Month in Chicago", caption = "Data source: Explore_bikeshare_data")
Chigraph2



ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

mynydow = ny[,c("X","Start.Time")]
mynydow$Start.Time = weekdays(as.Date(mynydow$Start.Time))
#names(mynydow) = c("X","startdate")
#mynydow[,2] = weekdays(mynydow[,2])
names(mynydow) = c("X","nydow")
library("plyr")
countsnydow = ddply(mynydow, .(mynydow$nydow), nrow)
names(countsnydow) = c("day_of_week","freq")
head(countsnydow)
countsnydow1 = countsnydow[order(countsnydow$day_of_week), ]
head(countsnydow1)


library(ggplot2)
nydowgraph = ggplot(countsnydow1, aes(x = day_of_week , y = freq)) +
    geom_bar(stat = "identity", fill = "Red") + theme_classic()

nydowgraph1 = nydowgraph + geom_text(aes(label = freq), vjust = 1.5, color = "white", size = 3) + theme_classic()
nydowgraph2 =  nydowgraph1 + labs(title = "Most Common Day of the week in New York", caption = "Data source: Explore_bikeshare_data")
nydowgraph2


mywashdow = wash[,c("X","Start.Time")]
mywashdow$Start.Time = weekdays(as.Date(mywashdow$Start.Time))
#names(mywashdow) = c("X","startdate")
#mywashdow[,2] = weekdays(mywashdow[,2])
names(mywashdow) = c("X","washdow")
library("plyr")
countswashdow = ddply(mywashdow, .(mywashdow$washdow), nrow)
names(countswashdow) = c("day_of_week","freq")
head(countswashdow)
countswashdow1 = countswashdow[order(countswashdow$day_of_week), ]
head(countswashdow1)

library(ggplot2)
washdowgraph = ggplot(countswashdow1, aes(x = day_of_week , y = freq)) +
    geom_bar(stat = "identity", fill = "grey") + theme_classic()

washdowgraph1 = washdowgraph + geom_text(aes(label = freq), vjust = 1.5, color = "white", size = 3) + theme_classic()
washdowgraph2 = washdowgraph1 + labs(title = "Most Common Day of the week in washington", caption = "Data source: Explore_bikeshare_data")
washdowgraph2


mychidow = chi[,c("X","Start.Time")]
mychidow$Start.Time = weekdays(as.Date(mychidow$Start.Time))
#names(mychidow) = c("X","startdate")
#mychidow[,2] = weekdays(mychidow[,2])
names(mychidow) = c("X","chidow")
head(mychidow)
library("plyr")
countschidow = ddply(mychidow, .(mychidow$chidow), nrow)
names(countschidow) = c("day_of_week","freq")
head(countschidow)
countschidow1 = countschidow[order(countschidow$day_of_week), ]
head(countschidow1)


library(ggplot2)
chidowgraph = ggplot(countschidow1, aes(x = day_of_week , y = freq)) +
    geom_bar(stat = "identity", fill = "black") + theme_classic()

chidowgraph1 = chidowgraph + geom_text(aes(label = freq), vjust = 1.5, color = "white", size = 3) + theme_classic()
chidowgraph2 = chidowgraph1 + labs(title = "Most Common Day of the week in Chicago", caption = "Data source: Explore_bikeshare_data")
chidowgraph2

y = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library('lubridate')

mynyhod = ny[,c("X","Start.Time")]
mynyhod$Start.Time = format(as.POSIXct(mynyhod$Start.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),"%H")
names(mynyhod) = c("X","nyhod")
library("plyr")
countsnyhod = ddply(mynyhod, .(mynyhod$nyhod), nrow)
names(countsnyhod) = c("Hour_of_the_day","freq")
countsnyhod1 = countsnyhod[order(countsnyhod$Hour_of_the_day), ]
head(countsnyhod1)


library(ggplot2)
nyhodgraph = ggplot(countsnyhod1, aes(x = Hour_of_the_day , y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

nyhodgraph1 = nyhodgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme_classic()
nyhodgraph2 = nyhodgraph1 +  labs(title = "Most Common Hour of the day in New York", caption = "Data source: Explore_bikeshare_data")
nyhodgraph2

mychihod = chi[,c("X","Start.Time")]
mychihod[,2] = format(as.POSIXct(mychihod[,2]),"%H")
names(mychihod) = c("X","chihod")
library("plyr")
countschihod = ddply(mychihod, .(mychihod$chihod), nrow)
names(countschihod) = c("Hour_of_the_day","freq")
countschihod1 = countschihod[order(countschihod$Hour_of_the_day), ]
head(countschihod1)

library(ggplot2)
chihodgraph = ggplot(countschihod1, aes(x = Hour_of_the_day , y = freq)) +
    geom_bar(stat = "identity", fill = "orange") + theme_classic()

chihodgraph1 = chihodgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme_classic()
chihodgraph2 = chihodgraph1 + labs(title = "Most Common Hour of the day in Chicago", caption = "Data source: Explore_bikeshare_data")
chihodgraph2

mywashhod = wash[,c("X","Start.Time")]
mywashhod$Start.Time = format(as.POSIXct(mywashhod$Start.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),"%H")
names(mywashhod) = c("X","washhod")
library("plyr")
countswashhod = ddply(mywashhod, .(mywashhod$washhod), nrow)
names(countswashhod) = c("Hour_of_the_day","freq")
countswashhod1 = countswashhod[order(countswashhod$Hour_of_the_day), ]
head(countswashhod1)
names(countswashhod1) = c("Hour_of_the_day","freq")

library(ggplot2)
countswashhodgraph = ggplot(countswashhod1, aes(x = Hour_of_the_day, y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

countswashhod1 = countswashhodgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme_classic()
countswashhod2 = countswashhod1 + labs(title = "Most Common Hour of the day in washington", caption = "Data source: Explore_bikeshare_data")
countswashhod2

# Your solution code goes here

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

mywashstrst = wash[,c("X","Start.Station")]
library(tidyverse)
countswashstrst = ddply(mywashstrst, .(mywashstrst$Start.Station), nrow)
names(countswashstrst) = c("stations","freq")
wash_top_station0 = countswashstrst %>% arrange(desc(freq))
wash_top_station1 = wash_top_station0 %>% top_n(10)
wash_top_station1

library(ggplot2)
washtopgraph = ggplot(wash_top_station1, aes(x = stations , y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

washtopgraph1 = washtopgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
washtopgraph2 = washtopgraph1 + labs(title = "Top 10 departure stations in washington", caption = "Data source: Explore_bikeshare_data")
washtopgraph2

mychistrst = chi[,c("X","Start.Station")]
library(tidyverse)
countschistrst = ddply(mychistrst, .(mychistrst$Start.Station), nrow)
names(countschistrst) = c("stations","freq")
chi_top_station0 = countschistrst %>% arrange(desc(freq))
chi_top_station1 = chi_top_station0 %>% top_n(10)
chi_top_station1

library(ggplot2)
chitopgraph = ggplot(chi_top_station1, aes(x = stations , y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

chitopgraph1 = chitopgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
chitopgraph2 = chitopgraph1 + labs(title = "Top 10 departure stations in Chicago", caption = "Data source: Explore_bikeshare_data")
chitopgraph2

mynystrst = ny[,c("X","Start.Station")]
library(tidyverse)
countsnystrst = ddply(mynystrst, .(mynystrst$Start.Station), nrow)
names(countsnystrst) = c("stations","freq")
ny_top_station0 = countsnystrst %>% arrange(desc(freq))
ny_top_station1 = ny_top_station0 %>% top_n(10)
ny_top_station1

library(ggplot2)
nytopgraph = ggplot(ny_top_station1, aes(x = stations, y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

nytopgraph1 = nytopgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
nytopgraph2  = nytopgraph1 +  labs(title = "Top 10 departure stations in New York", caption = "Data source: Explore_bikeshare_data")
nytopgraph2


ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

mywashstrst = wash[,c("X","End.Station")]
library(tidyverse)
countswashstrst = ddply(mywashstrst, .(mywashstrst$End.Station), nrow)
names(countswashstrst) = c("stations","freq")
wash_top_station0 = countswashstrst %>% arrange(desc(freq))
wash_top_station1 = wash_top_station0 %>% top_n(10)
wash_top_station1

library(ggplot2)
washtopgraph = ggplot(wash_top_station1, aes(x = stations, y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

washtopgraph1 = washtopgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
washtopgraph2 = washtopgraph1 + labs(title = "Top 10 destinations stations in washington", caption = "Data source: Explore_bikeshare_data")
washtopgraph2

mychistrst = chi[,c("X","End.Station")]
library(tidyverse)
countschistrst = ddply(mychistrst, .(mychistrst$End.Station), nrow)
names(countschistrst) = c("stations","freq")
chi_top_station0 = countschistrst %>% arrange(desc(freq))
chi_top_station1 = chi_top_station0 %>% top_n(10)
chi_top_station1

library(ggplot2)
chitopgraph = ggplot(chi_top_station1, aes(x = stations , y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

chitopgraph1 = chitopgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
chitopgraph2 = chitopgraph1 + labs(title = "Top 10 destinations stations in chicago", caption = "Data source: Explore_bikeshare_data")
chitopgraph2

mynystrst = ny[,c("X","End.Station")]
library(tidyverse)
countsnystrst = ddply(mynystrst, .(mynystrst$End.Station), nrow)
names(countsnystrst) = c("stations","freq")
ny_top_station0 = countsnystrst %>% arrange(desc(freq))
ny_top_station1 = ny_top_station0 %>% top_n(10)
ny_top_station1

library(ggplot2)
nytopgraph = ggplot(ny_top_station1, aes(x = stations , y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

nytopgraph1 = nytopgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
nytopgraph2 = nytopgraph1 + labs(title = "Top 10 destinations stations in New York", caption = "Data source: Explore_bikeshare_data")
nytopgraph2

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library(plyr)
library(tidyverse)

mywash = wash[,c("X","Start.Station","End.Station")]
mywash$start_end = paste(mywash$Start.Station, mywash$End.Station, sep="_")
mywash1 = mywash[,c("X","start_end")]
countswashR = ddply(mywash1, .(mywash1$start_end), nrow)
names(countswashR) = c("routes","freq")
wash_top_routes0 = countswashR %>% arrange(desc(freq))
wash_top_routes1 = wash_top_routes0 %>% top_n(10)
wash_top_routes1

library(ggplot2)
washtopRgraph = ggplot(wash_top_routes1, aes(x = routes, y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

washtopRgraph1 = washtopRgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
washtopRgraph2 = washtopRgraph1 + labs(title = "Common Routes in New York", caption = "Data source: Explore_bikeshare_data")
washtopRgraph2

myny = ny[,c("X","Start.Station","End.Station")]
library(tidyr)
myny1 = unite(myny, start_end, c(End.Station, End.Station))
myny2 = myny1[,c("X","start_end")]
head(myny2)
countsnyR = ddply(myny2, .(myny2$start_end), nrow)
names(countsnyR) = c("routes","freq")
ny_top_routes0 = countsnyR %>% arrange(desc(freq))
ny_top_routes1 = ny_top_routes0 %>% top_n(10)
ny_top_routes1

library(ggplot2)
nytopRgraph = ggplot(ny_top_routes1, aes(x = routes, y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

nytopRgraph1 = nytopRgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
nytopRgraph2 = nytopRgraph1 + labs(title = "Common Routes in New York", caption = "Data source: Explore_bikeshare_data")
nytopRgraph2

mychi = chi[,c("X","Start.Station","End.Station")]
mychi$start_end = paste(mychi$Start.Station, mychi$End.Station, sep="_")
mychi1 = mychi[,c("X","start_end")]
countschiR = ddply(mychi1, .(mychi1$start_end), nrow)
names(countschiR) = c("routes","freq")
chi_top_routes0 = countschiR %>% arrange(desc(freq))
chi_top_routes1 = chi_top_routes0 %>% top_n(10)
chi_top_routes1

library(ggplot2)
chitopRgraph = ggplot(chi_top_routes1, aes(x = routes, y = freq)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

chitopRgraph1 = chitopRgraph + geom_text(aes(label = freq), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
chitopRgraph2 = chitopRgraph1 + labs(title = "Common Routes in Chicago", caption = "Data source: Explore_bikeshare_data")
chitopRgraph2

# Your solution code goes here

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library(lubridate)
head(chi)
mychidur = chi[,c("X","Start.Time","End.Time")]
mychidur$Start.Time = as.POSIXct(mychidur$Start.Time)
names(mychidur) = c("X","starttime","endtime")
mychidur$timediff = as.integer(as.numeric(difftime(mychidur$endtime , mychidur$starttime,tz="GMT", units = "mins")))
names(mychidur) = c("X","starttime","endtime","timediff")
head(mychidur)
print("Total Minutes covered by Chicago residents")
totalminsmoved_chi = sum(mychidur$timediff)
head(totalminsmoved_chi)
print("Total Hours covered by Chicago residents")
totalhoursmoved_chi = as.integer(sum(mychidur$timediff)/60)
head(totalhoursmoved_chi)


head(wash)
mywashdur = wash[,c("X","Start.Time","End.Time")]
mywashdur$Start.Time = as.POSIXct(mywashdur$Start.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
mywashdur$End.Time = as.POSIXct(mywashdur$End.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
names(mywashdur) = c("X","starttime","endtime")
mywashdur$timediff = as.integer(as.numeric(difftime(mywashdur$endtime , mywashdur$starttime,tz="GMT", units = "mins")))
names(mywashdur) = c("X","starttime","endtime","timediff")
head(mywashdur)
print("Total Minutes covered by washington residents")
totalminsmoved_wash = sum(mywashdur$timediff,na.rm=TRUE)
head(totalminsmoved_wash)
print("Total Hours covered by washington residents")
totalhoursmoved_wash = as.integer(sum(mywashdur$timediff,na.rm=TRUE)/60)
head(totalhoursmoved_wash)


head(ny)
mynydur = ny[,c("X","Start.Time","End.Time")]
mynydur$Start.Time = as.POSIXct(mynydur$Start.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
mynydur$End.Time = as.POSIXct(mynydur$End.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
names(mynydur) = c("X","starttime","endtime")
mynydur$timediff = as.integer(as.numeric(difftime(mynydur$endtime , mynydur$starttime,tz="GMT", units = "mins")))
names(mynydur) = c("X","starttime","endtime","timediff")
head(mynydur)
print("Total Minutes covered by washington residents")
totalminsmoved_ny = sum(mynydur$timediff,na.rm=TRUE)
head(totalminsmoved_ny)
print("Total Hours covered by washington residents")
totalhoursmoved_ny = as.integer(sum(mynydur$timediff,na.rm=TRUE)/60)
head(totalhoursmoved_ny)


combinehours = data.frame(cities = c("New York", "Chicago","Washington") , hours = c(totalhoursmoved_ny,totalhoursmoved_chi,totalhoursmoved_wash))
head(combinehours)

library(ggplot2)

hoursgraph = ggplot(combinehours, aes(x = cities , y = hours)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

hoursgraph1 = hoursgraph + geom_text(aes(label = hours), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
hoursgraph2 = hoursgraph1 + labs(title = "Total Hours Moved in New York, Chicago and Washington cities", caption = "Data source: Explore_bikeshare_data")
hoursgraph2

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library(lubridate)
head(chi)
mychidur = chi[,c("X","Start.Time","End.Time")]
mychidur$Start.Time = as.POSIXct(mychidur$Start.Time)
names(mychidur) = c("X","starttime","endtime")
mychidur$timediff = as.integer(as.numeric(difftime(mychidur$endtime , mychidur$starttime,tz="GMT", units = "mins")))
names(mychidur) = c("X","starttime","endtime","timediff")
head(mychidur)
meanChi = as.integer(mean(mychidur$timediff))
print("Total average Minutes covered by Chicago residents")
meanChi

head(wash)
mywashdur = wash[,c("X","Start.Time","End.Time")]
mywashdur$Start.Time = as.POSIXct(mywashdur$Start.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
mywashdur$End.Time = as.POSIXct(mywashdur$End.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
names(mywashdur) = c("X","starttime","endtime")
mywashdur$timediff = as.integer(as.numeric(difftime(mywashdur$endtime , mywashdur$starttime,tz="GMT", units = "mins")))
names(mywashdur) = c("X","starttime","endtime","timediff")
head(mywashdur)
meanwash = as.integer(mean(mywashdur$timediff, na.rm=TRUE))
print("Total average Minutes covered by Washigton residents")
meanwash


head(ny)
mynydur = ny[,c("X","Start.Time","End.Time")]
mynydur$Start.Time = as.POSIXct(mynydur$Start.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
mynydur$End.Time = as.POSIXct(mynydur$End.Time,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())
names(mynydur) = c("X","starttime","endtime")
mynydur$timediff = as.integer(as.numeric(difftime(mynydur$endtime , mynydur$starttime,tz="GMT", units = "mins")))
names(mynydur) = c("X","starttime","endtime","timediff")
head(mynydur)
meanny = as.integer(mean(mynydur$timediff, na.rm=TRUE))
print("Total average Minutes covered by New York residents")
meanny


combinehours = data.frame(cities = c("New York", "Chicago","Washington") , avgmin = c(meanny,meanChi,meanwash))
head(combinehours)

library(ggplot2)

hoursgraph = ggplot(combinehours, aes(x = cities , y = avgmin)) +
    geom_bar(stat = "identity", fill = "green") + theme_classic()

hoursgraph1 = hoursgraph + geom_text(aes(label = avgmin), vjust = 1.5, color = "black", size = 3) + theme(axis.text.x = element_text(angle = 90))
hoursgraph2 = hoursgraph1 + labs(title = "Average Minutes Moved in New York, Chicago and Washington cities", caption = "Data source: Explore_bikeshare_data")
hoursgraph2


system('python -m nbconvert Explore_bikeshare_data.ipynb')

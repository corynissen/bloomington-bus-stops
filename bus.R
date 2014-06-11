
library(tm)
library(stringr)
library(ggmap)
library(ggplot2)
library(ggmap)
library(downloader)
library(XML)
library(httr)
library(RColorBrewer)
library(rCharts)

# dl all files...
url1 <- "http://www.district87.org/pages/Bloomington_School_District_87/Parents_and_Students/Bus_Routes/Bloomington_High_School"
x <- GET(url1)
text <- content(x, as="text")
doc <- htmlParse(text)
a <- xpathSApply(doc, "//table[@id='fsvItemsTable']/tr")
links <- xpathSApply(a[[1]], "//a/@href")
links <- links[grepl("^/files", links)]

for(link in links){
  url1 <- paste0("http://www.district87.org", link)
  download(url1, file.path("data", basename(url1)))  
}

files <- list.files("data")

df <- NULL
for(file in files){
  pdf <- readPDF(PdftotextOptions = "-layout")
  dat <- pdf(elem = list(uri=paste0("data/", file)), language='en', id='id1')
  dat <- gsub(' +', ',', dat)
  out <- read.csv(textConnection(dat), header=FALSE)
  out <- apply(out, 1, function(x)paste(x, collapse=" "))
  tmp.df <- data.frame(route=rep(file, length(out)), od=out)
  df <- rbind(df, tmp.df)
}

# trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
df$od <- trim(df$od)

# first thing should be pick up time. Remove rows if no pick up time
df <- subset(df, grepl("^[0-9]+:[0-9]+ (AM|PM)", df$od))

# parse string to get times, stop location
# starts w/ one or more numbers, colon, one+ number, zero or one space AM or PM
df$pu.time <- str_extract(df$od, "^[0-9]+:[0-9]+( (AM|PM))?")
df$do.time <- str_extract(df$od, "[0-9]+:[0-9]+( (AM|PM))?$")

start <- regexpr("^[0-9]+:[0-9]+( (AM|PM))?", df$od)
end <- regexpr("[0-9]+:[0-9]+( (AM|PM))?$", df$od)
df$stop <- substring(df$od, start+attr(start, "match.length")+1, end-2)
# geocoding was a little off, try subbing @ for and
df$stop <- gsub("@", "and", df$stop)

a <- geocode(paste0(df$stop, ", bloomington IL"))
df$lat <- a$lat
df$lon <- a$lon

save(df, file="data.Rdata")

# all of the stuff from above this line should be run once... saved as an 
# Rdata file, and not run again. 

load("data.Rdata")

# quick look at static map of data
p <- qmap("bloomington IL", zoom=12) 
p + geom_point(aes(x=lon, y=lat), size=3, data=df) + theme_bw() + 
  coord_cartesian(xlim=c(-89.05, -88.90), ylim=c(40.44, 40.51))

# create an interactive leaflet version...
# max of 12 cats in colorbrewer, gotta add 6 more
colors <- brewer.pal(12, "Paired")
colors <- c(colors, "#050505", "#FAF20F", "#FA28EC", "#24E3CD", "#DAFAD4", "#6B6C6E")
df2 <- df
routes <- unique(df2$route)
df2$color <- colors[match(df2$route, routes)]
df2$popup <- paste0("<p>Pick up time:  ", df$pu.time, 
                    "<br>Drop off time:  ", df$do.time, 
                    "<br>", df$stop,
                    "<br>Route Number:  ", str_extract(df2$route, "[0-9]+"), "</p>")
tmp.data <- apply(df2, 1, as.list)

# leaflet map time...
bus.map <- Leaflet$new()
bus.map$setView(c(40.4739, -88.9719), zoom = 13)
bus.map$tileLayer(provider = 'Stamen.TonerLite')
bus.map$geoJson(toGeoJSON(tmp.data, lat = 'lat', lon = 'lon'),
           onEachFeature = '#! function(feature, layer){
           layer.bindPopup(feature.properties.popup)
           } !#',
           pointToLayer =  "#! function(feature, latlng){
           return L.circleMarker(latlng, {
           radius: 5,
           fillColor: feature.properties.color || 'red', 
           color: '#000',
           weight: 1,
           fillOpacity: 0.8
           })
           } !#"           
           )
bus.map$set(width = 1600, height = 800)
bus.map$enablePopover(TRUE)
#bus.map$publish('Bloomington IL Bus Stops', host = 'gist')
bus.map$save("index.html", cdn=T)

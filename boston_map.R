#Crime map Boston using Leaflet
#Author: Pierre Mulliez 
#Source: Kaggle 
#https://www.kaggle.com/AnalyzeBoston/crimes-in-boston?select=crime.csv


#####LOAD DATASET######
path_data_Pierre <- "C:/Users/Pierre Computer/Documents/IE_classes/R_adv/assignment_1_Pierre_Mulliez/crime.csv"
library(leaflet);
bostondata <- data.frame(read.csv(path_data_Pierre))

#Small EDA
head(bostondata)
length(bostondata)
nrow(bostondata)

######SAMPLE THE DATASET#####
#lets select only 2018 data, avoid loading time of the dataset
library(dplyr)
#install.packages("tidyverse")
library(lubridate)
unique(year(bostondata$OCCURRED_ON_DATE))
paste("The dataset size is", as.character(unlist(count(bostondata[year(bostondata$OCCURRED_ON_DATE) == "2018",]))), "rows")
#still quite big, lets get only June 
subboston <- bostondata %>% filter(year(bostondata$OCCURRED_ON_DATE) == "2018" & month(bostondata$OCCURRED_ON_DATE) == "6",)
#given the size of the dataset lets select only monday and sunday
subbostonMondays <- subboston  %>% filter(DAY_OF_WEEK == "Monday" | DAY_OF_WEEK == "Sunday")


####BUILD THE MAP ############
# Plot leaflet map
bos_map <- leaflet(data =  subbostonMondays);


# Add outline
bos_map <- addTiles(bos_map);

#Using piping for remaining layers 
bos_map <- addCircles(bos_map, lng = ~Long, lat = ~Lat);
bos_map

#we can notice some Outliers lets remove them using UDF 
removeoutliers <- function(col,df){
  listquantiles <-  quantile(col, probs = c(0.99,0.01), na.rm = TRUE)
  newdf <- subset(df, col > listquantiles[2] & col < listquantiles[1])
  return(newdf)
}
cleanBoston <- removeoutliers(subbostonMondays$Lat,subbostonMondays)


#tag the shooting as bigger point
cleanBoston$size <- 20
cleanBoston[cleanBoston$SHOOTING == "Y",]$size <- 80
#tag the district with different colors 
dist <- unique(cleanBoston[cleanBoston$DISTRICT != "",]$DISTRICT)
color <- c("green","blue","orange","purple","red","coral","black", "brown","chocolate","grey", "orchid","yellow")
colordf <- data.frame(DISTRICT = dist, col = color)
#merge the color dataframe to our boston df
t <- merge(cleanBoston, colordf, by.x = "DISTRICT", by.y = "DISTRICT")
unique(t$DISTRICT)


####BUILD THE MAP ############
# Plot leaflet map
bos_m <- leaflet(t) %>% addTiles()

#We will have the popup text to equal the offence 

bos_map <- addCircles(bos_m, lng = ~Long, lat = ~Lat, popup = ~OFFENSE_CODE_GROUP, color = ~col, radius = ~size,fill = TRUE,group = ~DISTRICT )
bos_map <- addLegend(bos_map,"bottomright", 
                      labels = colordf$DISTRICT, 
                     colors = colordf$col,
                     title = "Disticts",
                     opacity = 1,
                     group = "Legend") 
bos_map <- addLayersControl(bos_map, overlayGroups = c("Legend",unique(t$DISTRICT)) , options = layersControlOptions(collapsed = FALSE))

bos_map

######LETS DIVE IN DEEPER/ DIFFERENCE IN DAYS AND EVENT TYPE PROPORTION######
#Lets run the same map but have the monday and sunday as layer control to compare the crime commited during those 2 days  !
#color of the day of the week 
t$mcol <- "red"
t[t$DAY_OF_WEEK == "Sunday",]$mcol <- "blue"

#We will analyse more deeply what the proportion of offenses are and add them as popup
t$prop <- paste("offence", toString(0),"%")
t$propint <- 0
unique(t$OFFENSE_CODE_GROUP)
t <- na.omit(t)
for (var in unique(t$OFFENSE_CODE_GROUP)){
  perc <- round(count(t[t$OFFENSE_CODE_GROUP == var,])/nrow(t),digits = 2)
  t[t$OFFENSE_CODE_GROUP == var,]$prop <- paste(var, toString(perc), "%")
  t[t$OFFENSE_CODE_GROUP == var,]$propint <- toString(perc)
}

#We will enlarge the very rare events 
t$evsize <- 20
t[t$propint < 0.05,]$evsize <- 40
t[t$propint < 0.02,]$evsize <- 60

#Set the view for better visibility 
bos_m <- leaflet(t) %>% addTiles() 
bos_map <- addCircles(bos_m, lng = ~Long, lat = ~Lat, popup = ~prop, color = ~mcol, radius = ~evsize,fill = TRUE,group = ~DAY_OF_WEEK )
bos_map <- addLegend(bos_map,"bottomright", 
                     labels = c("Monday","Tuesday"), 
                     colors = c("red","blue"),
                     title = "Days",
                     opacity = 1) %>% addLayersControl(position = "bottomright", overlayGroups = c(unique(t$DAY_OF_WEEK)) , options = layersControlOptions(collapsed = FALSE)) 
bos_map <- setView(bos_map, mean(t$Long),mean(t$Lat),zoom = 12)

bos_map

######APPLY THE MAP TO A SINGLE DISTRICT######

#Highlighting Dudley borough
tb2 <- filter(t, DISTRICT == "B2")
bos_m <- leaflet(tb2) %>% addTiles() 
bos_map <- addCircles(bos_m, lng = ~Long, lat = ~Lat, popup = ~prop, color = ~mcol, radius = ~evsize,fill = TRUE,group = ~DAY_OF_WEEK )
bos_map <- addRectangles(bos_map,lng1 = -71.0870606,lng2 = -71.0826082,lat1 = 42.3311578,lat2 = 42.3282152,label = "Dudley",fillColor = "transparent",color = "green")
bos_map <- addLegend(bos_map,"bottomright", 
                     labels = c("Monday","Tuesday"), 
                     colors = c("red","blue"),
                     title = "Days",
                     opacity = 1) %>% addLayersControl(position = "bottomright", overlayGroups = c(unique(t$DAY_OF_WEEK)) , options = layersControlOptions(collapsed = FALSE)) 
bos_map


#####On step deeper kmean over the B2 distict#####
# Compute k-means with k = 4
set.seed(123)
tk <- select(tb2,Lat, Long)
km.res <- kmeans(tk, 4, nstart = 25)
tkb <- cbind(tb2, cluster = km.res$cluster)

#tag the district with different colors 
clust <- unique(tkb$cluster)
colore <- c("green","blue","orange","purple")
colordf2 <- data.frame(cluster = clust, colr = colore)
#merge the color dataframe to our boston df
tkb <- merge(tkb, colordf2, by.x = "cluster", by.y = "cluster")

####APPLY THE MAP TO THE CLUSTERS
bos_m <- leaflet(tkb) %>% addTiles() 
bos_map <- addCircles(bos_m, lng = ~Long, lat = ~Lat, popup = ~prop,radius = ~evsize, color = ~colr, fill = TRUE)
bos_map <- addLegend(bos_map,"bottomright", 
                     labels = clust, 
                     colors = colore,
                     title = "Clusters",
                     opacity = 1) %>%  setView( mean(tkb$Long),mean(tkb$Lat),zoom = 14)
bos_map




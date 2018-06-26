# Preliminary-interactive-map-Week-4


---
title: "Week 4: preliminary interactive map"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

```{r setup, include=FALSE}

##############################################
## existing tree canopy per SNAD parcel
##############################################

## connect to NYSpatial database
library(sf)
library(RPostgreSQL)
library(dplyr)

host <- '10.14.1.175'

con <- dbConnect(dbDriver("PostgreSQL"), user='postgres', password='treglia2016', host=host, dbname='nycgis_v2')
#con

## first arguement: connection to database; then folder and file name
## read in intersection with TC
treeCover <- st_read(con, c("results_scratch", "snad_parcels_lc_gkd"))

tc_df <- as.data.frame(treeCover)
tc_df$total_pix <- rowSums(tc_df[11:18])

## calculate percent TC per parcel
tc_df$perc_canopy <- (tc_df[,12]/tc_df[,20])*100

## convert back to sf
tc_sf <- st_as_sf(tc_df)
##plot(tc_sf["perc_canopy"],)

##split by borough: SI and BX
BX_tc <- filter(tc_sf, borough=="BX")
SI_tc <- filter(tc_sf, borough=="SI")


##############################################
## existing tree canopy per borough
##############################################

boro_tc <- st_read(con, c("results_scratch", "boroughs_tc_gkd"))

bronx <- filter(boro_tc, boroname=="Bronx")
staten <- filter(boro_tc, boroname=="Staten Island")

##############################################
## existing tree canopy per nta
##############################################

library(flexdashboard)
library(shiny)
library(jsonlite)
library(maptools)
library(tidyr)
library(dplyr)
library(purrr)
library(leaflet)
library(plotly)
library(RPostgreSQL)
library(sf)
library(ggplot2)
library(dplyr)

setwd("C:/Users/gloria.dasanker/Desktop/Readings/GIS/R/MLT_Analytics_samplework/MLT_Analytics_samplework")

## at scale of tabulation area
neighbdata <- st_read("neighborhoodData.geojson")
st_crs(neighbdata) <- 2263 ## telling it what the projection is

## pull out columns
neighbdata.red <- neighbdata[,c("NEIGH_CODE", "Alt_Name", "TreeCanopy.Metrics.NYC.Neighborhoods.Singlepart.Version10C_TC_E_P", "TreeCanopy.Metrics.NYC.Neighborhoods.Singlepart.Version10C_TC_P_P", "TreeCanopy.Metrics.NYC.Neighborhoods.Singlepart.Version10C_TC_Pi_P", "TreeCanopy.Metrics.NYC.Neighborhoods.Singlepart.Version10C_TC_Pv_P")]

#NOTE - Potential Canopy Should be Existing Canopy + P_P + Pi_P + Pv_P

## renaming columns
## potential total canopy is existing canopy + land that could be canopy
names(neighbdata.red)[3:6] <- c("ExistingPctCanopy", "PotentialPctCanopy_Ttl", "PotentialPctCanopy_Imperv", "PotentialPctCanopy_Veg")
head(neighbdata.red)
neighbdata.red[][[3]] <- round(neighbdata.red[][[3]], 1)
neighbdata.red[][[4]] <- round(neighbdata.red[][[4]], 1)
neighbdata.red[][[5]] <- round(neighbdata.red[][[5]], 1)
neighbdata.red[][[6]] <- round(neighbdata.red[][[6]], 1)



```

Page 1
================================

Column {data-width=650}
-----------------------------------------------------------------------


### Existing and Percent canopy interactive
```{r}

library(plotly)
library(leaflet)
library(crosstalk)
library(rgdal)
library(sp)
library(DT)

## javascript library(D3)
## plotly is based on d3 and is used for visualization
p <- plot_ly(neighbdata.red, x = ~ExistingPctCanopy, y = ~PotentialPctCanopy_Ttl) %>% add_markers(alpha = 0.5,text = ~paste(Alt_Name)) 


#%>%

               
               #hoverinfo = 'text',
               #text = ~paste(boroughs$boroname)) %>%
  #highlight("plotly_selected", dynamic = TRUE)


## transforms data
## creates the leaflet map
## customization

## interactive map!
map <- st_transform(neighbdata.red,crs = 4326) %>% leaflet() %>% 
   addTiles() %>% addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
     opacity = 1.0, fillOpacity = 0.5,
     fillColor = ~colorQuantile("Greens", ExistingPctCanopy)(ExistingPctCanopy), highlightOptions = highlightOptions(color = "white", weight = 2,
       bringToFront = TRUE))


###
## creating data table
sd_neigh <- SharedData$new(neighbdata.red)

neigh_centroids <- st_transform(st_centroid(neighbdata.red),crs = 4326)

sd_neigh <- SharedData$new(neigh_centroids)

test <- bscols(widths=c(7, 6, 6), device="md",
               list(filter_slider("ExistingPctCanopy", "Existing Percent Canopy", sd_neigh,
                                  ~ExistingPctCanopy, step=10, round=1, sep=""),
                    filter_slider("PotentialPctCanopy_Ttl", "Potential Percent Canopy", sd_neigh,
                                  ~PotentialPctCanopy_Ttl, step=10, round=1)),
              leaflet(sd_neigh) %>% 
              addProviderTiles(providers$CartoDB.Positron) %>%
                  addPolygons(data=st_transform(neighbdata.red, crs=4326),color = "#444444", weight = 1, 
                            smoothFactor = 0.5,
                            opacity = 1.0, 
                            fillOpacity = 0.5,
                            fillColor = ~colorQuantile("Greens", ExistingPctCanopy)(ExistingPctCanopy), 
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = FALSE), group="Existing Percent Canopy") %>% 
                addPolygons(data=st_transform(neighbdata.red, crs=4326),color = "#444444", weight = 1, 
                            smoothFactor = 0.5,
                            opacity = 1.0, 
                            fillOpacity = 0.5,
                            fillColor = ~colorQuantile("Greens", 
                                                       PotentialPctCanopy_Ttl)(PotentialPctCanopy_Ttl), 
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = FALSE), group="Potential Percent Canopy") %>% 
              addCircleMarkers(radius=5, stroke=TRUE, color="yellow", weight=1,fillColor = "#03F",
                               opacity=1,fillOpacity=1, group="pts",
                               popup=~as.character(Alt_Name)) %>%
              addLayersControl(baseGroups = c("Existing Percent Canopy", "Potential Percent Canopy"),overlayGroups="Neighborhoods",
                              options = layersControlOptions(collapsed = FALSE)),
              plot_ly(sd_neigh, x = ~ExistingPctCanopy, y = ~PotentialPctCanopy_Ttl) %>% 
                  add_markers(alpha = 0.5,text = ~Alt_Name) %>% 
                  layout(xaxis = list(title="Existing % Canopy (2010)"), 
                         yaxis = list(title="Potential % Canopy")) %>%
                  highlight("plotly_hover", off='plotly_deselect', 
                            opacityDim=getOption("opacityDim",  0.2)))

test
```


Column {data-width=350}
-----------------------------------------------------------------------

### Existing Percent Canopy Cover for New York City
```{r}
plot(neighbdata.red["ExistingPctCanopy"],)

```


### Panel c
```{r}
plot(neighbdata.red["PotentialPctCanopy_Ttl"],)

```

Page 2
================================

Row
-----------------------------------------------------------------------


### The Bronx Percent Existing Canopy per SNAD Parcel
```{r}

plot(BX_tc["perc_canopy"],)

```

### Development in the Bronx on SNAD parcels (scenario 1 25% loss)
```{r}
plot(BX_tc["scen1"],)

```

Row
-----------------------------------------------------------------------

### Staten Island Percent Existing Canopy per SNAD Parce
```{r}
plot(SI_tc["perc_canopy"],)
```

### Development in Staten Island on SNAD parcels (scenario 1 25% loss)
```{r}
plot(SI_tc["scen1"],)

```


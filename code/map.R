devtools::install_github("dkahle/ggmap") 
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggmap)
library(htmltools)
library(htmlwidgets)

#Download files to data folder and create dataframes 
URL <- "https://data.texas.gov/api/views/bc5r-88dy/rows.csv?accessType=DOWNLOAD"
download.file(URL, destfile = "./data/operation.csv", method="curl")
operation_df <- read.csv("./data/operation.csv")
rm(URL)


URL <- "https://data.texas.gov/api/views/tqgd-mf4x/rows.csv?accessType=DOWNLOAD"
download.file(URL, destfile = "./data/noncompliance.csv", method="curl")
noncompliance_df <- read.csv("./data/noncompliance.csv")
rm(URL)

# Merge dataframes by Operation_ID
merged_df <- merge(operation_df, noncompliance_df, by="OPERATION_ID")

#Filter El Paso, Texas and write it to a .csv
filter_df <- filter(merged_df, COUNTY == "EL PASO")
write.csv(filter_df, file="data/elpaso.csv")
# Read in the CSV data and store it in a variable 
origAddress <- read.csv("./data/elpaso.csv", stringsAsFactors = FALSE)

#Geocode the addresses so we can put it in a map
#taken from http://www.storybench.org/geocode-csv-addresses-r/
# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)
# Register API key
register_google(key = "XXXXX")
# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$LOCATION_ADDRESS[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}
# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "./data/geocoded.csv")

#Put csv into dataframe
geocoded <- read.csv("./data/geocoded.csv")
# Select the joined dataset with geocoding and select a few columns. 
df = select(geocoded, 
            OPERATION_NAME, PHONE_NUMBER, WEBSITE_ADDRESS, DEFICIENCY_HIGH,
            DEFICIENCY_MEDIUM, DEFICIENCY_LOW, lat, lon) %>% 
  unique()

# Create color codes based off how many high deficiencies are found. 
getColor <- function(df) {
  sapply(df$DEFICIENCY_HIGH, function(DEFICIENCY_HIGH) {
    if(DEFICIENCY_HIGH >= 5) {
      "green"
    } else if(DEFICIENCY_HIGH <= 6) {
      "orange"
    } else {
      "red"
    } })
}

# choose icons
icons <- awesomeIcons(
  icon = 'users',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(df)
)
# Create function for color legend. 
#If there's a way to avoid doing this and just using getColor I'd be happy to know .
pal <- colorBin(c("green", "orange", "red"), domain = df$DEFICIENCY_HIGH, 3, pretty = FALSE)

heatPlugin <- htmlDependency("Leaflet.heat", "99.99.99",
                             src = c(href = "http://leaflet.github.io/Leaflet.heat/dist/"),
                             script = "leaflet-heat.js"
) 
registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map}


# creating the map
leaflet(df) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addAwesomeMarkers(lng = ~lon, lat = ~lat, 
                    icon=icons, 
                    group = 'df', label = df$OPERATION_NAME,
             popup = ~paste("<b>Name:</b>", OPERATION_NAME, "<br>",
                            "<b>Phone Number:</b>", PHONE_NUMBER, "<br>",
                            "<b>Website:</b>", WEBSITE_ADDRESS, "<br>",
                            "<b>High Deficiancy:</b>", DEFICIENCY_HIGH, "<br>",
                            "<b>Medium Deficiancy:</b>", DEFICIENCY_MEDIUM, "<br>",
                            "<b>Low Deficiancy:</b>", DEFICIENCY_LOW, "<br>")) %>% 
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))
  ) %>% 
  addSearchFeatures(
    targetGroups = 'df',
    options = searchFeaturesOptions(
      zoom=12, initial= TRUE, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = F, minLength = 2)
  ) %>% 
  addLegend("bottomright", pal = pal, values = df$DEFICIENCY_HIGH,
            title = "Number of high deficiencies", opacity = 9) 




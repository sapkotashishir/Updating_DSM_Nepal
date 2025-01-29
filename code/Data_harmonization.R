# read and inspect the datasets 
library(sf)
library(ggplot2)
library(knitr)
library(dplyr)
library(readxl)
setwd("C:/Users/SSAPKOTA/OneDrive - CIMMYT/DSM/DSM_update/Updating_DSM")
narc_79_80 <- read_excel('./data/soil_samples/100.DSM data_2079.80 _sent.xlsx')
narc_80_81 <- read_excel('./data/soil_samples/100.DSM Data 2080.81_final_sent.xlsx')
nsaf_2024 <- read.csv('./data/soil_samples/NSAF_soil_data_2024.csv')

narc_79_80 <- narc_79_80 %>% 
  rename(S.No = 'S.N',
      Altitude = 'Altitude (m asl)', 
      TN = "N%", 
      OM = "OM%",
      P205 = "P2O5 kg/ha" , 
      K2O = "K2O kg/ha",
      Zn = "Zn (ppm)" ) %>% 
  mutate(
    B = as.numeric(NA),
    Sand = as.numeric(NA),
    Silt = as.numeric(NA),
    Clay = as.numeric(NA)
  ) %>%
  select(pH,TN,OM,P205,K2O,Zn,B,Sand,Silt,Clay,Latitude,Longitude,Altitude)

narc_80_81 <- narc_80_81 %>%
  rename (
         Altitude = "Alt",
         Latitude = "Lat" ,
         Longitude = "Long",
         TN = "N", 
         P205 = "P" , 
         K2O = "K") %>% 
  mutate(
  Zn = as.numeric(NA),
  B = as.numeric(NA),
  Altitude = as.numeric(Altitude),
  Longitude = as.numeric(Longitude), 
  Latitude = as.numeric(Latitude)
    ) %>%
  select(pH,TN,OM,P205,K2O,Zn,B,Sand,Silt,Clay,Latitude,Longitude,Altitude)

nsaf_2024 <- nsaf_2024 %>%
  rename (
    Altitude  = 'altitude', 
    Latitude = 'latitude',
    Longitude = "longitude",
    TN = "TN..", 
    OM = "O.C.." ,
    P205 = "P2O5.ppm" ,
    K2O = "K2O.ppm",
    Sand = "Sand..",                
    Silt = "Silt.." ,                
    Clay = "Clay.."
  )%>% mutate (
    Zn = as.numeric(NA),
    B = as.numeric(NA),
    #convert P205 and K20 from ppm to kg/ha 
    K2O = K2O*2.0,
    P205 = P205*2.0, 
    OM = OM*1.72
  )%>%  select(pH,TN,OM,P205,K2O,Zn,B,Sand,Silt,Clay,Latitude,Longitude,Altitude)


# merge these three data sets 
combined_data <- bind_rows(narc_79_80,narc_80_81,nsaf_2024)

# Convert Latitude and Longitude to numeric and round to 6 decimals (if needed)
combined_data$Latitude <- as.numeric(round(combined_data$Latitude, 6))
combined_data$Longitude <- as.numeric(round(combined_data$Longitude, 6))

# Check for duplicates BEFORE removing them
combined_duplicates_before <- combined_data[duplicated(combined_data[, c('Latitude', 'Longitude')]) | 
                                            duplicated(combined_data[, c('Latitude', 'Longitude')], fromLast = TRUE), ]
print(nrow(combined_duplicates_before))  # Should print 13

# Remove duplicates
combined_datas <- combined_data[!duplicated(combined_data[, c('Latitude', 'Longitude')]), ]

# Check for duplicates AFTER removal (should be 0)
combined_duplicates_after <- combined_datas[duplicated(combined_datas[, c('Latitude', 'Longitude')]) | 
                                            duplicated(combined_datas[, c('Latitude', 'Longitude')], fromLast = TRUE), ]
print(nrow(combined_duplicates_after))  


# now let us have the district and palika level data joined into the combined dataset
# import the Nepal palika shape file 

Nepal_shp <- st_read('./data/Nepal_admin/Nepal_Districts.shp')
#print(str(Nepal_shp))

# Convert combined datas sf features using Longitude and Latitude values 
# filtering NA in those columns 

combined_sf <- combined_datas %>% 
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#print(str(combined_sf))
ggplot() + 
  geom_sf(data =  Nepal_shp, fill = NA, color = "black") + 
  geom_sf(data = combined_sf, color = "red", size =0.5)+ 
  geom_sf_text(data = Nepal_shp, aes(label = DISTRICT ), size = 2, color = "blue") +
  theme_minimal()+ 
  labs(title = "Distribution of Soil data points over Nepal Palika Boundaries")


# spatial join the district names to the soil data 

# ensure both datasets have same crs
combined_sf <- st_transform(combined_sf, crs = st_crs(Nepal_shp))
combined_sf <- st_join(combined_sf, Nepal_shp[,c('DISTRICT')], left =TRUE)

# drop points with district NA values 

combined_sf <- combined_sf%>%
  filter(!is.na(DISTRICT))

# export the combined_sf for further exploratory data analysis 
st_write(combined_sf, './data/harmonized_data.shp', delete_layer = TRUE)

# Count the data based on each district and soil property
soil_properties <- c("pH","TN","OM","P205","K2O","Sand","Silt","Clay")  # Example properties

combined_data_sf <- combined_sf

# Count the number of non-NA values for each soil property in each district
combined_counts <- combined_data_sf %>%
  st_drop_geometry() %>%
  group_by(DISTRICT) %>%
  summarise(across(all_of(soil_properties), ~ sum(!is.na(.)), .names = "count_{.col}"), .groups = "drop")

# Print the result
print(combined_counts)

# now we move on to exploratory data analysis for the new datasets. 



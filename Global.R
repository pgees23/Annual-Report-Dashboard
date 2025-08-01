# Specify the path to GDAL data ----
#Sys.setenv("GDAL_DATA" = "/usr/gdal34/share/gdal")

#Load geospatial libraries
#these need to be on when deploying dashboard locally but commented out
#when publishing to password protected or non-password protected dashboard
#dyn.load("/usr/gdal34/lib/libgdal.so")
#dyn.load("/usr/geos310/lib64/libgeos_c.so", local = FALSE)

################
library(dplyr)
library(readxl)
library(shiny)
library(plotly)
library(ggplot2)
library(shinydashboard)
library(shinymanager)
library(htmlwidgets)
library(vembedr)
library(haven)
library(openxlsx)
library(lubridate)
library(jsonlite)
library(httr)
library(janitor)
library(tidyverse)
library(eeptools)
library(DT)
library(scales)
library(ggrepel)
library(memisc)
#dyn.load("/usr/gdal34/lib/libgdal.so")
#dyn.load("/usr/geos310/lib64/libgeos_c.so", local = FALSE)
library(sf)
library(terra)
library(sp)
library(raster)
library(leaflet)
# library(magick)
# library(pdftools)

# File Locations:----
#File_path <- "/conf/quality/msaudit/Active/(04) Project Reports/Annual Reports/2025/Tableau/Datafiles/"
File_path <- "Data/2025/"

### Read in Shapefile:----
#file_location_shape <- "/conf/quality/msaudit/Active/(04) Project Reports/Annual Reports/"  
#file_location_shape <- "/conf/quality/msaudit/Active/Template/"
Intro_Shape <- read_sf(dsn = "ShapeFiles/", layer = "HBShapeFile")
Intro_Shape <- st_transform(Intro_Shape, 4326)


### Read files into Dash board----
agegenderrate <- read_xlsx(paste0(File_path,"agegenderrate_v2.xlsx"))
diag2cont <- read_xlsx(paste0(File_path, "diag2cont.xlsx"))
investigations <- read_xlsx(paste0(File_path, "investigations_v2.xlsx"))
primref2diag <- read_xlsx(paste0(File_path, "primref2diag.xlsx"))
DMTs <- read_xlsx(paste0(File_path, "DMTs_v2.xlsx"))
incidence_type <- read_xlsx(paste0(File_path, "incidence_type.xlsx")) 
DMTs_MSType <- read_xlsx(paste0(File_path, "DMTs_MSType_v2.xlsx"))
dobgender <- read_xlsx(paste0(File_path, "dobgender_v2.xlsx"))
Top_five_causes <- read_xlsx(paste0(File_path, "Top_five_causes.xlsx"))
incidence_numbers <- read_xlsx(paste0(File_path, "incidence_numbers.xlsx"))
MS_Deaths <- read_xlsx(paste0(File_path,"MS_Deaths.xlsx"))
Hospital_coordinates <- read_csv(paste0(File_path,"Hospital_coordinates.csv"))
funnelRates <- read_xlsx(paste0(File_path,"FunnelRates.xlsx"))
lkup <- read_xlsx(paste0(File_path,"lkup.xlsx"))
deprivation <- read_xlsx(paste0(File_path,"SIMD 2025.xlsx"))
COMPLIANCE_SCOT <- sum(funnelRates$N_BREAK)/sum(funnelRates$N_TOTAL)
Data_standard <- diag2cont %>% 
  filter(Time == "<=2 weeks", HB == "Scotland") %>% 
  group_by(year)
Data_standard_p <- Data_standard %>% 
  mutate(STD_PC_round = round(STD_PC,1),
         STD_PC_round_char =paste0(as.character(STD_PC_round),"%"))
acknowledgements <- read_xlsx(paste0(File_path,"Acknowledgements.xlsx"))
glossary <- read_xlsx(paste0(File_path,"SMSR Glossary.xlsx"))

### Calculate Scotland rate
Scotland_Incidence_box <- incidence_numbers %>% 
  filter(Hblabel == "Scotland") %>% 
  filter(Year >= (max(Year)  - 4)) %>% 
  group_by(Hblabel) %>%  # Group by Health Board
  summarise(Average_Incidence = mean(Rate, na.rm = TRUE)) # Calculate average incidence


### PHS Colour Schemes ----
phs_pu1 <- "#3F3685"
phs_ma1 <- "#9B4393"
phs_ma50 <- "#AF69A9"
phs_bl1 <- "#0078D4"
phs_gre1 <- "#83BB26"
phs_gra1 <- "#948DA3"
phs_te1 <- "#1E7F84"
phs_li1 <- "#6B5C85"
phs_ru1 <- "#C73918"

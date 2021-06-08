#GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install/Load libraries
library(utils)
if (!require("pacman")) install.packages("pacman")
# pacman::p_load(devtools, usethis, shiny, shinyjs, rgdal, dplyr, leaflet, highcharter, zoo, ggplot2, rgeos, classInt, geosphere,
#                shinythemes, sf, purrr, shinydashboard, readxl, DT, formattable, tibble, curl, sp, stringr, shinyWidgets,
#                leaflet.extras, kableExtra, tidytidbits, data.table, openxlsx, sass)
packages <- c("devtools", "usethis", "shiny", "shinyjs", "rgdal", "dplyr", "leaflet", "highcharter", "zoo", "ggplot2", "rgeos", "classInt", "geosphere", "shinythemes", "sf", "purrr", "shinydashboard",
              "readxl", "DT", "formattable", "tibble", "curl", "sp", "stringr", "shinyWidgets", "leaflet.extras", "kableExtra", "tidytidbits", "data.table", "openxlsx",
                  "sass", "Matrix", "robustbase", "rgl", "minpack.lm", "googlesheets", "tidyselect", "lubridate", "plyr", "tidyr", "stats", "graphics", "grDevices","datasets", "methods")
packages1 <- c("devtools", "shiny", "shinyjs", "rgdal", "dplyr", "leaflet", "highcharter", "zoo", "ggplot2", "rgeos", "geosphere",
              "shinythemes", "sf", "purrr", "shinydashboard", "readxl", "DT", "formattable", "tibble", "curl", "sp", "stringr", "shinyWidgets",
              "leaflet.extras", "kableExtra", "tidytidbits", "data.table", "openxlsx", "tidyr", "sass")

pacman::p_load(char = packages1)
# p_load_gh("mabafaba/reachR")
# other.packages <- c("googlesheets", "reachR", "qpcR", "Matrix", "robustbase", "rgl", "minpack.lm", "MASS", "tidyselect",
#                     "lubridate", "plyr", "tidyr", "stats", "graphics", "grDevices", "utils", "datasets", "methods")

# #install packages
# library(devtools)
# library(usethis)
# library(shiny)
# library(shinyjs)
# library(rgdal)
# library(dplyr)
# library(leaflet)
# library(highcharter)
# library(zoo)
# library(ggplot2)
# library(rgeos)
# library(classInt)
# library(geosphere)
# library(shinythemes)
# library(sf)
# library(purrr)
# library(shinydashboard)
# library(readxl)
# library(DT)
# library(formattable)
# library(tibble)
# library(curl)
# library(sp)
# library(stringr)
# library(shinyWidgets)
# library(leaflet.extras)
# library(kableExtra)
# library(tidytidbits)
# library(data.table)
# library(openxlsx)
# # remove.packages("sass")
# # install.packages("sass")
# library("sass")
# # packageVersion("sass") 

smeb <- data.frame(SMEB = c(rep("SMEB Wash", 4), rep("SMEB Wash", 5)),                                  # define SMEB content table
                   Category = c(rep("Non-Food Items", 3), "Water", rep("Food Items", 5)),
                   Item = c("Soap", "Laundry powder", "Sanitary Napkins", "Cubic meter water",
                            "Wheat flour", "Beans dry","Vegetable oil", "Sugar", "Salt"),
                   Quantity = c("10.5 ", "20 Kg", "2 Boxes", "3.15 m^3", "7.5 Kg", "10 ", "8 ", "2.5 ", "1Kg"))

# smeb_kbl <- smeb %>%                                                                                      # make a html (kable) object out of dataframe
#   filter(SMEB=="SMEB Wash") %>% select(-SMEB) %>%
#   kbl(escape = F) %>%
#   kable_styling(bootstrap_options = c("hover", "condensed", "striped"), fixed_thead = T, full_width = F) %>%
#   column_spec(1, width = "8em", bold = T, background = "white") %>%
#   column_spec(2, width = "10em") %>%
#   column_spec(3, width = "8em") %>%
#   collapse_rows(columns = 1, valign = "top") %>%
#   row_spec(0:17, extra_css = "font-size: 11px;")

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

##-------------------------- TABULAR DATA WRANGLE ----------------------

# Full district database for data explorer KII dataset 

full_data <- read.csv("data/data_all.csv") %>%
  dplyr::select(-matches("district_au|cash_feasibility|market_|_source|exchange_rate_market.|type_market|_other|infra|mrk_supply_issues")) %>%
  setnames(old=c("jmmi_date","governorate_name","governorate_id","district_name","district_id","calc_price_wheat_flour","calc_price_rice","calc_price_beans_dry","calc_price_beans_can","calc_price_lentil","calc_price_vegetable_oil","calc_price_sugar","calc_price_salt","calc_price_potato","calc_price_onion","calc_price_petrol","calc_price_diesel","calc_price_bottled_water","calc_price_treated_water","calc_price_soap","calc_price_laundry","calc_price_sanitary","cost_cubic_meter","exchange_rate_result"),
           new=c("Date","Governorate","government_ID","District","district_ID","wheat_flour","rice","beans_dry","beans_can","lentil","vegetable_oil","sugar","salt","potato","onion","petrol","diesel","bottled_water","treated_water","soap","laundry_powder","sanitary_napkins","cost_cubic_meter","exchange_rates")) %>%
  dplyr::mutate(WASH_SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15)) %>% round(.,0),
                Food_SMEB = as.numeric((wheat_flour*75+beans_dry*10+vegetable_oil*8+sugar*2.5+salt)) %>% round(.,0), .before="wheat_flour")%>%
  dplyr::mutate(Date=as.Date(as.yearmon(Date))) %>%
  dplyr::rename("mrk_increse_food_100" = mrk_increse_food_100,
                "mrk_increse_food_50" = mrk_increse_food_50,
                "mrk_increse_fuel_100" = mrk_increse_fuel_100,
                "mrk_increse_fuel_50" = mrk_increse_fuel_50,
                "mrk_increse_wash_100" = mrk_increse_wash_100,
                "mrk_increse_wash_50" = mrk_increse_wash_50,
                "mrk_increse_water_100" = mrk_increse_water_100,
                "mrk_increse_water_50" = mrk_increse_water_50,
                "mrk_supply_routes" = mrk_supply_routes) %>%
  rename_with(~paste0("% of traders reporting selling ", gsub("_", " ", gsub("sell_", "", .))), matches("sell_"))

# full_data <- read.csv("data/data_all.csv")%>%
#   dplyr::select(c("jmmi_date","governorate_name","governorate_id","district_name","district_id","calc_price_wheat_flour","calc_price_rice","calc_price_beans_dry","calc_price_beans_can","calc_price_lentil","calc_price_vegetable_oil","calc_price_sugar","calc_price_salt","calc_price_potato","calc_price_onion","calc_price_petrol","calc_price_diesel","calc_price_bottled_water","calc_price_treated_water","calc_price_soap","calc_price_laundry","calc_price_sanitary","cost_cubic_meter","exchange_rate_result"))%>%
#   setNames(c("Date","Governorate","government_ID","District","district_ID","wheat_flour","rice","beans_dry","beans_can","lentil","vegetable_oil","sugar","salt","potato","onion","petrol","diesel","bottled_water","treated_water","soap","laundry_powder","sanitary_napkins","cost_cubic_meter","exchange_rates"))%>%
#   dplyr::mutate(WASH_SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15)),
#          Food_SMEB = as.numeric((wheat_flour*75+beans_dry*10+vegetable_oil*8+sugar*2.5+salt)), .before="wheat_flour")%>%
#   dplyr::mutate(Date=as.Date(as.yearmon(Date)))

Admin1data <- read.csv("data/governorate_interactive.csv") %>% as_tibble() %>% dplyr::select(-X) %>%
  dplyr::mutate(WASH_SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15)) %>% round(.,0),
                Food_SMEB = as.numeric((wheat_flour*75+beans_dry*10+vegetable_oil*8+sugar*2.5+salt)) %>% round(.,0))                        #The SMEB calculation
Admin2data <- read.csv("data/district_interactive.csv") %>% as_tibble()%>% dplyr::select(-X) %>% 
  dplyr::mutate(WASH_SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15)) %>% round(.,0),
                Food_SMEB = as.numeric((wheat_flour*75+beans_dry*10+vegetable_oil*8+sugar*2.5+salt)) %>% round(.,0))                        #The SMEB caluclation
AdminNatData <- read.csv("data/national_interactive.csv") %>% as_tibble() %>% dplyr::select(-X) %>% 
  dplyr::mutate(WASH_SMEB = as.numeric((soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15)) %>% round(.,0),
                Food_SMEB = as.numeric((wheat_flour*75+beans_dry*10+vegetable_oil*8+sugar*2.5+salt)) %>% round(.,0))                        #The SMEB caluclation)

max_date <- max(as.Date(as.yearmon(AdminNatData$date)))  

#Wrangle Data into appropriate formats
#Governorates
Admin1table<-as.data.frame(Admin1data)
Admin1table$date2<- as.Date(Admin1table$date, format("%d-%b-%y"), tz="UTC")
Admin1table$date2 <- as.Date(as.yearmon(Admin1table$date))

Admin1data_current <- Admin1table %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max_date)
currentD <- as.character(format(max(Admin1table$date2),"%B %Y")) #define current date for disply in dashboard
Admin1table[4:14] <- sapply(Admin1table[4:14], as.numeric)

#Districts
Admin2table <- as.data.frame(Admin2data)
Admin2table$date2 <- as.Date(as.yearmon(Admin2table$date))

Admin2data_current <- Admin2table %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2))%>%
  filter(date2 == max_date)
currentD <- as.character(format(max(Admin2table$date2),"%B %Y")) #define current date for disply in dashboard
Admin2table[7:16] <- sapply(Admin2table[7:16], as.numeric)

#National
AdminNatTable<-as.data.frame(AdminNatData)
AdminNatTable$date2 <- as.Date(as.yearmon(AdminNatTable$date))

AdminNatData_current <- AdminNatTable %>% #subset only recent month dates to attach to shapefile
  arrange(desc(date2)) %>%
  filter(date2 == max_date)
currentD <- as.character(format(max(AdminNatTable$date2),"%B %Y"))
  #define current date for display in dashboard

# Price long data for Plot tab
prices_long <- Admin2table %>%
  dplyr::select(date2, government_name:district_ID, everything(), -date, -government_ID, -district_ID) %>%
  dplyr::rename(Date=date2, Governorate=government_name, District=district_name) %>%
  tidyr::pivot_longer(cols = 4:ncol(.)) %>%
  dplyr::rename(Item=name, Price=value)

# Full district database for data explorer + Plot tab
indicators <- read.xlsx("data/market functionnality indicators.xlsx", check.names = F) %>%
  setnames(gsub("_", " ", gsub("\\.", " ", colnames(.))))
indicators_long <- indicators %>%
  tidyr::pivot_longer(cols = 4:ncol(.)) %>%
  dplyr::rename(Item=name, Price=value)
prices_long <- prices_long %>% rbind(indicators_long)                           ## For the plot tab

data <- Admin2table %>%                                                         ## for the data explorer tab
  dplyr::rename(Date=date, Governorate=government_name, District=district_name) %>%
  left_join(indicators, by = c("Date", "Governorate", "District"))

##-------------------------- SPATIAL DATA WRANGLE ----------------------
#Read in shapefiles
Admin1<- readOGR("./www", "YEM_adm1_Governorates")
Admin2<- readOGR("./www", "YEM_adm2_Districts")

Admin1@data$admin1name<-gsub("Amanat Al Asimah", "Sana'a City", Admin1@data$admin1name)
Admin1@data$admin1refn<-gsub("Amanat Al Asimah", "Sana'a City", Admin1@data$admin1refn)
Admin2@data$admin1name<-gsub("Amanat Al Asimah", "Sana'a City", Admin2@data$admin1name)
Admin2@data<- Admin2@data %>% dplyr::mutate_if(is.factor, as.character) 


##-------------------------- COMBINE TABULAR & SPATIAL DATA----------------------
#Merge data from Google Sheet with Rayon shp file
Rshp <- merge(x=Admin2,y=Admin2data_current, by.x="admin2pcod", by.y= "district_ID")

DistsNumb<-sum(!is.na(Rshp@data$district_name)) #get number of districts covered...

Rshp<-st_simplify(st_as_sf(Rshp), dTolerance = 0.5)
Rshp <- st_transform(x = Rshp, 
                     crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Rshp<-as(Rshp,"Spatial")


Admin1<-st_simplify(st_as_sf(Admin1), dTolerance = 0.5)
Admin1<- st_transform(x = Admin1, 
                      crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Admin1<-as(Admin1,"Spatial")

##-------------------------- CREATE MAP LABELS ----------------------
#GOVERNORATE LABELS
# Get polygons centroids
centroids <- as.data.frame(centroid(Admin1))
colnames(centroids) <- c("lon", "lat")
centroids <- data.frame("ID" = 1:nrow(centroids), centroids)

# Create SpatialPointsDataFrame object
coordinates(centroids) <- c("lon", "lat") 
proj4string(centroids) <- sp::proj4string(Admin1) # assign projection
centroids@data <- sp::over(x = centroids, y = Admin1, returnList = FALSE)
centroids1 <- as.data.frame(centroid(Admin1))
colnames(centroids1) <- c("lon", "lat")
centroids@data<- cbind(centroids@data, centroids1)

#YEMEN LABEL
YEMl<- as.data.frame(cbind(48.5164,15.5527))
colnames(YEMl) <- c("lon", "lat")
YEMl <- data.frame("ID" = 1:nrow(YEMl), YEMl)
coordinates(YEMl) <- c("lon", "lat") 
proj4string(YEMl) <- proj4string(Admin1)
UKRl1<- as.data.frame(cbind(48.5164,15.5527))
YEMl@data<-cbind(YEMl@data, "YEMEN", UKRl1 )
colnames(YEMl@data) <- c("index","name","lon", "lat")

## For drop down selection in data explorer and plot tabs 

vars_functionnality <- colnames(indicators)[-(1:3)]
names(vars_functionnality)<-vars_functionnality


plot_location_list <- Admin2table %>%                                                 # Define location list
  ungroup %>%
  dplyr::rename(Governorate=government_name, District=district_name) %>%
  dplyr::select(Governorate, District) %>%
  arrange(Governorate, District) %>%
  filter(!duplicated(District))

#DROP DOWN MENU SELECTIONS 
## Drop-down for Plot & Data Explorer + map parameters
# To be updated whenever adding new item

vars <- c(
  "WASH SMEB"="WASH_SMEB", "Food SMEB"="Food_SMEB",
  "Parallel Exchange Rates"="exchange_rates", "Wheat Flour" = "wheat_flour",
  "Rice" = "rice", "Dry Beans" = "beans_dry",
  "Canned Beans" = "beans_can", "Lentils" = "lentil",
  "Vegetable Oil" = "vegetable_oil", "Sugar" = "sugar",
  "Salt" = "salt", "Potato" = "potato",
  "Onion" = "onion", "Petrol" = "petrol",
  "Diesel" = "diesel", "Bottled Water"="bottled_water",
  "Treated Water"="treated_water", "Soap"="soap",
  "Laundry Powder"="laundry_powder", "Sanitary Napkins"="sanitary_napkins",
  "Water Trucking"= "cost_cubic_meter"
  )
title.legend <- c("WASH SMEB Cost", "Food SMEB Cost", "YER to 1 USD", "Price (1 Kg)", "Price (1 Kg)", "Price (10 Pack)", "Price (15oz can)","Price (1 Kg)", 
  "Price (1 L)", "Price (1 Kg)", "Price (1 Kg)", "Price (1 Kg)", "Price (1 Kg)", "Price (1 L)", "Price (1 L)", "Price (0.75 L)", "Price (10 L)", 
  "Price (100 g)", "Price (100 g)", "Price (10 Pack)", "Price (Cubic m)",
  rep("	% of traders", 28))
unit <- c(rep(" YER", 21), rep(" %", 28))

## Setting palettes for all items:
pal1<-colorRamp(c("#ADFFA5", "#A7383D", "#420A0D"), interpolate="linear")
pal2<-colorRamp(c("#C3FFFD", "#6EFBF6", "#009F99", "#00504D"), interpolate="linear")
pal3<-colorRamp(c("#C9C3F8", "#5D52AD", "#FAD962", "#AA9239"), interpolate="linear")
pal4<-colorRamp(c("#FFD7D9", "#FF535B", "#FB000D", "#830007", "#480004"), interpolate="linear")
pal5<-colorRamp(c("#C7C0FF", "#7A6AFF", "#1501B9", "#0A005D", "#050033"), interpolate="linear")

palette <- c(pal1, "Greens", "Greens", pal2, "YlOrBr", pal2, "BuPu","RdPu", pal3, "Greens", pal3,
             "Greens", "Greens", "YlOrBr", pal4, pal5, pal2, "RdPu", "Purples", "BuPu", pal3, 
             rep("YlOrBr", 28))                                                 # keep same palette for the market functionality indicators

indicator_group <- c(rep("I. Indices", 2), 
                     "II. Currencies",
                     rep("III. Food items", 10),
                     rep("IV. Fuels", 2),
                     rep("V. Water", 2),
                     rep("VI. Non-food items", 3),
                     "V. Water",
                     rep("VI. Other indicators", 28))

# Indicator_list => will determine drop down list + legend + palettes for maps
indicator_list <- data.frame(Item=names(c(vars, vars_functionnality)),
                             Variable=unname(c(vars, vars_functionnality)),
                             Group = indicator_group,
                             Legend = title.legend,
                             Unit = unit,
                             Palette = I(palette))

dates <- sort(unique(Admin2table$date2))                                            # define list with date range in data
dates_min  <- as.Date("2020-01-01")                                               # set minimum date to be displayed
dates_max  <- max(Admin2table$date2, na.rm = T)                                     # maximum date in data
dates_max2 <- sort(unique(Admin2table$date2), decreasing=T)[2]                      # second-latest date

# dates_max_1y <- as.POSIXlt(dates_max)                                             # most recent month minus 1 year
# dates_max_1y$year <- dates_max_1y$year-1
# dates_max_1y <- as.Date(dates_max_1y)
# 
# dates_jram <- sort(unique(jram$Date))                                             # date range in JRAM data
# dates_min_jram <- min(dates_jram)                                                 # minimum date in JRAM data
# dates_max_jram <- max(dates_jram)                                                 # maximum date in JRAM data






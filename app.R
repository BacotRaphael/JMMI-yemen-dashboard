# JMMI Dashboard
# 08.06.21
# With food components

#GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL GLOBAL

#install packages
library(devtools)
library(usethis)
library(shiny)
library(shinyjs)
library(rgdal)
library(dplyr)
library(leaflet)
library(highcharter)
library(zoo)
library(ggplot2)
library(rgeos)
library(classInt)
library(geosphere)
library(shinythemes)
library(sf)
library(purrr)
library(shinydashboard)
library(readxl)
library(DT)
library(formattable)
library(tibble)
library(curl)
library(sp)
library(stringr)
library(shinyWidgets)
library(leaflet.extras)
library(kableExtra)
library(tidytidbits)
library(data.table)
library(openxlsx)
library(grDevices)
library(sass)
library(scales)
library(htmltools)
library(httr)  
set_config(use_proxy(url="10.3.100.207",port=8080))                             # to solve "error in curl::curl_fetch_memory

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

# Full database for data explorer at KII level 
full_data <- read.csv("data/data_all.csv") %>%
  dplyr::select(-matches("district_au|cash_feasibility|market_|_source|exchange_rate_market.|type_market|_other|infra|X"), -ends_with("mrk_supply_issues"), -ends_with("other"), -ends_with("not_answer")) %>%
  dplyr::rename(Date=date, Governorate=government_name, District=district_name) %>%
  dplyr::mutate(Date=as.Date(as.yearmon(Date))) 
  
# Full database for data explorer at district level + Plot tab
indicators <- read.csv("data/data_market_functionnality.csv") %>%
  dplyr::select(-matches("X|country|district_au|cash_feasibility|market_|_source|exchange_rate_|type_market|_other|infra"), -ends_with("mrk_supply_issues"), -ends_with("other"), -ends_with("not_answer")) %>%
  dplyr::select(jmmi_date, governorate_name, district_name, 7:ncol(.)) %>%
  tidyr::gather(Indicator, Value, 4:(ncol(.))) %>%
  dplyr::rename(date=jmmi_date, governorate=governorate_name, district=district_name) %>%
  dplyr::group_by(date, governorate, district, Indicator) %>%
  dplyr::summarise(freq = sum(Value == 1 | Value == "yes" | Value == "Yes", na.rm = TRUE) / sum(!is.na(Value)) * 100) %>%
  mutate_if(is.numeric, round, 0) %>% mutate(freq=ifelse(is.nan(freq), NA, freq)) %>%
  tidyr::spread(Indicator, freq) %>%
  dplyr::rename(Date = date, Governorate = governorate, District = district) %>% ungroup

indicators_long <- indicators %>%
  tidyr::pivot_longer(cols = 4:ncol(.)) %>%
  dplyr::rename(Item=name, Price=value)

# Summarizing market functionnality indicators at gov and national level
indicators_admin1 <- indicators %>% dplyr::select(-District) %>% group_by(Date, Governorate) %>%
  summarise_at(vars(-group_cols()), ~mean(., na.rm=T)) %>% mutate_at(vars(-group_cols()), ~ifelse(is.nan(.), NA, .))
indicators_admin0 <- indicators %>% dplyr::select(-District, -Governorate) %>% group_by(Date) %>%
  summarise_at(vars(-group_cols()), ~mean(., na.rm=T))%>% mutate_at(vars(-group_cols()), ~ifelse(is.nan(.), NA, .))

## Uploading national, governorate and district data
AdminNatData <- read.csv("data/national_interactive.csv") %>% as_tibble() %>% dplyr::select(-X) %>%
  left_join(indicators_admin0, by=c("date"="Date"))
Admin1data <- read.csv("data/governorate_interactive.csv") %>% as_tibble() %>% dplyr::select(-X) %>%
  left_join(indicators_admin1, by=c("date"="Date", "government_name"="Governorate"))                       
Admin2data <- read.csv("data/district_interactive.csv") %>% as_tibble()%>% dplyr::select(-X) %>%
  left_join(indicators, by = c("date"="Date", "government_name"="Governorate", "district_name"="District"))

## SMEB Calculation - update the SMEB in this function only!
calculate.smeb <- function(df){
  df <- df %>%
    mutate(WASH_SMEB = (soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15) %>% as.numeric %>% round(.,0),
           Food_SMEB = (wheat_flour*75+beans_dry*10+vegetable_oil*8+sugar*2.5+salt) %>% as.numeric %>% round(.,0),
           NFI_Shelter_lumpsum = ifelse(aor == "North", 25000, ifelse(aor == "South", 28750, mean(c(25000,28750)))) %>% round(.,0),
           Services_lumpsum = ifelse(aor == "North", 19000, ifelse(aor == "South", 21850, mean(c(19000,21850)))) %>% round(.,0),
           SMEB = ifelse(!is.na(WASH_SMEB) & !is.na(Food_SMEB), WASH_SMEB + Food_SMEB + NFI_Shelter_lumpsum + Services_lumpsum %>% round(.,0), NA))
  return(df)
}

## Calculate SMEB at district level
Admin2data <- Admin2data %>% calculate.smeb

## Aggregate SMEB and join to admin1 and national level
smeb_gov <- Admin2data %>% group_by(date, government_ID) %>% summarise_at(vars(matches("SMEB|lumpsum")), ~median(., na.rm=T))
Admin1data <- Admin1data %>% full_join(., smeb_gov, by = c("date", "government_ID")) 
smeb_nat <- Admin2data %>% group_by(date) %>% summarise_at(vars(matches("SMEB|lumpsum")), ~median(., na.rm=T))
AdminNatData <- AdminNatData %>% full_join(., smeb_nat, by = c("date"))
max_date <- max(as.Date(as.yearmon(AdminNatData$date)))  

#Wrangle Data into appropriate formats
#Governorates
Admin1table <- Admin1data %>% mutate_at(vars(-matches("date|aor|government")), ~ round(as.numeric(.)), 0) %>%
  mutate(date2 = as.Date(as.yearmon(date)))
Admin1data_current <- Admin1table %>% arrange(desc(date2)) %>% dplyr::filter(date2 == max_date) # subset only recent month dates to attach to shapefile
currentD <- as.character(format(max(Admin1table$date2),"%B %Y"))                                # define current date for disply in dashboard

#Districts
Admin2table <- Admin2data %>% mutate_at(vars(-matches("date|aor|government|district")), ~ round(as.numeric(.)), 0) %>%
  mutate(date2 = as.Date(as.yearmon(date)))
Admin2data_current <- Admin2table %>% arrange(desc(date2)) %>% dplyr::filter(date2 == max_date) # subset only recent month dates to attach to shapefile
currentD <- as.character(format(max(Admin2table$date2),"%B %Y"))                                # define current date for disply in dashboard

#National
AdminNatTable <- AdminNatData %>% mutate_at(vars(-matches("date|aor")), ~ round(as.numeric(.)), 0) %>%
  mutate(date2 = as.Date(as.yearmon(date)))
AdminNatData_current <- AdminNatTable %>% arrange(desc(date2)) %>% dplyr::filter(date2 == max_date) # subset only recent month dates to attach to shapefile
currentD <- as.character(format(max(AdminNatTable$date2),"%B %Y"))                                  # define current date for display in dashboard

# Price long data for Plot tab
prices <- Admin2table %>%
  dplyr::select(date2, government_name:district_ID, everything(), -date, -government_ID, -district_ID, -aor) %>%
  dplyr::rename(Date=date2, Governorate=government_name, District=district_name) 
prices_long <- prices %>%
  tidyr::pivot_longer(cols = 4:ncol(.)) %>%
  dplyr::rename(Item=name, Price=value)

prices_long <- prices_long %>% rbind(indicators_long)                           ## For the plot tab

data <- Admin2table %>%                                                         ## for the data explorer tab
  dplyr::rename(Date=date, Governorate=government_name, District=district_name) 

##-------------------------- SPATIAL DATA WRANGLE ----------------------

# Read in shapefiles
Admin1<- readOGR("./www", "YEM_adm1_Governorates")
Admin2<- readOGR("./www", "YEM_adm2_Districts")

Admin1@data$admin1name<-gsub("Amanat Al Asimah", "Sana'a City", Admin1@data$admin1name)
Admin1@data$admin1refn<-gsub("Amanat Al Asimah", "Sana'a City", Admin1@data$admin1refn)
Admin2@data$admin1name<-gsub("Amanat Al Asimah", "Sana'a City", Admin2@data$admin1name)
Admin2@data<- Admin2@data %>% dplyr::mutate_if(is.factor, as.character)

##-------------------------- COMBINE TABULAR & SPATIAL DATA----------------------
#Merge data from Google Sheet with Rayon shp file
# Rshp <- merge(x=Admin2,y=Admin2data_current, by.x="admin2pcod", by.y= "district_ID")
Rshp <- sp::merge(x=Admin2,y=Admin2table, by.x="admin2pcod", by.y= "district_ID", duplicateGeoms = TRUE,no.dups = FALSE )
Rshp <- Rshp[!is.na(Rshp$date2),]                                               # filter out NAs date
DistsNumb <- sum(!is.na(Rshp@data$district_name))                               # get number of districts covered...

Rshp <- st_simplify(st_as_sf(Rshp), dTolerance = 0.5)
Rshp <- st_transform(x = Rshp, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Rshp <- as(Rshp,"Spatial")

Admin1 <- st_simplify(st_as_sf(Admin1), dTolerance = 0.5)
Admin1 <- st_transform(x = Admin1, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Admin1 <- as(Admin1,"Spatial")

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
centroids@data <- cbind(centroids@data, centroids1)

# #YEMEN LABEL
YEMl <- as.data.frame(cbind(48.5164,15.5527))
colnames(YEMl) <- c("lon", "lat")
YEMl <- data.frame("ID" = 1:nrow(YEMl), YEMl)
coordinates(YEMl) <- c("lon", "lat")
proj4string(YEMl) <- proj4string(Admin1)
UKRl1<- as.data.frame(cbind(48.5164,15.5527))
YEMl@data<-cbind(YEMl@data, "YEMEN", UKRl1 )
colnames(YEMl@data) <- c("index","name","lon", "lat")

## For drop down selection in data explorer and plot tabs 

# WASH_SMEB = (soap*10.5+laundry_powder*20+sanitary_napkins*2+as.numeric(cost_cubic_meter)*3.15) %>% as.numeric %>% round(.,0),
# Food_SMEB = (wheat_flour*75+beans_dry*10+vegetable_oil*8+sugar*2.5+salt) %>% as.numeric %>% round(.,0),
# NFI_Shelter_lumpsum = ifelse(aor == "North", 25000, ifelse(aor == "South", 28750, mean(c(25000,28750)))) %>% round(.,0),
# Services_lumpsum = ifelse(aor == "North", 19000, ifelse(aor == "South", 21850, mean(c(19000,21850)))) %>% round(.,0),
# SMEB = ifelse(!is.na(WASH_SMEB) & !is.na(Food_SMEB), WASH_SMEB + Food_SMEB + NFI_Shelter_lumpsum + Services_lumpsum %>% round(.,0), NA))

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")

#DROP DOWN MENU SELECTIONS 
## Drop-down for Plot & Data Explorer + map parameters
# To be updated whenever adding new item

vars <- c(
  "SMEB"="SMEB", "SMEB Water"="WASH_SMEB", "SMEB Food"="Food_SMEB",
  "Parallel Exchange Rates"="exchange_rates",
  "Wheat Flour" = "wheat_flour", "Rice" = "rice", "Dry Beans" = "beans_dry", "Canned Beans" = "beans_can", "Lentils" = "lentil",
  "Vegetable Oil" = "vegetable_oil", "Sugar" = "sugar", "Salt" = "salt", "Potato" = "potato", "Onion" = "onion", 
  "Petrol" = "petrol", "Diesel" = "diesel",
  "Bottled Water"="bottled_water", "Treated Water"="treated_water",
  "Soap"="soap", "Laundry Powder"="laundry_powder", "Sanitary Napkins"="sanitary_napkins", "Bleach"="bleach", "Cooking gas"="cooking_gas",
  "Water Trucking"= "cost_cubic_meter"
)

vars_functionnality <- colnames(indicators)[-(1:3)]
names(vars_functionnality) <- 
  c("If the demand for food items were to increase by 100%, would you be able to respond to this increase?",
    "If the demand for food items were to increase by 50%, would you be able to respond to this increase?",
    "If the demand for fuel items were to increase by 100%, would you be able to respond to this increase?",
    "If the demand for fuel items were to increase by 50%, would you be able to respond to this increase?",
    "If the demand for WASH items were to increase by 100%, would you be able to respond to this increase?",
    "If the demand for WASH items were to increase by 50%, would you be able to respond to this increase?",
    "If the demand for water trucking were to increase by 100%, would you be able to respond to this increase?",
    "If the demand for water trucking were to increase by 50%, would you be able to respond to this increase?",
    "Supply issues: Destruction/damage to storage capacity",
    "Supply issues: Movement restrictions (check points, curfews, roadblocks, etc)",
    "Have supply routes changed in a way harmful to your business in the past 30 days?",
    paste0("% of vendors ", gsub("sell", "selling", gsub("_", " ", vars_functionnality[grepl("sell", vars_functionnality)])))
     )
n_mkt_fun <- length(vars_functionnality)

title.legend <- c("SMEB Cost", "WASH SMEB Cost", "Food SMEB Cost",
                  "YER to 1 USD",
                  "Price (1 Kg)", "Price (1 Kg)", "Price (10 Pack)", "Price (15oz can)","Price (1 Kg)", 
                  "Price (1 L)", "Price (1 Kg)", "Price (1 Kg)", "Price (1 Kg)", "Price (1 Kg)", 
                  "Price (1 L)", "Price (1 L)",
                  "Price (0.75 L)", "Price (10 L)", 
                  "Price (100 g)", "Price (100 g)", "Price (10 Pack)", "Price (Cubic m)", "Price (1 L)", "Price (18.8kg)",
                  rep("	% of traders", n_mkt_fun))
unit <- c(rep(" YER", 24),
          rep(" %", n_mkt_fun))

indicator_group <- c(rep("I. Indices", 3), 
                     "II. Currencies",
                     rep("III. Food items", 10),
                     rep("IV. Fuels", 2),
                     rep("V. Water", 2),
                     rep("VI. Non-food items", 5),
                     "V. Water",
                     rep("VI. Other indicators", n_mkt_fun))

## Setting custom maps color palettes for all items:
pal_red <<- colorRamp(c("#FEF2F2", "#F7B7B7", "#EE5859", "#8F3535", "#471A1A"), interpolate="linear")
pal_blue <<- colorRamp(c("#EEF3F8", "#B6CBDF", "#0067A9", "#004876", "#002844"), interpolate="linear")
pal_green <<- colorRamp(c("#E7ECE6", "#C1CFBF", "#72966E", "#50694D", "#2D3B2C"), interpolate="linear")
pal_yellow <<- colorRamp(c("#FFFDDD", "#FFF9A9", "#FFF67A", "#B2AC55", "#666231"), interpolate="linear")

palette <- c(rep("pal_blue", length(indicator_group[indicator_group %in% c("I. Indices", "II. Currencies", "V. Water")])),
             rep("pal_green", length(indicator_group[indicator_group %in% c("III. Food items")])),
             rep("pal_red", length(indicator_group[indicator_group %in% c("IV. Fuels", "VI. Non-food items")])),
             rep("pal_yellow", length(indicator_group[indicator_group %in% c("VI. Other indicators")]))) %>% lapply(get)      

# Indicator_list => will determine drop down list + legend + palettes for maps
Item <- names(c(vars, vars_functionnality))
Item2 <- stringr::str_wrap(Item, width = 45)  # To wrap the choices that are too large in the drop down menu
Item2 <- stringr::str_replace_all(Item2, "\\n", "<br>")

indicator_list <- data.frame(Item=Item,
                             Item2=Item2,
                             Variable=unname(c(vars, vars_functionnality)),
                             Group = indicator_group,
                             Legend = title.legend,
                             Unit = unit,
                             Palette = I(palette))
# write.xlsx(indicator_list %>% dplyr::select(-Palette), "indicator_list.xlsx")

plot_location_list <- Admin2table %>% ungroup %>%                               # Define location list
  dplyr::rename(Governorate=government_name, District=district_name) %>%
  dplyr::select(Governorate, District) %>%
  arrange(Governorate, District) %>%
  dplyr::filter(!duplicated(District))

dates <- sort(unique(Admin2table$date2))                                        # define list with date range in data
dates_min  <- as.Date("2020-01-01")                                             # set minimum date to be displayed
dates_max  <- max(Admin2table$date2, na.rm = T)                                 # maximum date in data
dates_max2 <- sort(unique(Admin2table$date2), decreasing=T)[2]                  # second-latest date

dates_max_1y <- as.POSIXlt(dates_max)                                             # most recent month minus 1 year
dates_max_1y$year <- dates_max_1y$year-1
dates_max_1y <- as.Date(dates_max_1y)

# Prepare tables for dashboard tab [under construction]
prices_country <- prices %>%                                                      # aggregate price data at country level
  dplyr::select(-Governorate, -District) %>%
  dplyr::select(Date:num_obs, matches("SMEB"), -num_obs) %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise_all(median, na.rm = TRUE)
  
prices_country_long <- tidyr::gather(prices_country, Item, Price, 2:ncol(prices_country))# transform country-level price data to long format

prices_country_home <- prices_country %>%                                         # filter out SMEB data from country level price data
  dplyr::filter(Date >= dates_min) %>%
  dplyr::select(Date, SMEB, Food_SMEB, WASH_SMEB) %>%
  tidyr::gather(Item, Price, SMEB:WASH_SMEB)                                        # transform SMEB data to long format so highcharter can read dataframe

prices_changes <- prices_country_long %>%                                         # calculate bi-monthly/yearly changes of item prices
  dplyr::filter(Date == dates_max | Date == dates_max2 | Date == dates_max_1y) %>%
  dplyr::group_by(Item) %>%
  dplyr::mutate(change  = scales::percent(Price/lag(Price, order_by=Date)-1, accuracy = 1),
         change2 = scales::percent(Price/lag(Price, n = 2, order_by=Date)-1, accuracy = 1)) %>%
  dplyr::mutate(change  = ifelse(!grepl('^\\-', change) & change != "0%" & !is.na(change), paste0("+", change, HTML(" &#9650;")), change),
         change  = ifelse(grepl('^\\-', change), paste0(change, HTML(" &#9660;")), change),
         change  = ifelse(change == "0%", paste0(change, HTML(" &#9654;")), change),
         change2 = ifelse(!grepl('^\\-', change2) & change2 != "0%" & !is.na(change2), paste0("+", change2, HTML(" &#9650;")), change2),
         change2 = ifelse(grepl('^\\-', change2), paste0(change2, HTML(" &#9660;")), change2),
         change2 = ifelse(change2 == "0%", paste0(change2, HTML(" &#9654;")), change2)) %>%
  dplyr::filter(Date == dates_max, !is.na(Price)) %>%
  dplyr::select(-Date) %>%
  dplyr::mutate(Price   = format(Price, big.mark=","),
         change2 = tidyr::replace_na(change2, "NA")) %>%
  dplyr::rename("Price (in YER)"    = Price,
         "Bi-monthly change" = change,
         "Yearly change"     = change2) %>%
  left_join(indicator_list %>% dplyr::select(Item, Variable, Group), by=c("Item"="Variable")) %>% ungroup %>%
  dplyr::select(-Item) %>% dplyr::rename(Item=Item.y) %>% dplyr::relocate(Item, .before=1)

prices_changes_items <- prices_changes %>%
  dplyr::filter(!(str_detect(Item, "^SMEB") | Item == "Parallel Exchange Rates"))

prices_changes_meb <- prices_changes %>% 
  dplyr::filter(str_detect(Item, "SMEB") | str_detect(Item, "Parallel Exchange Rates")) %>%
  dplyr::arrange(Group, Item) %>% dplyr::mutate(Item=gsub("Parallel Exchange Rates","US dollar (1 USD)", Item))

data_latest <- full_data %>%                                                                                   # latest dataset for download on dashboard page
  dplyr::filter(Date == dates_max) %>%
  dplyr::select(-aor)

month_collected      <- paste0(format(dates_max, "%B"), " ",format(dates_max, "%Y"))                      # define overview of last round
shops_covered        <- nrow(data_latest)
districts_covered    <- n_distinct(data_latest$District, na.rm = FALSE)
governorates_covered <- n_distinct(data_latest$Governorate, na.rm = FALSE)
overview_round       <- data.frame(figure = c("Month", "Traders interviewed", "Districts covered", "Governorates covered"),
                                   value  = c(month_collected, shops_covered, districts_covered, governorates_covered))

table_round <- overview_round %>%                                                                         # style overview table
  kbl(escape = F, format.args = list(big.mark = ","), align = "lr", col.names = NULL) %>%
  column_spec(1, width = "12em") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = T) %>%
  row_spec(1,   extra_css = "font-size: 11.5px; border-top: 2px solid gainsboro") %>%
  row_spec(2:nrow(overview_round), extra_css = "font-size: 11.5px;")

table_changes <- prices_changes_items %>%                                                                # style item table
  arrange(Group) %>% dplyr::select(-Group) %>%
  kbl(escape = F, format.args = list(big.mark = ","), align = "lrrr") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = F) %>%
  column_spec(1, width = "12em") %>%
  column_spec(3, color = ifelse(grepl('^\\+', prices_changes_items$'Bi-monthly change'), "red", ifelse(grepl('^\\-', prices_changes_items$'Bi-monthly change'), "green", "auto"))) %>%
  column_spec(4, color = ifelse(grepl('^\\+', prices_changes_items$'Yearly change'), "red", ifelse(grepl('^\\-', prices_changes_items$'Yearly change'), "green", "auto"))) %>%
  pack_rows("Food Items", 1, 10, label_row_css = "background-color: #f5f5f5; font-size: 10.5px; border-top: 2px solid gainsboro") %>%
  pack_rows("Fuels & Water", 11, 15, label_row_css = "background-color: #f5f5f5; font-size: 10.5px; border-top: 2px solid gainsboro") %>%
  # pack_rows("Water", 13, 15,  label_row_css = "background-color: #f5f5f5; font-size: 10.5px; border-top: 2px solid gainsboro") %>%
  pack_rows("Non-Food Items", 16, 20, label_row_css = "background-color: #f5f5f5; font-size: 10.5px; border-top: 2px solid gainsboro") %>%
  row_spec(0:nrow(prices_changes_items), extra_css = "font-size: 11px;") %>%
  scroll_box(height="90%")

# Dashboard tables

smeb <- data.frame(Category = c(rep("Water", 4), rep("Food Items", 5), "Non-food items & shelter", "Services"), # define SMEB content table
                   Item = c("Soap", "Laundry powder", "Sanitary Napkins", "Cubic meter water",
                            "Wheat flour", "Beans dry","Vegetable oil", "Sugar", "Salt", "NFI & shelter lumpsum", "Services lumpsum"),
                   Quantity = c("10.5 ", "20 Kg", "2 Boxes", "3.15 m^3", "7.5 Kg", "10 ", "8 ", "2.5 ", "1Kg",
                                "North: 25'000 YER\nSouth: 28'750 YER", "North: 19'000 YER \nSouth: 21'850 YER"))

smeb_kbl <- smeb %>%                                                                                      # make a html (kable) object out of dataframe
  kbl(escape = F) %>%
  kable_styling(bootstrap_options = c("hover", "condensed", "striped"), fixed_thead = T, full_width = F) %>%
  column_spec(1, width = "8em", bold = T, background = "white") %>%
  column_spec(2, width = "10em") %>%
  column_spec(3, width = "12em") %>%
  collapse_rows(columns = 1, valign = "top") %>%
  row_spec(0:nrow(smeb), extra_css = "font-size: 11px;")

table_changes_meb <- prices_changes_meb %>% dplyr::select(-Group) %>%           # style key figures table
  kbl(escape = F, format.args = list(big.mark = ","), align = "lrrr") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = F) %>%
  column_spec(1, width = "9em") %>%
  column_spec(3, color = ifelse(grepl('^\\+', prices_changes_meb$'Bi-monthly change'), "red", ifelse(grepl('^\\-', prices_changes_meb$'Bi-monthly change'), "green", "auto"))) %>%
  column_spec(4, color = ifelse(grepl('^\\+', prices_changes_meb$'Yearly change'), "red", ifelse(grepl('^\\-', prices_changes_meb$'Yearly change'), "green", "auto"))) %>%
  row_spec(0:nrow(prices_changes_meb), extra_css = "font-size: 11px;")
  
varsDate <- c("Months to Select" = "varDateSelect")

# UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI

ui <- tagList(
  
  shiny::includeCSS("navbar.css"), # custom CSS to kill the white space on top of the navbar
  navbarPage("REACH: Yemen Joint Market Monitoring Initiative (JMMI)",
             
             collapsible = T, windowTitle = "REACH: Yemen Joint Market Monitoring Initiative (JMMI)", # Title for browser tab window
             
             #### * 6.1 Home ######################################################################
             
             tabPanel("Dashboard",                                                                      # define panel title
                      icon = icon("tachometer-alt"),                                                    # select icon to be displayed in front of title
                      
                      tags$head(shiny::includeCSS("styles.css"),                                        # Include our custom CSS
                                style =" {overflow-y: scroll; }"),
                      
                      div(class="dashboard",                                                            # set dashboard class from CSS file
                          
                          tags$head(shiny::includeCSS("styles_IRQ.css")),                               # load CSS stylesheet
                          
                          leafletOutput("map_home", width = "100%", height = "100%"),                   # display background map
                          
                          absolutePanel(                                                                # define introduction box
                            id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                            top = as.character(ver.anchor<-45), left = "15", right = "auto", bottom = "auto", width = "400", height = 350,
                            h4("Introduction"),
                            p("The  Yemen  Joint  Market  Monitoring  Initiative  (JMMI) is an initative led by REACH in collaboration with the Water, Sanitation,
                              and Hygiene (WASH) Cluster  and the Cash and Market Working Group (CMWG) to support humanitarian cash actors with the harmonization of price
                              monitoring throughout Yemen. The basket of goods assessed includes eight non-food items (NFIs), including fuel, water and hygiene products,
                              reflecting the programmatic areas of the WASH Cluster. The JMMI tracks all components of the WASH Survival Minimum Expenditure Basket
                              (SMEB) since September 2018.",
                              style="text-align:justify"),
                            p(tags$i(h6("Display price data over time with the Price Plot, do spatial analysis with the Map, or
                                         discover the data with the Data Explorer.",
                                        style="color:grey;text-align:justify"))),
                            br()
                          ),
                          
                          absolutePanel(                                                                    # define chart box
                            id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                            top = as.character(ver.anchor+365), left = "15", right = "auto", bottom = "auto", width = "400", height = "270",
                            hchart(prices_country_home, "line",                                           # define chart
                                   hcaes(x = Date, y = Price, group = Item)) %>%
                              hc_yAxis(min = 0, title = list(text = "")) %>%
                              hc_xAxis(title = "", labels = list(align = "center")) %>%
                              hc_size(height = "253") %>%
                              hc_title(
                                text = "Overall Median SMEB Over Time (in YER)",
                                margin = 10,
                                align = "left",
                                style = list(fontSize = 15)
                              ) %>%
                              hc_colors(cols) %>%
                              hc_legend(style = list(fontSize = 8))
                          ),
                          
                          absolutePanel(
                            id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                            top = as.character(ver.anchor), left = "430", right = "auto", bottom = "auto", width = "340", height = "250",
                            h4(paste0("Key Figures", " (", format(dates_max, "%b"), " ", format(dates_max, "%Y"), ")")),
                            HTML(table_changes_meb), br()
                          ),
                          
                          absolutePanel(
                            id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                            top = as.character(ver.anchor+265), left = "430", right = "auto", bottom = "auto", width = "340", height = "185",
                            h4("Latest Round"),
                            HTML(table_round), br()
                          ),
                          
                          absolutePanel(
                            id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                            top = as.character(ver.anchor+465), left = "430", right = "auto", bottom = "auto", width = "340", height = "170",
                            h4("Data Download"),
                            # p("Visit the Data Explorer or download the full dataset from the latest round here:"),
                            downloadButton("downloadDataLatest", style = "font-size: 12px",
                                           paste0("Download ", format(dates_max, "%B"), " ", format(dates_max, "%Y"), " dataset           ")),
                            br(),
                            br(),
                            downloadButton("downloadFactsheet", style = "font-size: 12px",
                                           paste0("Download ", format(dates_max, "%B"), " ", format(dates_max, "%Y"), " situation overview")),
                            br()
                          ),
                          
                          absolutePanel(
                            id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                            top = as.character(ver.anchor), left = "785", right = "auto", bottom = "auto",
                            width = "400", height = "635",
                            h4(paste0("Overall Median Item Prices", " (", format(dates_max, "%b"), " ", format(dates_max, "%Y"), ")")),
                            HTML(table_changes), br()
                          ),
                          
                          absolutePanel(id = "dropdown", top = as.character(ver.anchor+30), left = 675, width = 200, fixed=FALSE, draggable = FALSE, height = "auto",
                                        dropdown(
                                          h4("SMEB contents"),
                                          column(
                                            HTML(smeb_kbl),
                                            width = 6),
                                          column(p(h6("Each month, enumerators conduct KI interviews with market vendors to collect three price quotations for each item from the same market in each district.
                                                       REACH calculates the WASH SMEB, which is composed of four median item prices: Soap (1.05 kg), Laundry Powder (2 kg), Sanitary Napkins (20 units), and Water Trucking (3.15 m3).")),
                                                 p(h6("The calculation of the aggregated median price for districts and governorates is done following a stepped approach. Firstly, the median of all the price quotations related to the same market is taken. 
                                                      Secondly, the median quotation from each market is aggregated to calculate the district median. Finally, the median quotation from each district is aggregated to calculate the governorate median.")),
                                                 p(h6("More details on the SMEB can be found here:",
                                                      tags$a(href="https://www.humanitarianresponse.info/sites/www.humanitarianresponse.info/files/documents/files/cmwg_yemen_smeb_gn_final_27102020.pdf",
                                                             "SMEB Guidance Note"), ".")),
                                                 width = 5),
                                          width = "650px",
                                          tooltip = tooltipOptions(title = "Click for more details on the SMEB."),
                                          size = "xs",
                                          up = FALSE,
                                          style = "jelly", icon = icon("info"),
                                          animate = animateOptions(
                                            enter = "fadeInDown",
                                            exit  = "fadeOutUp",
                                            duration = 0.5)
                                        )
                          ),
                          # display CWG & REACH logos on bottom left
                          absolutePanel(id = "logo", class = "card", 
                                        top = as.character(v.anch<-750),
                                        # bottom = 20,
                                        left = 20,
                                        # left = 1200,
                                        fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.humanitarianresponse.info/sites/www.humanitarianresponse.info/files/documents/files/cmwg_yemen_smeb_gn_final_27102020.pdf', target = "_blank",
                                               tags$img(src='CMWG Logo.jpg', height='40'))),
                          
                          absolutePanel(id = "logo", class = "card",
                                        top = as.character(v.anch),
                                        bottom = 20,
                                        left = 140,
                                        # left = 1320,
                                        fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.reach-initiative.org', target = "_blank", tags$img(src='reach_logoInforming.jpg', height='40'))),
                          
                          # display partner logos on bottom right
                          absolutePanel(id = "logo", class = "card", top = ver<-v.anch+70, left = (anchor<-20), fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.acted.org/en/countries/yemen/', target = "_blank", tags$img(src='0_acted.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver, left = anchor+85, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_almaroof.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver, left = anchor+150, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://adra.org/', target = "_blank", tags$img(src='0_adra.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver, left = anchor+205, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_thadamon.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver, left = anchor+280, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_b4d.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+65, left = anchor, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='http://bchr-ye.org/', target = "_blank", tags$img(src='0_bchr.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+65, left = anchor+60, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.facebook.com/cyf.org77/', target = "_blank", tags$img(src='0_cyf.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+65, left = anchor+100, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='http://www.drc.dk', target = "_blank", tags$img(src='0_drc.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+65, left = anchor+175, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.facebook.com/noqat.org/', target = "_blank", tags$img(src='0_gwq.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+65, left = anchor+210, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.iom.int/countries/yemen', target = "_blank", tags$img(src='0_iom.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+65, left = anchor+275, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.rescue.org/', target = "_blank", tags$img(src='0_IRC.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+65, left = anchor+310, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.mercycorps.org/where-we-work/yemen', target = "_blank", tags$img(src='0_mercy.jfif', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+65, left = anchor+340, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='http://nfdhr.org/', target = "_blank", tags$img(src='0_nfdhr.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+130, left = anchor, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_nfhd.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+130, left = anchor+80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.nrc.no/countries/middle-east/yemen/', target = "_blank", tags$img(src='0_nrc.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+130, left = anchor+180, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_ocfd.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+130, left = anchor+230, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.oxfam.org/en/tags/yemen', target = "_blank", tags$img(src='0_oxfam.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+130, left = anchor+325, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://rocye.org/', target = "_blank", tags$img(src= '0_roc.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+195, left = anchor, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://yemen.savethechildren.net/', target = "_blank", tags$img(src='0_sci.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+195, left = anchor+120, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_steps.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+195, left = anchor+160, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.facebook.com/TamdeenYouth/', target = "_blank", tags$img(src='0_soul.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+195, left = anchor+295, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='http://yfca.org/en/', target = "_blank", tags$img(src='0_yfca.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+195, left = anchor+360, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_ysd.jpg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+260, left = anchor, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.solidarites.org/en/missions/yemen/', target = "_blank", tags$img(src='0_si.jpeg', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+260, left = anchor+50, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_tyf.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+260, left = anchor+160, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='', target = "_blank", tags$img(src='0_vision.png', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+260, left = anchor+235, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.zoa-international.com/files/yemen/', target = "_blank", tags$img(src='0_zoa.PNG', height='30'))),
                          
                          absolutePanel(id = "logo", class = "card", top = ver+260, left = anchor+310, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.facebook.com/sama.alyemen.5', target = "_blank", tags$img(src='0_sama.jpg', height='30')))
                          
                          
                          
                      )                                                                                     # close dashboard class 
             ),
             
             ###..................................M A P. . P A G E ..........................................
             tabPanel("Map", #TAB LABEL
                      icon= icon("map"), #TAB ICON
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            shiny::includeCSS("AdminLTE.css"),
                            shiny::includeCSS( "bootstrap.css"), #added
                            shiny::includeCSS(path = "shinydashboard.css"),
                            br()#added
                          ),
                          
                          #LEAFLET MAP
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leaflet::leafletOutput("map1", width="100%", height="100%"), # BRING IN LEAFLET MAP, object created in server.R
                          tags$head(tags$style(".leaflet-control-zoom { display: none; }

                                            #controls {height:90vh; overflow-y: auto; }
                                              ")),                              # remove map zoom controls
                          
                          tags$head(tags$style(
                            type = "text/css",
                            "#controlPanel {background-color: rgba(255,255,255,0.8);}",
                            ".leaflet-top.leaflet-left .leaflet-control {
                           margin-top: 25px;
                         }"
                          )),
                          # https://stackoverflow.com/questions/37861234/adjust-the-height-of-infobox-in-shiny-dashboard
                          
                          #SIDE PANEL
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = 1,
                                        # width = 500,
                                        width = "35%",
                                        height = "auto",
                                        
                                        hr(),
                                        # h5("The Yemen Joint Market Monitoring Initiative (JMMI) is a harmonized price monitoriong initiative that focuses on informing
                                        # the Water, Sanitation, and Hygiene (WASH) Cluster and the Cash
                                        # and Market Working Group (CMWG) to support humanitarian activies throughout Yemen.
                                        # The JMMI provides an indicative estimation of the prices of WASH and fuel items across districts in Yemen."),
                                        
                                        h5(tags$u("Most recent findings displayed in map are from data collected in ", #DistsNumn and currentD will change based on the most recent JMMI, defined in global.R
                                                  tags$strong(DistsNumb), "districts in ", tags$strong(paste0(currentD,"."))),
                                           ("The districts outlined in red indicate that data for the selected item was collected in that district in previous months.")),
                                        
                                        h5("Further details regarding the JMMI methodology and the Survival Minimum Expenditure Basket (SMEB) calculation can be found on the information tab.
                                         For additional information on supply chains and market-related concerns, please visit the  ",a("REACH Resource Center", target="_blank",    href="https://www.reachresourcecentre.info/country/yemen/cycle/754/#cycle-754"), " to access the monthly situation overviews."),
                                        
                                        hr(),
                                        
                                        #h5(tags$u("Most recent findings displayed in map are from data collected in ", #DistsNumn and currentD will change based on the most recent JMMI, defined in global.R
                                        #   tags$strong(DistsNumb), "districts in ", tags$strong(currentD))),
                                        
                                        # selectInput("variable1", h4("Select Variable Below"), vars, selected = "Food_SMEB"), #linked text
                                        
                                        pickerInput("variable1",
                                                    label = "Select a variable below",
                                                    # choices = lapply(split(indicator_list$Variable, indicator_list$Group), as.list)[1:6],
                                                    # choices = lapply(split(indicator_list$Variable, indicator_list$Group), as.list),
                                                    choices = lapply(split(indicator_list$Item, indicator_list$Group), as.list),
                                                    options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                    selected = "SMEB",
                                                    multiple = FALSE,
                                                    choicesOpt = list(content = indicator_list$Item2)
                                        ),
                                        
                                        sliderTextInput("date_map",                                        # set date slider
                                                        "Month:",
                                                        force_edges = TRUE,
                                                        choices = dates,
                                                        selected = dates_max,
                                                        animate = TRUE
                                        ),
                                        
                                        h5(textOutput("text3")), #extra small text which had to be customized as an html output in server.r (same with text1 and text 2)
                                        
                                        #HIGH CHART
                                        highchartOutput("hcontainer", height= 300,
                                                        width = "100%"
                                                        #, width = 450
                                        ),
                                        
                                        #new data table
                                        hr(),
                                        selectInput(inputId= "varDateSelect", label = h4("Select Month of Data Collection"), choices=NULL, selected = (("varDateSelect"))), # linked date stuff
                                        h5("Please select a district to enable month selection"),
                                        h5(textOutput("text_DT")),
                                        DT::dataTableOutput("out_table_obs",height = "auto", width = "100%"),
                                        
                                        #####Attempt to add an info box
                                        hr(),
                                        h5("Exchange Rate for selected month"),
                                        h5("Please select month to populate the information box"),
                                        fluidRow(valueBoxOutput("info_exchange", width = 12)),
                                        hr(),
                                        #hr(),
                                        
                                        h6(htmlOutput("text1")),
                                        h6(htmlOutput("text2")),
                                        h6(htmlOutput("text4")),
                                        column(width=12, align="center", div(id="cite2", "Funded by: "), img(src='DFID UKAID.png', width= "90px"),img(src='OCHA@3x.png', width= "90px"),
                                               img(src='USAID.png', width= "105px")) #donor logos
                                        
                                        
                          ),
                          
                          
                          tags$div(id="cite",
                                   a(img(src='reach_logoInforming.jpg', height= "40px"), target="_blank", href="http://www.reach-initiative.org"),
                                   img(src='CMWG Logo.jpg', height= "40px", style='padding:1px;border:thin solid black;'),
                                   img(src='washlogo_grey-300DPI.png', height= "40px"))
                          
                          # tags$div(id="cite",
                          #          a(img(src='reach_logoInforming.jpg', width= "200px"), target="_blank", href="http://www.reach-initiative.org"))
                      )
             ),
             
             #### Plot ######################################################################
             
             tabPanel("Plot",                                                                         # set panel title
                      icon = icon("chart-line"),                                                            # select icon
                      chooseSliderSkin(skin = "Flat", color = NULL),                                        # set theme for sliders
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          tags$i(h6("Note: Reported prices are indicative only.", style="color:#045a8d")),
                          
                          pickerInput("plot_aggregation",
                                      label = "Aggregation level:",
                                      choices = c("District", "Governorate", "Country"),
                                      selected = "Country",
                                      multiple = FALSE
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Country'",
                                           radioGroupButtons("plot_type",
                                                             label = "Plot type:",
                                                             choices = c("Line Graph", "Boxplot"),
                                                             selected = "Line Graph",
                                                             justified = TRUE
                                           )
                          ),
                          
                          hr(),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'District'",
                                           radioGroupButtons("plot_by_district_item",
                                                             label = "Group by:",
                                                             choices = c("Item", "District"),
                                                             selected = "Item",
                                                             justified = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Governorate'",
                                           radioGroupButtons("plot_by_governorate_item",
                                                             label = "Group by:",
                                                             choices = c("Item", "Governorate"),
                                                             selected = "Item",
                                                             justified = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'District' & input.plot_by_district_item == 'District'",
                                           pickerInput("select_bydistrict_district",
                                                       label = "District(s):",
                                                       choices = lapply(split(plot_location_list$District, plot_location_list$Governorate), as.list),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = c("Al Mansura"),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'District' & input.plot_by_district_item == 'District'",
                                           pickerInput("select_bydistrict_item",
                                                       label = "Item:",
                                                       choices = lapply(split(indicator_list$Variable, indicator_list$Group), as.list)[1:6],
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = "Food_SMEB",
                                                       multiple = FALSE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Governorate' & input.plot_by_governorate_item == 'Governorate'",
                                           pickerInput("select_bygovernorate_governorate",
                                                       label = "Governorate(s):",
                                                       choices = unique(plot_location_list$Governorate),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = c("Aden"),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Governorate' & input.plot_by_governorate_item == 'Governorate'",
                                           pickerInput("select_bygovernorate_item",
                                                       label = "Item:",
                                                       choices = lapply(split(indicator_list$Variable, indicator_list$Group), as.list)[1:6],
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = "Food_SMEB",
                                                       multiple = FALSE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Country' | (input.plot_aggregation == 'Governorate' & input.plot_by_governorate_item == 'Item') | (input.plot_aggregation == 'District' & input.plot_by_district_item == 'Item')",
                                           pickerInput("select_byitem_item",
                                                       label = "Item(s):",
                                                       choices = lapply(split(indicator_list$Variable, indicator_list$Group), as.list)[1:6],
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = c("SMEB", "Food_SMEB", "WASH_SMEB"),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'District' & input.plot_by_district_item == 'Item'",
                                           pickerInput("select_byitem_district",
                                                       label = "District:",
                                                       choices = lapply(split(plot_location_list$District, plot_location_list$Governorate), as.list),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = "Al Mansura",
                                                       multiple = FALSE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Governorate' & input.plot_by_governorate_item == 'Item'",
                                           pickerInput("select_byitem_governorate",
                                                       label = "Governorate:",
                                                       choices = unique(plot_location_list$Governorate),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = "Aden",
                                                       multiple = FALSE
                                           )
                          ),
                          
                          hr(),
                          
                          conditionalPanel(condition = "input.plot_aggregation != 'Country' | (input.plot_aggregation == 'Country' & input.plot_type == 'Line Graph')",
                                           sliderTextInput("select_date",                                                # set date slider
                                                           "Months:",
                                                           force_edges = TRUE,
                                                           choices = dates,
                                                           selected = c(dates_min, dates_max)
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Country' & input.plot_type == 'Boxplot'",
                                           sliderTextInput("select_date_boxplot",                                        # set date slider
                                                           "Month:",
                                                           force_edges = TRUE,
                                                           choices = dates,
                                                           selected = dates_max
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation != 'Country' | (input.plot_aggregation == 'Country' & input.plot_type == 'Line Graph')",
                                           prettySwitch(
                                             inputId = "select_index",
                                             label = "Index series",
                                             status = "success",
                                             fill = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.select_index == true & (input.plot_aggregation != 'Country' | (input.plot_aggregation == 'Country' & input.plot_type == 'Line Graph'))",
                                           sliderTextInput("select_date_index",
                                                           "Reference Month:",
                                                           force_edges = TRUE,
                                                           choices = dates,
                                                           selected = dates_max
                                           ),
                          ),
                          
                          h6("Select aggregation level, item(s), location(s) and month from drop-down menues to update plot.
                                    Displayed values are median prices - retail prices are first aggregated on site level and then
                                    on district level (and then on governorate/country level)."),
                          
                          absolutePanel(id = "dropdown", bottom = 20, left = 20, width = 200,                            # define blue info button
                                        fixed=TRUE, draggable = FALSE, height = "auto",
                                        dropdown(
                                          h4("SMEB contents"),
                                          # column(
                                          #   HTML(smeb_kbl),
                                          #   width = 6),
                                          column(p(h6("The Survival Minimum Expenditure Basket (SMEB) represents the minimum culturally adjusted group of items
                                                               required to support a six-person Yemeni household for one month, as defined by the CWG."))
                                                 ,
                                                 p(h6("The SMEB reported on this website only includes the food and water components, including a lump sump amount for
                                                               services(electricity, communication and transportation) and non-food items.")),
                                                 # p(h6("The composition of the SMEB was revised twice: 1) In the September
                                                 # 2018 round and onwards, the current water component replaced the fuel component. 2) Since January 2020,
                                                 #               the SMEB furthermore includes modified food and NFI components.")),
                                                 # p(h6("More details on the SMEB can be found here:",
                                                 #      tags$a(href="https://www.humanitarianresponse.info/en/operations/iraq/document/survival-minimum-expenditure-basket-technical-guidance-note-october-2019",
                                                 #             "SMEB Guidance Note"), "."))
                                                 # ,
                                                 width = 5),
                                          width = "650px",
                                          # tooltip = tooltipOptions(title = "Click for more details on the SMEB."),
                                          size = "xs",
                                          up = TRUE,
                                          style = "jelly", icon = icon("info"),
                                          animate = animateOptions(
                                            enter = "fadeInLeftBig",
                                            exit  = "fadeOutLeft",
                                            duration = 0.5)
                                        )
                          ),
                          
                          width = 3,                                                                    # set bootstrap width of sidebar (out of 12)
                        ),                                                                                # close sidebar panel
                        
                        mainPanel(
                          br(),
                          tags$i(textOutput("plot_text"), style = "color: red"),                        # display error message displayed if there is no data available
                          highchartOutput("graph", width = "100%", height = "600px"),                   # display large chart
                          width = 8,                                                                    # set width of main panel (out of 12, as per bootstrap logic)
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Country' & input.plot_type == 'Boxplot'",
                                           tags$i(h6("The boxplots are built with district medians and illustrate the variation of prices across the country.", style="color:#045a8d; text-align:center"))
                          )
                        )
                      )
             ),
             
             
             #### Data Explorer Page ######################################################################
             
             tabPanel("Explorer", icon = icon("table"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioGroupButtons("table_aggregation",
                                            label = "Aggregation level:",
                                            choices = c("District", "Key Informant"),
                                            selected = "District",
                                            justified = TRUE
                          ),
                          
                          conditionalPanel(condition = "input.table_aggregation != 'District'",
                                           # condition = TRUE,
                                           tags$i(h6("Note: Only district-level data can be displayed in the table on the right.
                                                            You can download data on either aggregation level by setting your desired
                                                            parameters and clicking on the download button below.",
                                                     style="color:red")),
                          ),
                          
                          hr(),
                          
                          conditionalPanel(condition = "input.table_aggregation == 'District'",
                                           # condition = TRUE,
                                           pickerInput("table_show_vars",
                                                       label = "Indicators:",
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       choices = lapply(split(indicator_list$Variable, indicator_list$Group), as.list),
                                                       selected = c("WASH SMEB", "Food SMEB", "Parallel Exchange Rates"),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.table_aggregation == 'Key Informant'",
                                           pickerInput("table_show_vars_ki",
                                                       label = "Indicators:",
                                                       choices = names(full_data),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = names(full_data),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          pickerInput("table_district",
                                      label = "Districts:",
                                      choices = lapply(split(plot_location_list$District, plot_location_list$Governorate), as.list),
                                      options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                      selected = plot_location_list$District,
                                      multiple = TRUE
                          ),
                          
                          sliderTextInput("table_date_select",
                                          "Months:",
                                          force_edges = TRUE,
                                          choices = dates,
                                          selected = c("2021-03-01", "2021-03-01")
                                          # ,selected = c(dates_min, dates_max)
                          ),
                          
                          hr(),
                          
                          actionButton("table_reset", "Reset filters"),
                          
                          downloadButton("downloadData", "Download as CSV"),
                          
                          width = 3
                        ),
                        
                        mainPanel(
                          DT::dataTableOutput("table", width = "100%", height = "100%")
                          # ,width = 9
                        )
                      )
             ),
             
             
             tabPanel("Tracker",
                      
                      
                      style=("{overflow-y:auto; }"),
                      icon= icon("bar-chart"), #info-circle
                      div(tags$head(
                        # Include our custom CSS
                        tags$style(".fa-check {color:#008000}"),
                        tags$style(HTML(".sidebar {height:50vh; overflow-y:auto; }"))
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("months","Number of months displayed", min = 1, max = 24, step = 1,value = 6, ticks = F),
                          h6("Displays the number of months from the most recent dataset"),
                          br(),
                          br(),
                          sliderInput("percent","Percentage change highlighted", min = 1, max = 100, value = 20, tick=F),
                          h6("Is the percent difference desired for the benchmark")
                          # ,width=2.5
                        )
                        ,mainPanel(
                          h2("Monthly SMEB Costs and Percentage Change from Standard SMEB Values"),
                          DT::dataTableOutput("table_smeb"),
                          tags$hr(),
                          h2("Monthly Cost for Other non-SMEB goods"),
                          DT::dataTableOutput("table_other")
                        )
                      )
                      
                      
                      
                      
                      )),
             
             
             ###..................................I N F O. . P A G E ..........................................
             tabPanel("Information",
                      tags$head(tags$style("{ height:90vh; overflow-y: scroll; }")),
                      
                      icon= icon("info"), #info-circle
                      div(#class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          shiny::includeCSS("styles.css"),
                          style=" { height:90vh; overflow-y: scroll; }
                                              "),
                        
                        column(width=8,h3("Overview")), #h1- h5 change the header level of the text
                        
                        column(width=7,h5("The  Yemen  Joint  Market  Monitoring  Initiative  (JMMI) is an
                                          initative led by REACH in collaboration with the Water, Sanitation,
                                          and Hygiene (WASH) Cluster  and the Cash and Market Working Group (CMWG)
                                          to support humanitarian cash actors with the harmonization of price
                                          monitoring throughout Yemen. The basket of goods assessed includes eight
                                          non-food items (NFIs), including fuel, water and hygiene products,
                                          reflecting the programmatic areas of the WASH Cluster. The JMMI
                                          tracks all components of the WASH Survival Minimum Expenditure Basket
                                          (SMEB) since September 2018.")),
                        
                        column(width=8,h3("Methodology")), #h1- h5 change the header level of the text
                        
                        column(width=7,h5("Data was collected through interviews with vendor Key Informants
                                          (KIs), selected by partner organizations from markets of various sizes
                                          in both urban and rural areas. To be assessed by the JMMI, markets
                                          must be either a single permanent market, or a local community where
                                          multiple commercial areas are located in close proximity to one another.
                                          When possible, markets/shops are selected within a single geographical
                                          location, where there is at least one wholesaler operating in the
                                          market, or multiple areas of commerce within the same geographical
                                          location when it is too small, to provide a minimum of three price
                                          quotations per assessed item.", tags$i(tags$strong("Findings are indicative for the assessed
                                          locations and timeframe in which the data was collected.")))),
                        
                        column(width=8,h3("SMEB Calculation")), #h1- h5 change the header level of the text
                        
                        column(width=7,h5("Each month, enumerators conduct KI interviews with market vendors to collect three price quotations for each item from the same market in each district.
                                          REACH calculates the WASH SMEB,
                                          which is composed of four median item prices: Soap (1.05 kg), Laundry Powder (2 kg), Sanitary Napkins (20 units) ,and Water Trucking (3.15 m3).",
                                          p(),
                                          p("The calculation of the aggregated median price for districts and governorates is done following a stepped approach.
                                          Firstly, the median of all the price quotations related to the same market is taken. Secondly, the median quotation from each market is aggregated to calculate the district median.
                                          Finally, the median quotation from each district is aggregated to calculate the governorate median. "))),
                        
                        
                        column(width=8,h3("About REACH")), #h1- h5 change the header level of the text
                        
                        column(width=7,h5("REACH is a joint initiative that facilitates the development of
                                          information tools and products that enhance the capacity of aid actors
                                          to make evidence-based decisions in emergency, recovery and development
                                          contexts. By doing so, REACH contributes to ensuring that communities
                                          affected by emergencies receive the support they need. All REACH
                                          activities are conducted in support to and within the framework of
                                          inter-agency aid coordination  mechanisms. For more information, please
                                          visit our",a("REACH Website", target="_blank",    href="https://www.reach-initiative.org"), "or contact us directly
                                          at yemen@reach-initiative.org.")),
                        
                        hr(),
                        p(),
                        p(),
                        hr(),
                        
                        tags$div(id="cite4",
                                 a(img(src='reach_logoInforming.jpg', width= "200px"), target="_blank", href="http://www.reach-initiative.org")))
             )
             # ,
             
             
             #### Partners Page ######################################################################
             
             # tabPanel("Partners",
             # 
             #          #style=("{overflow-y:auto; }"),
             #          icon= icon("handshake"), #info-circle
             #          div(tags$head(
             #            # Include our custom CSS
             #            tags$style(".fa-check {color:#008000}"),
             #            tags$style(HTML(".sidebar {height:50vh; overflow-y:auto; }"))
             #          ),
             # 
             #          column(width=8,h3("Partners (Past and Present)")), #h1- h5 change the header level of the text
             #          column(width=7, h6(tags$i("Check marks indicate that the partner participated in the most recent month’s JMMI"))),
             # 
             #          #icon("check", "fa-2x")),
             #          #list of partners orgs
             #          column(width=12,align = "center", h5("Agency for Technical Cooperation and Development (ACTED)",icon("check", "fa-2x")),a(img(src='0_acted.png', height= "50px"), target="_blank", href="https://www.acted.org/en/countries/yemen/")),
             #          column(width=12,align = "center", h5("Al Maroof",icon("check", "fa-2x")),a(img(src='0_almaroof.jpg', height= "50px"), target="_blank")),
             #          column(width=12,align = "center", h5("Adventist Development and Relief Agency (ADRA)"), a(img(src='0_adra.png', height= "50px"), target = "_blank", href="https://adra.org/")),
             #          column(width=12,align = "center", h5("Al Thadamon Association"), img(src='0_thadamon.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("Brains for Development (B4D)"),img(src='0_b4d.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("Benevolence Coalition for Humanitarian Relief (BCHR)", icon("check", "fa-2x")), a(img(src='0_bchr.jpg', height= "50px"), target="_blank", href="http://bchr-ye.org/")),
             #          column(width=12,align = "center", h5("Creative Youth Forum (CYF)"), a(img(src='0_cyf.jpg', height= "50px"),target="_blank", href="https://www.facebook.com/cyf.org77/")),
             #          column(width=12,align = "center", h5("Danish Refugee Council (DRC)", icon("check", "fa-2x")), a(img(src='0_drc.png', height= "50px"), target="_blank", href="http://www.drc.dk")),
             #          column(width=12,align = "center", h5("Generations without Qat (GWQ),",icon("check", "fa-2x")), img(src='0_gwq.png', height= "50px")),
             #          #column(width=12,align = "center", h5("LLMPO"), img(src='0_cyf.png', height= "50px")),
             #          column(width=12,align = "center", h5("International Organization for Migration (IOM)",icon("check", "fa-2x")), img(src='0_iom.png', height= "50px"), target="_blank", href="https://www.iom.int/countries/yemen"),
             #          column(width=12,align = "center", h5("International Rescure Committee (IRC)", icon("check", "fa-2x")), a(img(src='0_IRC.jpg', height= "50px"), target="_blank", href="https://www.rescue.org/")),
             #          column(width=12,align = "center", h5("Mercy Corps (MC)"), img(src='0_mercy.jfif', height= "50px")),
             #          column(width=12,align = "center", h5("National Foundation for Development and Humanitarian Response (NFDHR)"), a(img(src='0_nfdhr.png', height= "50px"),target="_blank", href="http://nfdhr.org/")),
             #          column(width=12,align = "center", h5("National Forum Human Development (NFHD)"), img(src='0_nfhd.png', height= "50px")),
             #          column(width=12,align = "center", h5("Norweigan Refugee Council (NRC)", icon("check", "fa-2x")), img(src='0_nrc.png', height= "50px")),
             #          column(width=12,align = "center", h5("Old City Foundation for Development (OCFD)"), img(src='0_ocfd.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("OXFAM", icon("check", "fa-2x")), img(src='0_oxfam.png', height= "50px")),
             #          column(width=12,align = "center", h5("Rising Org. for Children Rights Development (ROC)"), a(img(src='0_roc.jpg', height= "50px"), target="_blank", href="https://rocye.org/")),
             #          column(width=12,align = "center", h5("Sama Al Yemen", icon("check", "fa-2x")), img(src='0_sama.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("Save the Children (SCI)",icon("check", "fa-2x")), img(src='0_sci.png', height= "50px")),
             #          column(width=12,align = "center", h5("STEPS", icon("check", "fa-2x")), img(src='0_steps.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("Sustainable Development Foundation (SDF)"), img(src='0_sdf.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("Solidarites International (SI)", icon("check", "fa-2x")), img(src='0_si.jpeg', height= "50px")),
             #          column(width=12,align = "center", h5("Soul Yemen"), img(src='0_soul.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("Tamdeen Youth Foundation (TYF)",icon("check", "fa-2x")), img(src='0_tyf.png', height= "50px")),
             #          column(width=12,align = "center", h5("Vision Hope"), img(src='0_vision.png', height= "50px")),
             #          column(width=12,align = "center", h5("Yemen Family Care Association (YFCA)"), img(src='0_yfca.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("Yemen Shoreline Development (YSD)"), img(src='0_ysd.jpg', height= "50px")),
             #          column(width=12,align = "center", h5("ZOA Yemen",icon("check", "fa-2x")), img(src='0_zoa.PNG', height= "50px")),
             #          p(),
             #          hr()
             # 
             # 
             #          ))
             
  )
)

# SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER

server <- function(input, output, session) {
  
  #### Home ######################################################################
  
  output$map_home <- renderLeaflet({
    map_home <- leaflet(options = leafletOptions(attributionControl=FALSE, zoomControl = FALSE, dragging = FALSE, minZoom = 1, maxZoom = 12)) %>%
      setView(lng = 48.5164, lat = 17.5527, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB",
                       options = providerTileOptions(opacity = 0.8))
  })
  
  output$downloadDataLatest <- downloadHandler(
    filename = function() {
      paste("YEM-JMPI-download-", format(dates_max, "%b"), "-", format(dates_max, "%Y"),".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_latest, file, row.names = FALSE)
    }
  )
  
  output$downloadFactsheet <- downloadHandler(
    filename = function() {
      paste("YEM-JMPI-download-situation-overview-", format(dates_max, "%Y"),".pdf", sep = "")
    },
    content = function(file) {
      a(paste0(dates_max," Situation overview"), target="_blank",    href="https://reliefweb.int/sites/reliefweb.int/files/resources/REACH_YEM_JMMI_Situation-Overview_March-2021.pdf")
    }
  )
  
  
  
  #_________________________create map consisting of several layers and customization___________________
  
  output$map1 <- renderLeaflet({                                                # initiate map
    leaflet(options = leafletOptions(minZoom = 4.5)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%                      # Base map, can be changed
      setView(48.5164,15.5527, zoom = 6.5)
    # setMaxBounds( lng1 = -66.9, lat1 = 37, lng2 = -66.1, lat2 = 37.8 )
    })
  
  # "observe" inputs to define variables for map colors, titles, legends and subset district data
  observe({
    date_map_selected <- input$date_map
    # date_map_selected <- dates[33]
    VARIA <- input$variable1
    # VARIA <- "SMEB"
    VARIA <- indicator_list[indicator_list$Item==input$variable1, "Variable"]
    metacol <- c("admin2pcod", "admin1name", "admin1pcod", "admin2name", "admin2refn")
    metacol2 <- c("num_obs", "date2")
    
    # VARIA = "Food_SMEB"
    dataM_all <- Rshp[,c(metacol, VARIA, metacol2)]                             # subset VARIA column
    dataM <- dataM_all[dataM_all@data$date2==date_map_selected,]                # filter for the selected date 
    
    all.na <- sum(is.na(dataM@data[,VARIA]))==nrow(dataM@data)                  # flag when all values are NAs for this date
    pal_domain <- if (all.na){c(0,1)} else {dataM@data[,VARIA]}                 # adapt the palette domain
    mypal <- colorNumeric(palette = indicator_list[indicator_list$Variable==VARIA, "Palette"][[1]],
                          domain = pal_domain,
                          na.color = "#FBFBFB"
                            # "#D0CFCF"
                          ,reverse = F)

    pLa <- paste0(indicator_list[indicator_list$Variable==VARIA, "Item"],": ")
    pLa2 <- indicator_list[indicator_list$Variable==VARIA, "Item"]
    unitA <- indicator_list[indicator_list$Variable==VARIA, "Unit"]
    title_legend <- indicator_list[indicator_list$Variable==VARIA, "Legend"]

    dataM_NAs <- dataM@data %>% dplyr::filter(is.na(!!sym(VARIA))) %>% pull(admin2pcod) # Have a vector of all of the districts that currently have no data for the current month
    call_name <- colnames(dataM@data[6])                                        # get name of variable selected

    # Need to subset out for with districts have had values in the past that arent in this current month, get that as a list
    Admin2data_out <- Admin2table %>%                                           # subset out recent month dates to attach to shapefile
      dplyr::filter(district_ID %in% dataM_NAs) %>%                             # next only keep data from places that have had an observation in the past from the right varialbe
      dplyr::filter(!is.na(get(call_name)))

    Admin2data_out <- Admin2data_out[!duplicated(Admin2data_out$district_name),]
    Rshp@data <- Rshp@data %>% dplyr::mutate(alt_dist = (admin2pcod %in% Admin2data_out$district_ID)*1 %>% dplyr::na_if(0))

    # old_dist <- Rshp[Rshp@data$date2==date_map_selected,c(metacol, "alt_dist")]          # final dataset that needs to be pulled from the right Rshp (this is after you dynamically pull out the right data depending on what is selected)
    # old_dist_alt <- sf::st_as_sf(old_dist) %>%                                  # convert the data list (shapefile list) to a more useable list we can filter from while holding the coordinates https://gis.stackexchange.com/questions/156268/r-filter-shapefiles-with-dplyr/156290
    #   dplyr::filter(alt_dist == 1)                                              # actually do a filter on the stuff you want (this corresponds to the Rshp 31 and the 1 and NA we did before), basically its a janky way to do a clip on the fly from a GIS perspective
    # old_dist_alt_sp <- sf::as_Spatial(old_dist_alt)                             # convert back to a spatial datalist https://gis.stackexchange.com/questions/239118/r-convert-sf-object-back-to-spatialpolygonsdataframe
    # pal_alt <- colorBin(palette="#E5E5E5", domain=old_dist_alt_sp@data[,"alt_dist"]) # create a new color pallete for the new over lay

    map1 <- leafletProxy("map1") %>%                                            # leaflet map proxy updates created map to reflect obeserved changes
      clearShapes() %>%                                                         # clear polygons, so new ones can be added
      clearControls()%>%                                                        # reset zoom etc

      addLabelOnlyMarkers(centroids,                                            # ADD governorate LABELS
                          lat=centroids$lat,
                          lng=centroids$lon,
                          label=as.character(centroids$admin1name),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE, interactive = FALSE, direction = "bottom", textOnly = TRUE, offset = c(0, -10), opacity = 0.6,
                            style = list("color" = "#222224", "font-size" = "12px", "font-family"= "Helvetica", "font-weight"= 500)
                          )) %>%

      addPolygons(data = dataM,                                                  # Add subsetted district shapefiles
                  color = "#58585A", weight = 0.25,
                  label = lapply(paste0(dataM$admin2name," (", pLa, dataM@data[,6], indicator_list[indicator_list$Item==input$variable1, "Unit"],")<p>Number of market assessed: ", dataM@data[,"num_obs"]), htmltools::HTML),
                  # paste0(dataM$admin2name," (", pLa, dataM@data[,6]," )"),
                  opacity = 0.5, smoothFactor = 0.8, fill = TRUE, fillOpacity = 0.8,
                  fillColor =  (~mypal((dataM@data[,6]))), # custom palette
                  layerId = ~admin2pcod,
                  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = FALSE, sendToBack = FALSE),
                  popup = paste0(dataM$admin2name, "<br>",'<h7 style="color:black;">', pLa, "<b>"," ", dataM@data[,6],unitA, "</b>", '</h7>'),
      ) %>%
      # addPolygons(data = old_dist_alt_sp,                                        # Clipped data file of previous districts (make sure it is below your main district on or it will not be seen)
      #             color = "#EE5859", weight = 0.35,
      #             label = paste0(old_dist_alt_sp$admin2name,":previous ",pLa2," data present"), #added a different label that pops up
      #             opacity = 1, smoothFactor = 0, fill = TRUE, fillOpacity = .8,
      #             fillColor = ~pal_alt(old_dist_alt_sp@data[,"alt_dist"]),      # Custom palette as stated before
      #             layerId = ~admin2pcod,
      #             highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = FALSE, sendToBack = FALSE),
      # )%>%
      addPolylines(data = Admin1, weight= 0.5, stroke = T,                      # Add governorate lines for reference
                   color = "#58585A", fill=FALSE, fillOpacity = 0.1, opacity = 1)

    map1 %>% clearControls()

    # Needed to make a custom label because i hate R shiny https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
    # colors<-c("white" ,"#D3D3D3", "#D3D3D3")
    # labels<-c("Districts with previous data", "Governorate borders", "District borders")
    # sizes<-c("20", "20", "20")
    # shapes<-c("square", "line", "line")
    # borders<-c("red", "#2B2B2B", "#646464")
    colors<-c("white")
    labels<-c("Districts with previous data")
    sizes<-c("20")
    shapes<-c("square")
    borders<-c("#EE5859")
    addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){

      make_shapes <- function(colors, sizes, borders, shapes) {
        shapes <- gsub("circle", "50%", shapes)
        shapes <- gsub("square", "0%", shapes)
        paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
      }
      make_labels <- function(sizes, labels) {
        paste0("<div style='display: inline-block;height: ",
               sizes, "px;margin-top: 4px;line-height: ",
               sizes, "px;'>", labels, "</div>")
      }

      legend_colors <- make_shapes(colors, sizes, borders, shapes)
      legend_labels <- make_labels(sizes, labels)

      return(addLegend(map1,"topleft", colors = legend_colors, labels = legend_labels, opacity = 0.5))
    }

    map1 %>%
      addControl(c(""), position = "topleft" )
      # %>% addControl(c(""), position = "topleft" ) # add an empty legend to fix formatting issue with map [not satisfying in long run, CSS define positioning, hard to update]
    map1 %>% addScaleBar("topleft", options = scaleBarOptions(maxWidth = 100, metric = T, imperial = T, updateWhenIdle = T)) # add scale bar
    # map1 %>% addLegendCustom(colors, labels, sizes, shapes, borders)            # add new legend

    # add legend for var
    map1 %>% 
      execute_if(!all.na, 
                 addLegend_decreasing("topleft", pal = mypal, values =  dataM@data[,6], # update legend to reflect changes in selected district/variable shown
                                      labFormat=labelFormat(suffix=unitA), title = title_legend, opacity = 5, decreasing = T))
    
  })                                                                            # end of MAP

  #_________________________create reactive objects for use in the chart___________________
  clicked_state<- eventReactive(input$map1_shape_click,{ #capture ID of clicked district
    return(input$map1_shape_click$id)
  })
 
  clicked_state_gov<-eventReactive(input$map1_shape_click,{
    gov_id<- substr(input$map1_shape_click$id,1,nchar(input$map1_shape_click$id)-2)
    return(gov_id)
  })

  dist_data<-reactive({
    dist_dat<-Admin2table[Admin2table$district_ID==clicked_state(),] #strangely reactive objects are stored as functions
    dist_dat
  })

  gov_data<-reactive({
    gov_dat<-Admin1table[Admin1table$government_ID==clicked_state_gov(),] #adding the government data to the dataset
    gov_dat
  })

  nat_data<-reactive({
    nat_dat<-AdminNatTable
    nat_dat
  })

  gov_nat_data<-reactive({
    gov_nat_dat<-right_join(gov_data(),nat_data(), by = "date2")
  })

  state_data <- reactive({ #subset JMMI data table based on clicked state
    all_dat<-right_join(dist_data(),gov_nat_data(), by = "date2")               # using a full join so that the data that was for the other month when district wasnt select is still shown
    all_dat$date<-as.yearmon(all_dat$date2)
    all_dat
  })

  chartData1<-reactive({  #subset JMMI data table based on variable of interest (soap, water etc)
    metacol<-c("date", "government_name.x",  "government_ID.x", "district_name", "district_ID")
    # var <- as.character(input$variable1)
    var <- indicator_list[indicator_list$Item==input$variable1, "Variable"]
    # var="WASH_SMEB"
    # clicked_state<-"YE1514"
    # state_data <- right_join(Admin2table[Admin2table$district_ID==clicked_state,], right_join(Admin1table[Admin1table$government_ID==substr(clicked_state,1,4),], AdminNatTable, by = "date2"), by = "date2")
    r <- state_data() %>% dplyr::select(all_of(metacol), var, "date2", starts_with(paste0(var,".")), contains("num_obs"))
    colnames(r) <- c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    r
  })

  chartNAME<-reactive({
    # y = indicator_list[indicator_list$Variable==input$variable1,"Item"] %>% as.character  #define element to be used as title for selected variable
    y = input$variable1 %>% as.character  #define element to be used as title for selected variable
    })

  #_________________________Create highcharter element which uses dataset filtered by user inputs___________________
  #NEW OUT PUT FOR DATA TO BE SUBBED LATER
  #https://stackoverflow.com/questions/38113507/r-shiny-keep-retain-values-of-reactive-inputs-after-modifying-selection
  #https://stackoverflow.com/questions/57468457/how-can-i-set-the-yaxis-limits-within-highchart-plot

  observe({
    updateSelectInput(session = session, inputId = "varDateSelect", choices = chartData1()$date, selected=lapply(reactiveValuesToList(input), unclass)$varDateSelect)
  })

  output$hcontainer <- renderHighchart({
    event <- (input$map1_shape_click)                                           # Critical Line!!!

    (validate(need(event$id != "", "Please click on a district to display its history.")))

    ChartDat<-chartData1()                                                      # Define filtered table that is reactive element chartData1
    #y_min<- chartDatMIN()
    #y_max<- chartDatMAX()

    chosenD <- paste0(na.omit(unique(ChartDat[,2])),", ", na.omit(unique(ChartDat[,4]))) # TITLE FOR CHART (governorate and district name)

    highchart() %>% # high chart
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b %Y')) %>%
      #data for national
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, nat_val), color = "dodgerblue", name=paste(chartNAME(),"- National")) %>%
      #data for governorate
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, governorate_val), color = "forestgreen", name=paste(chartNAME(),"- Governorate")) %>%
      #data for district
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, variableSEL), color="#4F4E51", name=paste(chartNAME(),"- District")) %>%
      
      hc_yAxis(tithcle=list(
        text=paste0(chartNAME()," in YER")
        # text = "Price in YER test"
        ), opposite = FALSE
               #,min= as.numeric(y_min), max= as.numeric(y_max)
      ) %>%
      hc_title(text=chosenD) %>%
      hc_add_theme(hc_theme_gridlight()) %>%
      hc_plotOptions(line = list(lineWidth=1.5, dataLabels = list(enabled = FALSE)))

  })
  #building the table for the observations and prices
  #BUILDING TABLE  https://stackoverflow.com/questions/32149487/controlling-table-width-in-shiny-datatableoutput
  output$out_table_obs<-DT::renderDataTable({
    ChartDat_Date<-chartData1()                                                 # make a new dataset to play around with from the original state data one above // # https://stackoverflow.com/questions/40152857/how-to-dynamically-populate-dropdown-box-choices-in-shiny-dashboard
    ChartDat_Date_filter<-ChartDat_Date%>%                                      # filter out based on what was selected from the varDateSelect
      dplyr::filter(date == input$varDateSelect)
    chosenD_Date <- paste0(na.omit(unique(ChartDat_Date[,2])),", ",na.omit(unique(ChartDat_Date[,4]))) # make a quick title for the data table
    mat_date_test <- matrix(c(round(ChartDat_Date_filter[6],2),
                            round(ChartDat_Date_filter[8],2),
                            round(ChartDat_Date_filter[9],2),
                            ChartDat_Date_filter[10],
                            ChartDat_Date_filter[11],
                            ChartDat_Date_filter[12]),
                          nrow=2, ncol=3, byrow = T,
                          dimnames=list(c("Median Price (YER)","Number of Markets Assessed"),c(paste0("District: \n",na.omit(unique(ChartDat_Date[,4]))),paste0("Governorate: \n",na.omit(unique(ChartDat_Date[,2]))),"Yemen")))
    DT::datatable(mat_date_test,options = list(dom = 't'))
  })


  # output infobox for the info exchange rate
  output$info_exchange<-renderValueBox({
    exchange_data<-AdminNatTable
    exchange_data$date<-as.yearmon(exchange_data$date)
    exchange_date<-exchange_data%>%
      dplyr::filter(date == input$varDateSelect)
    exchange_rate<-exchange_date[1,10]
    valueBox(value = exchange_rate, subtitle="YER to 1 USD", icon = icon("dollar"), color = "green") # fill=T, dropped argument
    })

  output$text1 <- renderUI({ #customised text elements
    HTML(paste("Coordinate System:  WGS 1984",
               "Administrative boundaries:  OCHA",
               "R Mapping Packages:  leaflet, shiny, highcharter",
               "Please do NOT use Microsoft Edge for best user interaction",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 12px;
         font-family: Helvetica} </style>')
    })

  output$text2 <- renderUI({
    HTML(paste("<i>Note: Data displayed on this map should be interpreted as indicative.
               In addition, designations and boundaries used here do not imply acceptance
               by REACH partners and associated donors.</i>",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;
         font-family: Helvetica} </style>')
    })

  output$text3 <- renderText({                                                  # LARGE TEXT ABOVE CHART
    if (indicator_list[indicator_list$Item==input$variable1, "Group"] %in% c("VI. Other indicators")) {
      paste(chartNAME(), " ", "Average over time")
    } else {paste(chartNAME(), " ", "Medians over time")}
    })

  output$text_DT<-renderText({
    if (indicator_list[indicator_list$Item==input$variable1, "Group"] %in% c("VI. Other indicators")) {
      paste(chartNAME(), " ", "Average % of surveyed vendors, and number of markets assessed")
    } else {paste0(chartNAME(), "Median monthly costs, and number of markets assessed")}
    # paste0(chartNAME(),", Median Monthly Costs, and Number of Markets Assessed")
    })

  output$text4 <- renderUI({
    HTML(paste("<i>For more information, please visit our",a("REACH Website", target="_blank", href="https://www.reach-initiative.org"),
               "or contact us directly at yemen@reach-initiative.org.</i>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;
         font-family: Helvetica} </style>')
    })

  # FRoM ONLINE https://github.com/ua-snap/shiny-apps/blob/master/cc4liteFinal/server.R

  #######################
  #SMEB tracker
  #######################
  # Goal - build system that can adapt to X# of months back and benchmark at YY%
  # Data needed - national but may need to clip to districts of each individual one based on methodology
  # So pull number of district from most recent month and clip back based on X# of months.
  # then build a table function that allows for that stuff to be adapted

  # build the dataset
  # SMEB DATASET
  output$table_smeb<-DT::renderDataTable({
    #https://stackoverflow.com/questions/50912519/select-the-number-of-rows-to-display-in-a-datatable-based-on-a-slider-input
    time<-input$months
    percent_time<- input$percent/100

    # time<-6
    national_data_test<-nat_data()
    national_data_test<-AdminNatTable
    national_data_test$date2 <- as.yearmon(national_data_test$date)
    national_data<-arrange(national_data_test,desc(date2))

    month_all<-sort(unique(national_data$date2),decreasing = T)
    time_pull<-month_all[time]
    month_list<-month_all[1:match(time_pull,month_all)]

    #now have the month_list which we can cut from in the future

    national_data_pull<-dplyr::filter(national_data, date2 %in% month_list)%>%
      dplyr::select(-c(date,num_obs,exchange_rates))
    
    national_data_pull<-national_data_pull%>%
      reshape2::melt("date2")%>%
      reshape2::dcast(variable ~ date2)%>%
      round_df(.,0) %>% dplyr::select(variable, as.character(month_list))

    col_data_pull<-ncol(national_data_pull)
    name_perc_change<-paste0(colnames(national_data_pull[col_data_pull]), " percent change from standard SMEB")
    
    #Add SMEB base costs
    #https://stackoverflow.com/questions/13502601/add-insert-a-column-between-two-columns-in-a-data-frame
    national_data_pull<-national_data_pull%>%
      # add_column(.,  `Standard SMEB Values`= c(365,430,100,120,130,105,525,1825,12000,42000), .after = 1)%>%
      # add_column(.,  `Variable`= c("Petrol","Diesel","Bottled water","Treated water","Soap","Laundry powder","Sanitary napkins","Water trucking","SMEB total"), .after = 1)%>%
      dplyr::mutate(variable=gsub("_"," ",str_to_title(variable))) %>%
      dplyr::rename(Variable = variable)%>%
      dplyr::filter(Variable %in% c("Soap","Laundry powder","Sanitary napkins","Water trucking","Wash smeb", "Wheat flour", "Beans dry", "Vegetable oil", "Sugar", "Salt", "Food smeb")) %>%
      dplyr::mutate(Variable=gsub("smeb", "SMEB", Variable))

    #get number of columns now we will use later in the formatting of the table
    columns_of_data_begin<-ncol(national_data_pull)+1

    #get the column number of the percent change for future formatting
    percent_col<-time+2
    col_format_last<-time+1

    # This is where the percentage change are calculated. It was  calculated compared to some "standard" SMEB value, which is weird
    # It now computes percentage change between t and t-n for each month n
    #https://duckduckgo.com/?q=dynamic+naming+in+mutate+R&t=brave&ia=web
    national_data_pull <- national_data_pull %>%
      dplyr::mutate_at(., .vars = c(3:ncol(.)), .funs = list('change' = ~(((national_data_pull[,2])-.)/(.)))) %>%
      dplyr::rename_at(vars(contains("_change")), ~paste0("% change between ", ., " and ",  month_list[1])) %>% 
      setNames(gsub("_change", "", colnames(.)))

    columns_of_data_end <- ncol(national_data_pull)                             # get number of columns now we will use later in the formatting of the table
    names(national_data_pull) <- gsub("_", " ", names(national_data_pull))      # get rid of the weird naming from the mutate_at

    #Render the output DT
    #https://stackoverflow.com/questions/60659666/changing-color-for-cells-on-dt-table-in-shiny
    #https://blog.rstudio.com/2015/06/24/dt-an-r-interface-to-the-datatables-library/
    DT::datatable(national_data_pull,extensions = c('FixedColumns'),
                  options = list(
                    autoWidth=F,
                    searching = F, paging = F, scrollX=T, fixedColumns = list(leftColumns = 2, rightColumns = 0)),
                  rownames = F) %>%
      formatStyle(columns = 1, color = "white", backgroundColor = "grey", fontWeight = "bold") %>%
      DT::formatPercentage(columns = c(columns_of_data_begin:columns_of_data_end),2) %>%
      formatStyle(columns = c(columns_of_data_begin:columns_of_data_end),
                  color = styleInterval(c(-percent_time,percent_time), c('grey', 'black','white')),
                  backgroundColor = styleInterval(c(-percent_time,percent_time), c('#66FF66', 'white','#FA5353')),
                  fontWeight = styleInterval(c(-percent_time,percent_time),c('bold','normal','bold')))
    })


  #Other goods dataset
  output$table_other<-DT::renderDataTable({
    #https://stackoverflow.com/questions/50912519/select-the-number-of-rows-to-display-in-a-datatable-based-on-a-slider-input
    time<-input$months
    percent_time<- input$percent/100
    
    # time<-11
    national_data_test<-nat_data()
    national_data_test<-AdminNatTable
    national_data_test$date2 <- as.yearmon(national_data_test$date)
    national_data<-arrange(national_data_test,desc(date2))

    month_all<-sort(unique(national_data$date2),decreasing = T)
    time_pull<-month_all[time]
    month_list<-month_all[1:match(time_pull,month_all)]

    #now have the month_list which we can cut from in the future
    national_data_pull <- dplyr::filter(national_data, date2 %in% month_list)%>%
      dplyr::select(-c(date,num_obs,exchange_rates))
    
    national_data_pull <- national_data_pull %>%
      reshape2::melt("date2")%>%
      reshape2::dcast(variable ~ date2)%>%
      round_df(.,0) %>% dplyr::select(variable, as.character(month_list))

    col_data_pull <- ncol(national_data_pull)
    name_perc_change<-paste0(colnames(national_data_pull[col_data_pull]),
                             " percent change from standard SMEB")
    #Add SMEB base costs
    #https://stackoverflow.com/questions/13502601/add-insert-a-column-between-two-columns-in-a-data-frame
    national_data_pull<-national_data_pull%>%
      dplyr::mutate(variable = gsub("_", " ", str_to_title(variable)))%>%
      dplyr::rename(Variable = variable) %>%
      dplyr::filter(Variable %in% c("Petrol", "Diesel", "Bottled water", "Treated water"))

    #get number of columns now we will use later in the formatting of the table
    columns_of_data_begin <- ncol(national_data_pull)+1
    percent_col <- time+2                                                       # get the column number of the percent change for future formatting
    col_format_last <- time+1
    columns_of_data_end <- ncol(national_data_pull)                             # get number of columns now we will use later in the formatting of the table
    names(national_data_pull) <- gsub("_", " ", names(national_data_pull))      # get rid of the weird naming from the mutate_at

    #Render the output DT
    #https://stackoverflow.com/questions/60659666/changing-color-for-cells-on-dt-table-in-shiny
    #https://blog.rstudio.com/2015/06/24/dt-an-r-interface-to-the-datatables-library/
    DT::datatable(national_data_pull,extensions = c('FixedColumns'),
                  options = list(searching = F, paging = F, scrollX=T, fixedColumns = list(leftColumns = 1, rightColumns = 0)),
                  rownames = F)

  })

  #### Data Explorer ######################################################################
  
  # For District level data
  table_datasetInput1 <- reactive({
    data %>% dplyr::filter(
      is.null(input$table_district) | District %in% input$table_district,
      date2 >= input$table_date_select[1] & date2 <= input$table_date_select[2]
    ) %>%
      dplyr::select("date2", "Governorate", "District", input$table_show_vars)
  })
  # For KII data
  table_datasetInput2 <- reactive({
    full_data %>% dplyr::filter(
      is.null(input$table_district) | District %in% input$table_district,
      Date >= input$table_date_select[1] & Date <= input$table_date_select[2]
      ) %>%
      dplyr::select(Date, Governorate, District, input$table_show_vars_ki)
    })

  table_datasetInput <- reactive({
    if (input$table_aggregation == "District") {table_datasetInput1()
      # } else {
      # table_datasetInput2()
        }
    })

  output$table <- renderDT({

    DT::datatable(
      table_datasetInput1(),
      extensions = c('ColReorder'),
      options = list(autoWidth = FALSE, dom = 't', paging = FALSE, colReorder= TRUE, fixedHeader = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 3)),
      rownames = FALSE,
      style = 'bootstrap', class = 'table-condensed table-hover table-striped'
    ) %>%
      formatRound(
        4:ncol(table_datasetInput1()),
        digits = 0,
        interval = 3,
        mark = ",",
        dec.mark = getOption("OutDec")
      ) %>%
      formatStyle(names(table_datasetInput1()), "white-space"="nowrap")
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("YE-JMMI-data-download-", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(table_datasetInput(), file, row.names = FALSE, na = "")
    }
  )

  observe({
    input$table_reset
    updatePickerInput(session, "table_district", selected = plot_location_list$District)
    updatePickerInput(session, "table_show_vars", selected = c("date2", "Governorate", "District", "Food_SMEB", "WASH_SMEB"))
    updatePickerInput(session, "table_show_vars_ki", selected = names(full_data)[names(full_data) != "AOR"],) # unselect aor column from full data download
    updateSliderTextInput(session, "table_date_select", selected = c(dates_min, dates_max))
  })

  #### Plot ######################################################################

  plot_district_select <- reactive({
    if (input$plot_by_district_item == "Item") {input$select_byitem_district} else {input$select_bydistrict_district}
  })

  plot_governorate_select <- reactive({
    if (input$plot_by_governorate_item == "Item") {input$select_byitem_governorate} else {input$select_bygovernorate_governorate}
  })

  plot_item_select <- reactive({
    if (input$plot_aggregation == 'Country' | (input$plot_aggregation == 'Governorate' & input$plot_by_governorate_item == 'Item') | (input$plot_aggregation == 'District' & input$plot_by_district_item == 'Item')) {input$select_byitem_item} else if (input$plot_aggregation == 'District' & input$plot_by_district_item == 'District') {input$select_bydistrict_item} else {input$select_bygovernorate_item}
  })

  plot_datasetInput <- reactive({prices_long %>%
      dplyr::filter(
        is.null(plot_item_select()) | Item %in% plot_item_select()
      ) %>%
      execute_if(input$plot_type == 'Line Graph' | input$plot_aggregation != 'Country',
                 dplyr::filter(
                   Date >= input$select_date[1] & Date <= input$select_date[2]
                 )
      ) %>%
      execute_if(input$plot_type == 'Boxplot' & input$plot_aggregation == 'Country',
                 dplyr::filter(
                   Date == input$select_date_boxplot
                 )
      ) %>%
      execute_if(input$plot_aggregation == 'District',    dplyr::filter(is.null(plot_district_select()) | District %in% plot_district_select())) %>%
      execute_if(input$plot_aggregation == 'Governorate', dplyr::select(-District)) %>%
      execute_if(input$plot_aggregation == 'Governorate', group_by(Date, Governorate, Item)) %>%
      execute_if(input$plot_aggregation == 'Governorate', summarise_all(median, na.rm = TRUE)) %>%
      execute_if(input$plot_aggregation == 'Governorate', dplyr::filter(is.null(plot_governorate_select()) | Governorate %in% plot_governorate_select())) %>%
      execute_if(input$plot_aggregation == 'Country'      & input$plot_type == 'Line Graph', dplyr::select(-Governorate, -District)) %>%
      execute_if(input$plot_aggregation == 'Country'      & input$plot_type == 'Line Graph', group_by(Date, Item)) %>%
      execute_if(input$plot_aggregation == 'Country'      & input$plot_type == 'Line Graph', summarise_all(median, na.rm = TRUE)) %>%
      execute_if(input$plot_aggregation == 'District'     & input$select_index == 'TRUE', group_by(Governorate, District, Item)) %>%
      execute_if(input$plot_aggregation == 'Governorate'  & input$select_index == 'TRUE', group_by(Governorate, Item)) %>%
      execute_if(input$plot_aggregation == 'Country'      & input$plot_type == 'Line Graph' & input$select_index == 'TRUE', group_by(Item)) %>%
      execute_if((input$plot_aggregation == 'Country' & input$plot_type == 'Line Graph' | input$plot_aggregation != 'Country') & input$select_index == 'TRUE',
                 dplyr::mutate(Price = round(((Price / c(Price[Date == input$select_date_index], NA)[1])-1)*100, digits = 1))) %>%
      dplyr::filter(!is.na(Price))
  })

  output$plot_text <- renderText({
    if (nrow(plot_datasetInput()) == 0) {
      "There is no data for this selection. Change the time frame or select another indicator/location."} else {""}
  })

  output$graph <- renderHighchart({

    if (input$plot_aggregation == "Country" & input$plot_type == "Boxplot") {

      graph <- hcboxplot(x = plot_datasetInput()$Price, var = plot_datasetInput()$Item, outliers = FALSE) %>%
        hc_chart(type = "column") %>%
        hc_tooltip(valueSuffix = " IQD") %>%
        hc_yAxis(min = 0, title = list(text = "Price (in YER)")) %>%
        hc_exporting(
          enabled = TRUE,
          filename = paste0("IRQ-JPMI-boxplot_export-", Sys.Date()),
          buttons = list(
            contextButton = list(
              menuItems = list("downloadPNG", "downloadPDF", "downloadCSV")
            )),
          sourceWidth = 1000,
          sourceHeight = 600
        ) %>%
        hc_plotOptions(boxplot = list(fillColor = "#e9e9e9",
                                      lineWidth = 1,
                                      lineColor = "#5c5c5c",
                                      medianWidth = 2,
                                      medianColor = "#d9230f",
                                      stemColor = "#5c5c5c",
                                      stemWidth = 1,
                                      whiskerColor = "#5c5c5c",
                                      whiskerLength = "0%",
                                      whiskerWidth = 1
        ),
        series = list(dataSorting = list(enabled = TRUE, sortKey = "median"))
        ) %>%
        hc_tooltip(pointFormat = "Max: {point.high}<br>
                                          Q3:\u00a0\u00a0 {point.q3}<br>
                                          Med: {point.median}<br>
                                          Q1:\u00a0\u00a0 {point.q1}<br>
                                          Min:\u00a0 {point.low}<br>")


    } else if (input$plot_aggregation == "Country" | (input$plot_aggregation == "District" & input$plot_by_district_item == "Item") | (input$plot_aggregation == "Governorate" & input$plot_by_governorate_item == "Item")) {
      graph <- hchart(plot_datasetInput(), "line", hcaes(x = Date, y = Price, group = Item))

    } else if (input$plot_aggregation == "District"){
      graph <- hchart(plot_datasetInput(), "line", hcaes(x = Date, y = Price, group = District))

    } else {
      graph <- hchart(plot_datasetInput(), "line", hcaes(x = Date, y = Price, group = Governorate))
    }
    graph <- graph %>%
      hc_xAxis(title = "") %>%
      hc_colors(cols) %>%
      hc_exporting(
        enabled = TRUE,
        filename = paste0("IRQ-JPMI-linegraph_export-", Sys.Date()),
        buttons = list(contextButton = list(menuItems = list("downloadPNG", "downloadPDF", "downloadCSV"))),
        sourceWidth = 1000,
        sourceHeight = 600
      ) %>%
      execute_if(input$select_index == 'TRUE' & !(input$plot_type == 'Boxplot' & input$plot_aggregation == 'Country'),
                 hc_xAxis(plotLines = list(list(label = list(text = "Ref. month", style = list(fontSize = "11px", color = "dimgrey")),
                                                width = 1, color = "#FF0000", dashStyle = "dash",
                                                value = datetime_to_timestamp(as.Date(input$select_date_index, tz = 'UTC')))))
      ) %>%
      execute_if(input$select_index == 'TRUE' & !(input$plot_type == 'Boxplot' & input$plot_aggregation == 'Country'),
                 hc_yAxis(labels = list(format = "{value}%"), title = list(text = "% deviation from ref. month"),
                          plotLines = list(list(width = 1, color = "#FF0000", dashStyle = "dash", value = 0, zIndex=2)))
      ) %>%
      execute_if(input$select_index == 'TRUE' & !(input$plot_type == 'Boxplot' & input$plot_aggregation == 'Country'),
                 hc_tooltip(valueSuffix = "%")
      ) %>%
      execute_if(input$select_index == 'FALSE',
                 hc_tooltip(valueSuffix = " YER")
      ) %>%
      execute_if(input$select_index == 'FALSE',
                 hc_yAxis(min = 0, title = list(text = "Price (in YER)"))
      )
  })

}


shinyApp(ui, server)

# JMMI Dashboard 
# 16.05.21 
# With food components

# UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI UI

ui <- function(){
  
  #DROP DOWN MENU SELECTIONS 
  vars1 <- c(
    "WASH SMEB"="WASH_SMEB",
    "Food SMEB"="Food_SMEB",
    "Parallel Exchange Rates"="exchange_rates",
    "Wheat Flour" = "wheat_flour",
    "Rice" = "rice",
    "Dry Beans" = "beans_dry",
    "Canned Beans" = "beans_can",
    "Lentils" = "lentil",
    "Vegetable Oil" = "vegetable_oil",
    "Sugar" = "sugar",
    "Salt" = "salt",
    "Potato" = "potato",
    "Onion" = "onion",
    "Petrol" = "petrol",
    "Diesel" = "diesel",
    "Bottled Water"="bottled_water",
    "Treated Water"="treated_water",
    "Soap"="soap",
    "Laundry Powder"="laundry_powder",
    "Sanitary Napkins"="sanitary_napkins",
    "Water Trucking"= "cost_cubic_meter"
  )
  
  
  varsDate<- c("Months to Select" = "varDateSelect")
  
  #USER INTERFACE COMPONENTS 
  navbarPage(theme= shinytheme("journal"), 
             title=strong(HTML("<span style='font-size:30px'>YEMEN: JOINT MARKET MONITORING INITIATIVE</span>")), # id="nav", #MAIN TITLE
             windowTitle = "REACH: Yemen Joint Market Monitoring Initiative (JMMI)", #Title for browser tab window
             
             ###..................................M A P. . P A G E ..........................................
             tabPanel(strong("JMMI"), #TAB LABEL
                      icon= icon("map-marker"), #TAB ICON
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("AdminLTE.css"),
                            shiny::includeCSS( "bootstrap.css"), #added 
                            includeCSS(path = "shinydashboard.css"),
                            br()#added
                          ),
                          
                          #LEAFLET MAP
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map1", width="100%", height="100%"), #BRING IN LEAFLET MAP, object created in server.R
                          tags$head(tags$style(".leaflet-control-zoom { display: none; }
                                              
                                            #controls {height:90vh; overflow-y: auto; }
                                              ")), #remove map zoom controls
                          
                          tags$head(tags$style(
                            type = "text/css",
                            "#controlPanel {background-color: rgba(255,255,255,0.8);}",
                            ".leaflet-top.leaflet-left .leaflet-control {
                           margin-top: 25px;
                         }"
                          )),
                          #https://stackoverflow.com/questions/37861234/adjust-the-height-of-infobox-in-shiny-dashboard
                          
                          
                          #SIDE PANEL
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = 1,
                                        width = 500, height = "auto", 
                                        
                                        hr(),
                                        h5("The Yemen Joint Market Monitoring Initiative (JMMI) is a harmonized price monitoriong initiative that focuses on informing
                                      the Water, Sanitation, and Hygiene (WASH) Cluster and the Cash 
                                      and Market Working Group (CMWG) to support humanitarian activies throughout Yemen.  
                                      The JMMI provides an indicative estimation of the prices of WASH and fuel items across districts in Yemen."),
                                        
                                        h5(tags$u("Most recent findings displayed in map are from data collected in ", #DistsNumn and currentD will change based on the most recent JMMI, defined in global.R
                                                  tags$strong(DistsNumb), "districts in ", tags$strong(paste0(currentD,"."))),
                                           ("The districts outlined in red indicate that data for the selected item was collected in that district in previous months.")),
                                        
                                        h5("Further details regarding the JMMI methodology and the Survival Minimum Expenditure Basket (SMEB) calculation can be found on the information tab. 
                                         For additional information on supply chains and market-related concerns, please visit the  ",a("REACH Resource Center", target="_blank",    href="https://www.reachresourcecentre.info/country/yemen/cycle/754/#cycle-754"), " to access the monthly situation overviews."),
                                        
                                        hr(),
                                        
                                        #h5(tags$u("Most recent findings displayed in map are from data collected in ", #DistsNumn and currentD will change based on the most recent JMMI, defined in global.R
                                        #   tags$strong(DistsNumb), "districts in ", tags$strong(currentD))),
                                        
                                        selectInput("variable1", h4("Select Variable Below"), vars1, selected = "SMEB"), #linked text
                                        
                                        
                                        h5(textOutput("text3")), #extra small text which had to be customized as an html output in server.r (same with text1 and text 2)
                                        
                                        #HIGH CHART
                                        highchartOutput("hcontainer", height= 300, width = 450),
                                        
                                        #new data table 
                                        hr(),
                                        selectInput(inputId= "varDateSelect", label = h4("Select Month of Data Collection"), choices=NULL, selected = (("varDateSelect"))),#linked date stuff
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
             ###..................................I N F O. . P A G E ..........................................
             tabPanel(strong("Information"),
                      tags$head(tags$style("{ height:90vh; overflow-y: scroll; }")),
                      
                      icon= icon("info"), #info-circle
                      div(#class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
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
             ),
             
             tabPanel(strong("SMEB Tracker"),
                      
                      
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
                          h6("Is the percent difference desired for the benchmark"),
                          width=2.5),
                        
                        mainPanel(
                          h2("Monthly SMEB Costs and Percentage Change from Standard SMEB Values"),
                          DT::dataTableOutput("table_smeb"),
                          tags$hr(),
                          h2("Monthly Cost for Other non-SMEB goods"),
                          DT::dataTableOutput("table_other")
                          
                          
                        )
                      )
                      
                      
                      
                      
                      )),           
             #conditionalPanel("false", icon("crosshairs")),
             #)
             tabPanel(strong("Partners"),
                      
                      #style=("{overflow-y:auto; }"), 
                      icon= icon("handshake"), #info-circle
                      div(tags$head(
                        # Include our custom CSS
                        tags$style(".fa-check {color:#008000}"),
                        tags$style(HTML(".sidebar {height:50vh; overflow-y:auto; }"))
                      ),
                      
                      column(width=8,h3("Partners (Past and Present)")), #h1- h5 change the header level of the text
                      column(width=7, h6(tags$i("Check marks indicate that the partner participated in the most recent month’s JMMI"))),
                      
                      #icon("check", "fa-2x")),
                      #list of partners orgs
                      column(width=12,align = "center", h5("Agency for Technical Cooperation and Development (ACTED)",icon("check", "fa-2x")),a(img(src='0_acted.png', height= "50px"), target="_blank", href="https://www.acted.org/en/countries/yemen/")),
                      column(width=12,align = "center", h5("Al Maroof",icon("check", "fa-2x")),a(img(src='0_almaroof.jpg', height= "50px"), target="_blank")),
                      column(width=12,align = "center", h5("Adventist Development and Relief Agency (ADRA)"), a(img(src='0_adra.png', height= "50px"), target = "_blank", href="https://adra.org/")),
                      column(width=12,align = "center", h5("Al Thadamon Association"), img(src='0_thadamon.jpg', height= "50px")),
                      column(width=12,align = "center", h5("Brains for Development (B4D)"),img(src='0_b4d.jpg', height= "50px")),
                      column(width=12,align = "center", h5("Benevolence Coalition for Humanitarian Relief (BCHR)", icon("check", "fa-2x")), a(img(src='0_bchr.jpg', height= "50px"), target="_blank", href="http://bchr-ye.org/")),
                      column(width=12,align = "center", h5("Creative Youth Forum (CYF)"), a(img(src='0_cyf.jpg', height= "50px"),target="_blank", href="https://www.facebook.com/cyf.org77/")),
                      column(width=12,align = "center", h5("Danish Refugee Council (DRC)", icon("check", "fa-2x")), a(img(src='0_drc.png', height= "50px"), target="_blank", href="http://www.drc.dk")),
                      column(width=12,align = "center", h5("Generations without Qat (GWQ),",icon("check", "fa-2x")), img(src='0_gwq.png', height= "50px")),
                      #column(width=12,align = "center", h5("LLMPO"), img(src='0_cyf.png', height= "50px")),
                      column(width=12,align = "center", h5("International Organization for Migration (IOM)",icon("check", "fa-2x")), img(src='0_iom.png', height= "50px"), target="_blank", href="https://www.iom.int/countries/yemen"),
                      column(width=12,align = "center", h5("International Rescure Committee (IRC)", icon("check", "fa-2x")), a(img(src='0_IRC.jpg', height= "50px"), target="_blank", href="https://www.rescue.org/")), 
                      column(width=12,align = "center", h5("Mercy Corps (MC)"), img(src='0_mercy.jfif', height= "50px")),
                      column(width=12,align = "center", h5("National Foundation for Development and Humanitarian Response (NFDHR)"), a(img(src='0_nfdhr.png', height= "50px"),target="_blank", href="http://nfdhr.org/")),
                      column(width=12,align = "center", h5("National Forum Human Development (NFHD)"), img(src='0_nfhd.png', height= "50px")),
                      column(width=12,align = "center", h5("Norweigan Refugee Council (NRC)", icon("check", "fa-2x")), img(src='0_nrc.png', height= "50px")),
                      column(width=12,align = "center", h5("Old City Foundation for Development (OCFD)"), img(src='0_ocfd.jpg', height= "50px")),
                      column(width=12,align = "center", h5("OXFAM", icon("check", "fa-2x")), img(src='0_oxfam.png', height= "50px")),
                      column(width=12,align = "center", h5("Rising Org. for Children Rights Development (ROC)"), a(img(src='0_roc.jpg', height= "50px"), target="_blank", href="https://rocye.org/")),
                      column(width=12,align = "center", h5("Sama Al Yemen", icon("check", "fa-2x")), img(src='0_sama.jpg', height= "50px")),
                      column(width=12,align = "center", h5("Save the Children (SCI)",icon("check", "fa-2x")), img(src='0_sci.png', height= "50px")),
                      column(width=12,align = "center", h5("STEPS", icon("check", "fa-2x")), img(src='0_steps.jpg', height= "50px")),
                      column(width=12,align = "center", h5("Sustainable Development Foundation (SDF)"), img(src='0_sdf.jpg', height= "50px")),
                      column(width=12,align = "center", h5("Solidarites International (SI)", icon("check", "fa-2x")), img(src='0_si.jpeg', height= "50px")),
                      column(width=12,align = "center", h5("Soul Yemen"), img(src='0_soul.jpg', height= "50px")),
                      column(width=12,align = "center", h5("Tamdeen Youth Foundation (TYF)",icon("check", "fa-2x")), img(src='0_tyf.png', height= "50px")),
                      column(width=12,align = "center", h5("Vision Hope"), img(src='0_vision.png', height= "50px")),
                      column(width=12,align = "center", h5("Yemen Family Care Association (YFCA)"), img(src='0_yfca.jpg', height= "50px")),
                      column(width=12,align = "center", h5("Yemen Shoreline Development (YSD)"), img(src='0_ysd.jpg', height= "50px")),
                      column(width=12,align = "center", h5("ZOA Yemen",icon("check", "fa-2x")), img(src='0_zoa.PNG', height= "50px")),
                      p(),
                      hr()
                      
                      
                      ))
             
  )
}

# SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER

server<-function(input, output,session) {
  #_________________________create map consisting of several layers and customization___________________
  output$map1 <- renderLeaflet({  #initiate map
    leaflet(options = leafletOptions(minZoom = 4.5)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% #base map, can be changed
      setView(50.911019,15.889618, zoom = 6.5)
    # setMaxBounds( lng1 = -66.9
    #               , lat1 = 37
    #               , lng2 = -66.1
    #               , lat2 = 37.8 )
  })
  
  #"observe" inputs to define variables for map colors, titles, legends and subset district data
  observe({
    VARIA <- input$variable1
    metacol <- c("admin2pcod", "admin1name", "admin1pcod", "admin2name", "admin2refn")
    metacol2 <- c("num_obs", "date2")
    #YlOrRd #YlGnBu #RdPu #OrRd #Greys #Greens #viridis #magma
    if (VARIA == "WASH_SMEB") {
      dataM<-Rshp[,c(metacol, "WASH_SMEB", metacol2)] #subset WASH SMEB Col
      # dataM<-Rshp[,c(1,5,7,8,10,29,28,30)] #subset exchange rate col
      #mypal<-colorNumeric( palette="RdYlGn", domain=(dataM@data[,6]), na.color = "#9C9D9F", reverse = T)
      mypal<-colorNumeric( palette=(colorRamp(c("#ADFFA5", "#A7383D", "#420A0D"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"WASH SMEB: "
      pLa2<-"WASH SMEB"
      en<-" "
      unitA=" YER"
      title_legend<-"WASH SMEB Cost"
    }
    if (VARIA == "exchange_rates") {
      dataM<-Rshp[,c(metacol, "exchange_rates", metacol2)] #subset exchange rate col
      mypal<-colorNumeric( palette="Greens", domain=dataM@data[,6], na.color = "#D0CFCF",reverse = F)
      pLa<-"Parallel Exchange Rate: "
      pLa2<-"Parrallel Exchange Rate"
      en<-" "
      unitA=" YER"
      title_legend<-"YER to 1 USD"
    }
    if (VARIA == "petrol") {
      dataM<-Rshp[,c(metacol, "petrol", metacol2)] #subset petrol col
      mypal<-colorNumeric( palette="YlOrBr", domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Petrol Price: "
      pLa2<-"Petrol Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (1 L)"
    }
    if (VARIA == "diesel") {
      dataM<-Rshp[,c(metacol, "diesel", metacol2)] #subset diesel col
      #mypal<-colorNumeric( palette="Reds", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#FFD7D9", "#FF535B", "#FB000D", "#830007", "#480004"), interpolate="spline")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Diesel Price: "
      pLa2<-"Diesel Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (1 L)"
    }
    if (VARIA == "bottled_water") {
      dataM<-Rshp[,c(metacol, "bottled_water", metacol2)] #subset bottled water col
      #mypal<-colorNumeric( palette="PuBu", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#C7C0FF", "#7A6AFF", "#1501B9", "#0A005D", "#050033"), interpolate="spline")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Bottled Water Price: "
      pLa2<-"Bottled Water Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (0.75 L)"
    }
    if (VARIA == "treated_water") {
      dataM<-Rshp[,c(metacol, "treated_water", metacol2)] #subset exchange rate col
      mypal<-colorNumeric( palette=(colorRamp(c("#C3FFFD", "#6EFBF6", "#009F99", "#00504D"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      #mypal<-colorNumeric( palette="PuBuGn", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      pLa<-"Treated Water Price: "
      pLa2<-"Treated Water Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (10 L)"
    }
    if (VARIA == "soap") {
      dataM<-Rshp[,c(metacol, "soap", metacol2)] #subset soap col
      mypal<-colorNumeric( palette="RdPu", domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Soap Price: "
      pLa2<-"Soap Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (100 g)"
    }
    if (VARIA == "laundry_powder") {
      dataM<-Rshp[,c(metacol, "laundry_powder", metacol2)] #subset laundry powder col
      mypal<-colorNumeric( palette="Purples", domain=dataM@data[,6], na.color = "#D0CFCF",reverse = F)
      pLa<-"Laundry Powder Price: "
      pLa2<-"Laundry Powder Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (100 g)"
    }
    if (VARIA == "sanitary_napkins") {
      dataM<-Rshp[,c(metacol, "sanitary_napkins", metacol2)] #subset sanitary napkins col
      mypal<-colorNumeric( palette="BuPu", domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Sanitary Napkin Price: "
      pLa2<-"Sanitary Napkin Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (10 Pack)"
    }
    if (VARIA == "cost_cubic_meter") {
      dataM<-Rshp[,c(metacol, "cost_cubic_meter", metacol2)] #subset cost cubic meter water col
      #mypal<-colorNumeric( palette="Blues", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#C9C3F8", "#5D52AD", "#FAD962", "#AA9239"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = T)
      pLa<-"Water Trucking Cost per Cubic Meter: "
      pLa2<-"Water Trucking Cost per Cubic Meter"
      en<-" "
      unitA=" YER"
      title<-"Price (Cubic m)"
      title_legend<-title
      
    }
    ##########FOOD PRICES
    if (VARIA == "wheat_flour") {
      dataM<-Rshp[,c(metacol, "wheat_flour", metacol2)] #subset wheat flour col
      mypal<-colorNumeric( palette=(colorRamp(c("#C3FFFD", "#6EFBF6", "#009F99", "#00504D"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      #mypal<-colorNumeric( palette="PuBuGn", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      pLa<-"Wheat_flour Price: "
      pLa2<-"Wheat_flour Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (1 Kg)"
      
    }
    if (VARIA == "rice") {
      dataM<-Rshp[,c(metacol, "rice", metacol2)] #subset rice col
      mypal<-colorNumeric( palette=(colorRamp(c("#C3FFFD", "#6EFBF6", "#009F99", "#00504D"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      #mypal<-colorNumeric( palette="PuBuGn", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      pLa<-"Rice Price: "
      pLa2<-"Rice Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (1 Kg)"
      
    }
    if (VARIA == "beans_dry") {
      dataM<-Rshp[,c(metacol, "beans_dry", metacol2)] #subset beans dry col
      mypal<-colorNumeric( palette="BuPu", domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Dry beans Price: "
      pLa2<-"Dry beans Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (10 Pack)"
      
    }
    if (VARIA == "beans_can") {
      dataM<-Rshp[,c(metacol, "beans_can", metacol2)] #subset beans can col
      mypal<-colorNumeric( palette="RdPu", domain=dataM@data[,6], na.color = "#D0CFCF", reverse = F)
      pLa<-"Bean cans Price: "
      pLa2<-"Bean cans Price"
      en<-" "
      unitA=" YER"
      title_legend<-"Price (15oz can)"
      
    }
    if (VARIA == "lentils") {
      dataM<-Rshp[,c(metacol, "lentils", metacol2)] #subset lentils col
      #mypal<-colorNumeric( palette="Blues", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#C9C3F8", "#5D52AD", "#FAD962", "#AA9239"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = T)
      pLa<-"Lentils price: "
      pLa2<-"Lentils price"
      en<-" "
      unitA=" YER"
      title<-"Price (1 Kg)"
      title_legend<-title
      
      
    }
    if (VARIA == "vegetable_oil") {
      dataM<-Rshp[,c(metacol, "vegetable_oil", metacol2)] #subset vegetable oil col
      mypal<-colorNumeric( palette="Greens", domain=dataM@data[,6], na.color = "#D0CFCF",reverse = F)
      pLa<-"Vegerabke oil price: "
      pLa2<-"Vegerabke oil price"
      en<-" "
      unitA=" YER"
      title<-"Price (1 L)"
      title_legend<-title
      
    }
    if (VARIA == "sugar") {
      dataM<-Rshp[,c(metacol, "sugar", metacol2)] #subset sugar col
      #mypal<-colorNumeric( palette="Blues", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#C9C3F8", "#5D52AD", "#FAD962", "#AA9239"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = T)
      pLa<-"Sugar price: "
      pLa2<-"Sugar price"
      en<-" "
      unitA=" YER"
      title<-"Price (1 Kg)"
      title_legend<-title
      
    }
    if (VARIA == "salt") {
      dataM<-Rshp[,c(metacol, "salt", metacol2)] #subset salt col
      #mypal<-colorNumeric( palette="Blues", domain=dataM@data[,6], na.color = "#9C9D9F", reverse = F)
      mypal<-colorNumeric( palette=(colorRamp(c("#C9C3F8", "#5D52AD", "#FAD962", "#AA9239"), interpolate="linear")),domain=dataM@data[,6], na.color = "#D0CFCF", reverse = T)
      pLa<-"Salt price: "
      pLa2<-"Salt price"
      en<-" "
      unitA=" YER"
      title<-"Price (1 Kg)"
      title_legend<-title
      
    }
    if (VARIA == "potato") {
      dataM<-Rshp[,c(metacol, "potato", metacol2)] #subset potato col
      mypal<-colorNumeric( palette="Greens", domain=dataM@data[,6], na.color = "#D0CFCF",reverse = F)
      pLa<-"Potato price: "
      pLa2<-"Potato price"
      en<-" "
      unitA=" YER"
      title<-"Price (1 Kg)"
      title_legend<-title
      
    }
    if (VARIA == "onion") {
      dataM<-Rshp[,c(metacol, "onion", metacol2)] #subset onion col
      mypal<-colorNumeric( palette="Greens", domain=dataM@data[,6], na.color = "#D0CFCF",reverse = F)
      pLa<-"Onion price: "
      pLa2<-"Onion price"
      en<-" "
      unitA=" YER"
      title<-"Price (1 Kg)"
      title_legend<-title
      
    }
    if (VARIA == "Food_SMEB") {
      dataM<-Rshp[,c(metacol, "Food_SMEB", metacol2)] #subset Food_SMEB col
      mypal<-colorNumeric( palette="Greens", domain=dataM@data[,6], na.color = "#D0CFCF",reverse = F)
      pLa<-"Food SMEB: "
      pLa2<-"Food SMEB"
      en<-" "
      unitA=" YER"
      title<-"Food SMEB Cost"
      title_legend<-title
      
    }
    
    #Have a vector of all of the districts that currently have no data for the current month
    dataM_NAs<-dataM@data%>%
      filter(is.na(date2))%>%
      pull(admin2pcod)
    
    #get name of variable selected
    call_name<-colnames(dataM@data[6])
    
    #Need to subset out for with districts have had values in the past that arent in this current month, get that as a list
    Admin2data_out <- Admin2table %>% #subset out recent month dates to attach to shapefile
      filter(district_ID %in% dataM_NAs)%>% #next only keep data from places that have had an observation in the past from the right varialbe
      filter(!is.na(get(call_name)))
    
    Admin2data_out <- Admin2data_out[!duplicated(Admin2data_out$district_name),]
    
    Rshp@data<-Rshp@data%>%
      mutate(alt_dist = admin2pcod %in% Admin2data_out$district_ID)
    Rshp@data$alt_dist<-Rshp@data$alt_dist*1
    Rshp@data$alt_dist<-(dplyr::na_if(Rshp@data$alt_dist,0))
    
    #final dataset that needs to be pulled from the right Rshp (this is after you dynamically pull out the right data depending on what is selected)
    old_dist <- Rshp[,c(metacol, "alt_dist")]
    #convert the data list (shapefile list) to a more useable list we can filter from while holding the coordinates https://gis.stackexchange.com/questions/156268/r-filter-shapefiles-with-dplyr/156290
    old_dist_alt<-sf::st_as_sf(old_dist)
    #actually do a filter on the stuff you want (this corresponds to the Rshp 31 and the 1 and NA we did before), basically its a janky way to do a clip on the fly from a GIS perspective
    old_dist_alt<-old_dist_alt%>%filter(alt_dist == 1)
    #convert back to a spatial datalist https://gis.stackexchange.com/questions/239118/r-convert-sf-object-back-to-spatialpolygonsdataframe
    old_dist_alt_sp<-sf::as_Spatial(old_dist_alt)
    #create a new color pallete for the new over lay
    pal_alt<-colorBin( palette="#E5E5E5", domain=old_dist_alt_sp@data[,"alt_dist"])
    
    #leaflet map proxy updates created map to reflect obeserved changes
    map1<-leafletProxy("map1") %>%
      clearShapes() %>% #clear polygons, so new ones can be added
      clearControls()%>% #reset zoom etc
      
      
      addLabelOnlyMarkers(centroids, #ADD governorate LABELS
                          lat=centroids$lat,
                          lng=centroids$lon,
                          label=as.character(centroids$admin1name),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 0.6,
                            style = list(
                              "color"= "black",
                              "font-size" = "13px",
                              "font-family"= "Helvetica",
                              "font-weight"= 600)
                          )) %>%
      addLabelOnlyMarkers(YEMl, lat=YEMl$lat, #Add Yemen label
                          lng=YEMl$lon,
                          label=as.character(YEMl$name),
                          labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            interactive = FALSE,
                            direction = "bottom",
                            textOnly = TRUE,
                            offset = c(0, -10),
                            opacity = 1,
                            style = list(
                              "color"= "black",
                              "font-size" = "24px",
                              "font-family"= "Helvetica",
                              "font-weight"= 800,
                              "letter-spacing"= "3px")
                          )) %>%
      
      addPolygons(data= dataM,    # add subsetted district shapefiles
                  color = "grey",
                  weight = 0.8,
                  label= paste(dataM$admin2name," (", pLa, dataM@data[,6],en, ")"),
                  opacity = 1.0,
                  smoothFactor = 0.8,
                  fill = TRUE,
                  fillColor =  (~mypal((dataM@data[,6]))),#custom palette
                  fillOpacity = .8,
                  layerId = ~admin2pcod,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE, sendToBack = FALSE),
                  popup = paste0(dataM$admin2name, "<br>",'<h7 style="color:black;">',
                                 pLa, "<b>"," ", dataM@data[,6],unitA, "</b>", '</h7>'),
      ) %>%
      addPolygons(data= old_dist_alt_sp,    # this is you clipped data file of previous districts (make sure it is below your main district on or it will not be seen)
                  color = "red",
                  weight = 1.5,
                  label = paste0(old_dist_alt_sp$admin2name,":previous ",pLa2," data present"), #added a different label that pops up
                  opacity = .40,
                  smoothFactor = 0.5,
                  fill = TRUE,
                  fillColor = ~pal_alt(old_dist_alt_sp@data[,"alt_dist"]), #custom palette as stated before
                  fillOpacity = .8,
                  layerId = ~admin2pcod,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE, sendToBack = FALSE),
      )%>%
      addPolylines(data = Admin1, #add governorate lines for reference
                   weight= 3.25,
                   stroke = T,
                   color = "black",
                   fill=FALSE,
                   fillOpacity = 0.1,
                   opacity = 0.1 )
    
    map1 %>% clearControls()
    
    map1 %>%
      addLegend_decreasing("topleft", pal = mypal, values =  dataM@data[,6], #update legend to reflect changes in selected district/variable shown
                           labFormat=labelFormat(suffix=unitA),
                           title = title_legend,
                           opacity = 5,
                           decreasing = T)
    
    #needed to make a custom label because i hate R shiny https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
    colors<-c("white", "#D3D3D3", "#D3D3D3")
    labels<-c("Districts with previous data", "Governorate borders", "District borders")
    sizes<-c("20", "20", "20")
    shapes<-c("square", "line", "line")
    borders<-c("red", "#2B2B2B" , "#646464")
    
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
    
    #add new legend
    map1 %>% addLegendCustom(colors, labels, sizes, shapes, borders)
    
    #add scale bar
    map1 %>% addScaleBar("topleft", options = scaleBarOptions(maxWidth = 100, metric = T, imperial = T, updateWhenIdle = T))
    
    
  })#end of MAP
  
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
    all_dat<-right_join(dist_data(),gov_nat_data(), by = "date2")#using a full join so that the data that was for the other month when district wasnt select is still shown
    all_dat$date<-as.yearmon(all_dat$date2)
    all_dat
  })
  
  
  chartData1<-reactive({  #subset JMMI data table based on variable of interest (soap, water etc)
    metacol<-c("date", "government_name.x",  "government_ID.x", "district_name", "district_ID")
    
    if (input$variable1 == "WASH_SMEB"){
      r<-state_data() %>% select(all_of(metacol), "WASH_SMEB", "date2", contains("WASH_SMEB."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,16,17,31,43,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "exchange_rates"){
      r<-state_data() %>% select(all_of(metacol), "exchange_rates", "date2", contains("exchange_rates."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,14,17,29,41,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "petrol"){
      r<-state_data() %>% select(all_of(metacol), "petrol", "date2", contains("petrol."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,6,17,21,33,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "diesel"){
      r<-state_data() %>% select(all_of(metacol), "diesel", "date2", contains("diesel."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,7,17,22,34,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "bottled_water"){
      r<-state_data() %>% select(all_of(metacol), "bottled_water", "date2", contains("bottled_water."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,8,17,23,35,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "treated_water"){
      r<-state_data() %>% select(all_of(metacol), "treated_water", "date2", contains("treated_water."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,9,17,24,36,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "soap"){
      r<-state_data() %>% select(all_of(metacol), "soap", "date2", contains("soap."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,10,17,25,37,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "laundry_powder"){
      r<-state_data() %>% select(all_of(metacol), "laundry_powder", "date2", contains("laundry_powder."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,11,17,26,38,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "sanitary_napkins"){
      r<-state_data() %>% select(all_of(metacol), "sanitary_napkins", "date2", contains("sanitary_napkins."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,12,17,27,39,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "cost_cubic_meter"){
      r<-state_data() %>% select(all_of(metacol), "cost_cubic_meter", "date2", contains("cost_cubic_meter."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,13,17,28,40,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "wheat_flour"){
      r<-state_data() %>% select(all_of(metacol), "wheat_flour", "date2", contains("wheat_flour."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,14,17,29,41,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
    }
    
    if (input$variable1 == "rice"){
      r<-state_data() %>% select(all_of(metacol), "rice", "date2", contains("rice."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,15,17,30,42,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
    }
    
    if (input$variable1 == "beans_dry"){
      r<-state_data() %>% select(all_of(metacol), "beans_dry", "date2", contains("beans_dry."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,16,17,31,43,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
    }
    
    if (input$variable1 == "beans_can"){
      r<-state_data() %>% select(all_of(metacol), "beans_can", "date2", contains("beans_can."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,17,17,32,44,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
    }
    
    if (input$variable1 == "lentil"){
      r<-state_data() %>% select(all_of(metacol), "lentil", "date2", contains("lentil."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,18,17,33,45,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
    }
    
    if (input$variable1 == "vegetable_oil"){
      r<-state_data() %>% select(all_of(metacol), "vegetable_oil", "date2", contains("vegetable_oil."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,19,17,34,46,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
    }
    
    if (input$variable1 == "sugar"){
      r<-state_data() %>% select(all_of(metacol), "sugar", "date2", contains("sugar."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,20,17,35,47,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
    }
    
    if (input$variable1 == "salt"){
      r<-state_data() %>% select(all_of(metacol), "salt", "date2", contains("salt."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,21,17,36,48,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
      
    }
    
    if (input$variable1 == "potato"){
      r<-state_data() %>% select(all_of(metacol), "potato", "date2", contains("potato."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,22,17,37,49,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
      
    }
    
    if (input$variable1 == "onion"){
      r<-state_data() %>% select(all_of(metacol), "onion", "date2", contains("onion."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,23,17,38,50,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
      
    }
    
    if (input$variable1 == "Food_SMEB"){
      r<-state_data() %>% select(all_of(metacol), "Food_SMEB", "date2", contains("Food_SMEB."), contains("num_obs"))
      colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      # r<-state_data()[,c(1:5,23,17,38,50,15,30,42)]
      # colnames(r)<-c("date","government_name","government_ID","district_name","district_ID","variableSEL","date2","governorate_val","nat_val","dist_obs","gov_obs","nat_obs")
      
      
    }
    
    r
  })
  
  chartNAME<-reactive({ #define element to be used as title for selected variable
    if (input$variable1 == "WASH_SMEB"){
      y="WASH SMEB"
    }
    if (input$variable1 == "exchange_rates"){
      y="Parallel Exchange Rate"
    }
    if (input$variable1 == "petrol"){
      y= "Petrol Price"
    }
    if (input$variable1 == "diesel"){
      y= "Diesel Price"
    }
    if (input$variable1 == "bottled_water"){
      y= "Bottled Water Price"
    }
    if (input$variable1 == "treated_water"){
      y= "Treated Water Price"
    }
    if (input$variable1 == "soap"){
      y="Soap Price"
    }
    if (input$variable1 == "laundry_powder"){
      y= "Laundry Powder Price"
    }
    if (input$variable1 == "sanitary_napkins"){
      y= "Sanitary Napkins Price"
    }
    if (input$variable1 == "cost_cubic_meter"){
      y= "Water Trucking Price"
    }
    if (input$variable1 == "Food_SMEB"){
      y= "Food SMEB price"
    }
    if (input$variable1 == "wheat_flour"){
      y= "Wheat flour Price"
    }
    if (input$variable1 == "rice"){
      y= "Rice Price"
    }
    if (input$variable1 == "dry_bean"){
      y= "Dry beans Price"
    }
    if (input$variable1 == "can_bean"){
      y= "Can beans Price"
    }
    if (input$variable1 == "lentil"){
      y= "Lentils Price"
    }
    if (input$variable1 == "vegetable_oil"){
      y= "Vegetable oil Price"
    }
    if (input$variable1 == "sugar"){
      y= "Sugar Price"
    }
    if (input$variable1 == "salt"){
      y= "Salt Price"
    }
    if (input$variable1 == "potato"){
      y= "Potato Price"
    }
    if (input$variable1 == "onion"){
      y= "Onion Price"
    }
    y
  })
  
  #_________________________Create highcharter element which uses dataset filtered by user inputs___________________
  #NEW OUT PUT FOR DATA TO BE SUBBED LATER
  #https://stackoverflow.com/questions/38113507/r-shiny-keep-retain-values-of-reactive-inputs-after-modifying-selection
  #https://stackoverflow.com/questions/57468457/how-can-i-set-the-yaxis-limits-within-highchart-plot
  
  observe({
    updateSelectInput(session = session, inputId = "varDateSelect", choices = chartData1()$date, selected=lapply(reactiveValuesToList(input), unclass)$varDateSelect)
  })
  
  output$hcontainer <- renderHighchart({
    event <- (input$map1_shape_click) #Critical Line!!!
    
    (validate(need(event$id != "",
                   "Please click on a district to display its history.")))
    
    ChartDat<-chartData1() #define filtered table that is reactive element chartData1
    #y_min<- chartDatMIN()
    #y_max<- chartDatMAX()
    
    chosenD<- paste0(na.omit(unique(ChartDat[,2])),", ",na.omit(unique(ChartDat[,4]))) #TITLE FOR CHART (governorate and district name)
    
    highchart()%>% #high chart
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b %Y')) %>%
      
      #data for national
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, nat_val), color = "dodgerblue", name=paste(chartNAME(),"-National"))%>%
      #data for governorate
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, governorate_val), color = "forestgreen", name=paste(chartNAME(),"-Governorate"))%>%
      #data for district
      hc_add_series(data=ChartDat, type = "line", hcaes(date2, variableSEL), color="#4F4E51", name=paste(chartNAME(),"-District"))%>%
      
      hc_yAxis(tithcle=list(text=paste0(chartNAME()," in YER")), opposite = FALSE
               #,min= as.numeric(y_min), max= as.numeric(y_max)
      )%>%
      hc_title(text=chosenD)%>%
      hc_add_theme(hc_theme_gridlight())%>%
      hc_plotOptions(line = list(
        lineWidth=1.5,
        dataLabels = list(enabled = FALSE)))
    
  })
  #building the table for the observations and prices
  #BUILDING TABLE  https://stackoverflow.com/questions/32149487/controlling-table-width-in-shiny-datatableoutput
  output$out_table_obs<-DT::renderDataTable({
    
    ChartDat_Date<-chartData1()#make a new dataset to play around with from the original state data one above
    
    #https://stackoverflow.com/questions/40152857/how-to-dynamically-populate-dropdown-box-choices-in-shiny-dashboard
    
    ChartDat_Date_filter<-ChartDat_Date%>% #filter out based on what was selected from the varDateSelect
      filter(date == input$varDateSelect)
    
    chosenD_Date<- paste0(na.omit(unique(ChartDat_Date[,2])),", ",na.omit(unique(ChartDat_Date[,4])))#make a quick title for the data table
    
    mat_date_test<-matrix(c(round(ChartDat_Date_filter[6],2),
                            round(ChartDat_Date_filter[8],2),
                            round(ChartDat_Date_filter[9],2),
                            ChartDat_Date_filter[10],
                            ChartDat_Date_filter[11],
                            ChartDat_Date_filter[12]), 
                          nrow=2, ncol=3, byrow = T,
                          dimnames=list(c("Median Price (YER)","Number of Markets Assessed"),c(paste0("District: \n",na.omit(unique(ChartDat_Date[,4]))),paste0("Governorate: \n",na.omit(unique(ChartDat_Date[,2]))),"Yemen")))
    
    DT::datatable(mat_date_test,options = list(dom = 't'))
  })
  
  
  #output infobox for the info exchange rate
  output$info_exchange<-renderValueBox({
    exchange_data<-AdminNatTable
    exchange_data$date<-as.yearmon(exchange_data$date)
    exchange_date<-exchange_data%>%
      filter(date == input$varDateSelect)
    exchange_rate<-exchange_date[1,10]
    
    valueBox(
      value = exchange_rate,
      subtitle="YER to 1 USD", 
      icon = icon("dollar"),
      #fill=T, 
      color = "green")
    
  })
  
  output$text1 <- renderUI({ #customised text elements
    HTML(paste("Coordinate System:  WGS 1984",
               "Administrative boundaries:  OCHA",
               "R Mapping Packages:  leaflet, shiny, highcharter",
               "Please do NOT use Microsoft Edge for best user interaction",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 12px;
         font-family: Helvetica} </style>')
  })
  #
  output$text2 <- renderUI({
    HTML(paste("<i>Note: Data displayed on this map should be interpreted as indicative. 
               In addition, designations and boundaries used here do not imply acceptance 
               by REACH partners and associated donors.</i>",
               sep="<br/>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;
         font-family: Helvetica} </style>')
  })
  
  output$text3 <- renderText({ #LARGE TEXT ABOVE CHART
    paste(chartNAME(), " ", "Medians Over Time")
  })
  
  output$text_DT<-renderText({
    paste0(chartNAME(),", Median Monthly Costs, and Number of Markets Assessed")
  })
  
  output$text4 <- renderUI({
    HTML(paste("<i>For more information, please visit our",a("REACH Website", target="_blank", href="https://www.reach-initiative.org"), 
               "or contact us directly at yemen@reach-initiative.org.</i>"), '<style type="text/css"> .shiny-html-output { font-size: 11px; line-height: 11px;
         font-family: Helvetica} </style>')
  })
  
  #FRoM ONLINE https://github.com/ua-snap/shiny-apps/blob/master/cc4liteFinal/server.R
  
  #######################
  #SMEB tracker
  #######################
  #Goal - build system that can adapt to X# of months back and benchmark at YY%
  #Data needed - national but may need to clip to districts of each individual one based on methodology
  #So pull number of district from most recent month and clip back based on X# of months.
  #then build a table function that allows for that stuff to be adapted
  
  #build the dataset
  
  #SMEB DATASET  
  output$table_smeb<-DT::renderDataTable({
    #observe({
    #https://stackoverflow.com/questions/50912519/select-the-number-of-rows-to-display-in-a-datatable-based-on-a-slider-input
    time<-input$months
    percent_time<- input$percent/100
    
    
    #time<-6
    national_data_test<-nat_data()
    national_data_test<-AdminNatTable
    national_data_test$date2 <- as.yearmon(national_data_test$date)
    national_data<-arrange(national_data_test,desc(date2))
    
    month_all<-sort(unique(national_data$date2),decreasing = T)
    time_pull<-month_all[time]
    month_list<-month_all[1:match(time_pull,month_all)]
    
    #now have the month_list which we can cut from in the future
    
    national_data_pull<-dplyr::filter(national_data, date2==month_list)%>%
      dplyr::select(-c(date,num_obs,exchange_rates))
    
    
    national_data_pull<-national_data_pull%>%
      reshape2::melt("date2")%>%
      reshape2::dcast(variable ~ date2)%>%
      round_df(.,0) %>% select(variable, as.character(month_list))
    
    col_data_pull<-ncol(national_data_pull)
    
    name_perc_change<-paste0(colnames(national_data_pull[col_data_pull]),
                             " percent change from standard SMEB")
    #Add SMEB base costs
    #https://stackoverflow.com/questions/13502601/add-insert-a-column-between-two-columns-in-a-data-frame
    national_data_pull<-national_data_pull%>%
      # add_column(.,  `Standard SMEB Values`= c(365,430,100,120,130,105,525,1825,12000,42000), .after = 1)%>%
      # add_column(.,  `Variable`= c("Petrol","Diesel","Bottled water","Treated water","Soap","Laundry powder","Sanitary napkins","Water trucking","SMEB total"), .after = 1)%>%
      mutate(Variable=gsub("_"," ",str_to_title(variable)), .before=2) %>%
      dplyr::select(c(-1))%>%
      # dplyr::filter(Variable %in% c("Soap","Laundry powder","Sanitary napkins","Water trucking","Wash smeb", "Food smeb")) %>%
      mutate(Variable=gsub("smeb", "SMEB", Variable))
    
    #get number of columns now we will use later in the formatting of the table
    columns_of_data_begin<-ncol(national_data_pull)+1
    
    #get the column number of the percent change for future formatting
    percent_col<-time+2
    col_format_last<-time+1
    
    
    # This is where the percentage change are calculated. It used to be calculated compared to some "standard" SMEB value, which is crap.
    # Should either compute percentage change between t and t-n for each n
    #https://duckduckgo.com/?q=dynamic+naming+in+mutate+R&t=brave&ia=web
    national_data_pull<-national_data_pull%>%
      # dplyr::mutate_at(., .vars = c(3:ncol(.)),.funs = list(`percent change from standard SMEB`= ~((.-(national_data_pull[,2]))/(national_data_pull[,2]))))
    dplyr::mutate_at(., .vars = c(3:ncol(.)),
                     .funs = list('Percentage_change' = ~(((national_data_pull[,2])-.)/(.))),
                     .names = "{fn}_{col}")
    
    #get number of columns now we will use later in the formatting of the table 
    columns_of_data_end<-ncol(national_data_pull)
    
    #get rid of the weird naming from the mutate_at
    names(national_data_pull) <- gsub("_", " ", names(national_data_pull))
    
    #maybe keep for later
    #dplyr::mutate(.,!!name_perc_change := (((.[,col_data_pull]-.[,2])/(.[,2]))))
    
    #Render the output DT
    #https://stackoverflow.com/questions/60659666/changing-color-for-cells-on-dt-table-in-shiny
    #https://blog.rstudio.com/2015/06/24/dt-an-r-interface-to-the-datatables-library/
    DT::datatable(national_data_pull,extensions = c('FixedColumns'), 
                  options = list(searching = F, paging = F, scrollX=T, fixedColumns = list(leftColumns = 2, rightColumns = 0)),
                  rownames = F)%>%
      formatStyle(columns = 1, color = "white", backgroundColor = "grey", fontWeight = "bold")%>%
      DT::formatPercentage(columns = c(columns_of_data_begin:columns_of_data_end),2)%>%
      formatStyle(columns = c(columns_of_data_begin:columns_of_data_end),
                  color = styleInterval(c(-percent_time,percent_time), c('grey', 'black','white')),
                  backgroundColor = styleInterval(c(-percent_time,percent_time), c('#66FF66', 'white','#FA5353')),
                  fontWeight = styleInterval(c(-percent_time,percent_time),c('bold','normal','bold')))
    
    
  })
  
  
  #Other goods dataset  
  output$table_other<-DT::renderDataTable({
    #observe({
    #https://stackoverflow.com/questions/50912519/select-the-number-of-rows-to-display-in-a-datatable-based-on-a-slider-input
    time<-input$months
    percent_time<- input$percent/100
    
    
    #time<-6
    national_data_test<-nat_data()
    national_data_test<-AdminNatTable
    national_data_test$date2 <- as.yearmon(national_data_test$date)
    national_data<-arrange(national_data_test,desc(date2))
    
    month_all<-sort(unique(national_data$date2),decreasing = T)
    time_pull<-month_all[time]
    month_list<-month_all[1:match(time_pull,month_all)]
    
    #now have the month_list which we can cut from in the future
    national_data_pull<-dplyr::filter(national_data, date2%in%month_list)%>%
      dplyr::select(-c(date,num_obs,exchange_rates))
    
    # national_data_pull<-national_data_pull%>%
    #   reshape2::melt("date2")
    # national_data_pull<-national_data_pull%>%
    #   mutate(value=round(as.numeric(value)))%>%
    #   group_by(variable)%>%
    #   mutate(change=scales::percent(value/lag(value, order_by = date2)-1, accuracy=1),
    #          change2=scales::percent(value/lag(value, n = 2, order_by = date2)-1, accuracy=1))%>%
    #   mutate(change  = ifelse(!grepl('^\\-', change) & change != "0%" & !is.na(change), paste0("+", change, HTML(" &#9650;")), change),
    #          change  = ifelse(grepl('^\\-', change), paste0(change, HTML(" &#9660;")), change),
    #          change  = ifelse(change == "0%", paste0(change, HTML(" &#9654;")), change),
    #          change2 = ifelse(!grepl('^\\-', change2) & change2 != "0%" & !is.na(change2), paste0("+", change2, HTML(" &#9650;")), change2),
    #          change2 = ifelse(grepl('^\\-', change2), paste0(change2, HTML(" &#9660;")), change2),
    #          change2 = ifelse(change2 == "0%", paste0(change2, HTML(" &#9654;")), change2))

    national_data_pull<-national_data_pull%>%
      reshape2::melt("date2")%>%
      reshape2::dcast(variable ~ date2)%>%
      round_df(.,0) %>% select(variable, as.character(month_list))
    
    col_data_pull<-ncol(national_data_pull)
    
    name_perc_change<-paste0(colnames(national_data_pull[col_data_pull]),
                             " percent change from standard SMEB")
    #Add SMEB base costs
    #https://stackoverflow.com/questions/13502601/add-insert-a-column-between-two-columns-in-a-data-frame
    national_data_pull<-national_data_pull%>%
      #add_column(.,  `Standard SMEB Values`= c(365,430,100,120,130,105,525,1825,12000), .after = 1)%>%
      # add_column(.,  `Variable`= c("Petrol","Diesel","Bottled water","Treated water","Soap","Laundry powder","Sanitary napkins","Water trucking","SMEB total"), .after = 1)%>%
      mutate(Variable=gsub("_"," ",str_to_title(variable)), .before=1)%>%
      # mutate(Variable=lapply(variable,function(x) gsub("_"," ", paste0(toupper(substring(x,1,1)), substring(x,2,nchar(as.character(x)))))))%>%
      dplyr::select(c(-variable))%>%
      dplyr::filter(Variable %in% c("Petrol","Diesel","Bottled water","Treated water"))
    
    #get number of columns now we will use later in the formatting of the table
    columns_of_data_begin<-ncol(national_data_pull)+1
    
    #get the column number of the percent change for future formatting
    percent_col<-time+2
    col_format_last<-time+1
    
    
    
    #get number of columns now we will use later in the formatting of the table 
    columns_of_data_end<-ncol(national_data_pull)
    
    #get rid of the weird naming from the mutate_at
    names(national_data_pull) <- gsub("_", " ", names(national_data_pull))
    
    #maybe keep for later
    #dplyr::mutate(.,!!name_perc_change := (((.[,col_data_pull]-.[,2])/(.[,2]))))
    
    #Render the output DT
    #https://stackoverflow.com/questions/60659666/changing-color-for-cells-on-dt-table-in-shiny
    #https://blog.rstudio.com/2015/06/24/dt-an-r-interface-to-the-datatables-library/
    DT::datatable(national_data_pull,extensions = c('FixedColumns'), 
                  options = list(searching = F, paging = F, scrollX=T, fixedColumns = list(leftColumns = 1, rightColumns = 0)),
                  rownames = F)
    
  })
  
  
}


shinyApp(ui, server)

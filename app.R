
# library -----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(geojsonio)
library(openxlsx)
library(tidyverse)
library(googledrive)
library(plotly)
library(stringdist)
library(fuzzyjoin)
library(leaflet)
library(htmltools)
library(shinyWidgets)
library(sf)
library(ggridges)
library(DT)
library(leaflet.extras2)


# data import --------------------------------------------------------------


# # ffrs municity data from drive
# data1.Loc <- "https://drive.google.com/file/d/1i3rFBE3dboiCNKV7rXCI6WbxVQNlz3__/view?usp=drive_link"
# 
# #download from drive using googledrive
# drive_download(data1.Loc, overwrite = TRUE)

#import data and labels to r using openxlsx
data1 <-  readRDS("FFRS_mapdata_summaries.rds")


# map data ----------------------------------------------------------------

basemaps <- list()


basemaps[["region"]] <-  geojson_read("./phl-admin-psgc-master/phl_regions.geojson", what="sp") %>% 
  st_as_sf()

basemaps[["province"]] <-  geojson_read("./phl-admin-psgc-master/phl_provinces_ncr-districts_icc.geojson", what="sp") %>% 
  st_as_sf()

basemaps[["municity"]] <-  geojson_read("./phl-admin-psgc-master/phl_municities.geojson", what="sp") %>% 
  st_as_sf()

# ui ----------------------------------------------------------------------

ui <- 
  navbarPage(
    "FFRS map",
    theme = shinytheme("flatly"),
    tabPanel("Interactive Map",
             div(class="outer",
                 tags$head(includeCSS("styles.css")),
                 
                leafletOutput("map1", width="100%",height ="100%"),
                 
                absolutePanel(top = 60, right = 20, width = 450,
                               draggable = TRUE,
                               style = "opacity: 0.8",
                              tags$h3("Controls"),
                              #mapgroup
                              radioGroupButtons(
                                inputId = "mapgroup",
                                label = "Map grouping:",
                                choices = c("Regional","Provincial", "Municipality/City")
                              ),
                              #Region
                              selectizeInput(inputId = 'reg',
                                             label = 'Region:',
                                             choices =c("Select Region",levels(data1$region$parcel_address_reg))
                              ),
                              #Province
                              selectizeInput(inputId = 'prov',
                                             label = 'Province:',
                                             choices =c("Select Province",unique(data1$province$parcel_address_prv)),
                                             selected = ""),
                              
                              #eco
                              selectizeInput(inputId = 'eco',
                                             label = 'Ecosystem:',
                                             choices =c("Select ecosystem:",
                                                        "All ecosystem",
                                                        "Irrigated",
                                                        "Rainfed Lowland",
                                                        "Rainfed Upland",
                                                        "Undefined"),
                                             selected = "All ecosystem")
                )
             )
    
    ),
  tabPanel("Data",
           dataTableOutput("table1")
           ),
  tabPanel("Static maps",
           tags$h3("Download"),
           
           actionBttn(inputId= "download",
                      label = "Municipal maps",
                      color="primary",
                      style="unite",
                      icon = icon("download"),
                      onclick = "window.open('https://drive.google.com/uc?export=download&id=1OAjcXdgQTCyo0MFLRWJDyTWalSGEfZBj')")
  )
  
  )

# server ------------------------------------------------------------------

server = function (input, output, session) {
  

  
  #location list
  
  loclist <-  data1$brgy %>% 
    ungroup() %>% 
    distinct(parcel_address_reg,parcel_address_prv,parcel_address_mun, parcel_address_brgy)
  
  #location dropdown filtering
  
  reg.filtered <-  reactive ({
    if(input$reg== "Select Region"){
      loclist
    } else {
      loclist %>% 
        filter(parcel_address_reg==input$reg)
    }
  })
  
  prov.filtered <-  reactive ({
    if(str_detect(input$prov,"Select")){
      reg.filtered()
    } else {
      reg.filtered()%>% 
        filter(parcel_address_prv == input$prov)
    }
  })
  
  provchoices <-reactive ({
    unique(reg.filtered()$parcel_address_prv)
  })
  
  observe({
    updateSelectizeInput(inputId='prov',
                         label = "Province:",
                         choices = c("Select Province",provchoices())
    )
  })
  
  

  
  #data based on map grouping
  
  mapdata1 <-  reactive ({
    switch(input$mapgroup,
           "Regional" = data1$region,
           "Provincial"=data1$province, 
           "Municipality/City"=data1$municity)
  })
  
  
  
  
  
  #data filtering
  
  mapdata2 <-  reactive ({
    if(input$reg== "Select Region"){
      mapdata1()
    } else {
      mapdata1() %>% 
        filter(parcel_address_reg==input$reg)
    }
  })
  
  
  
  
  
  mapdata3 <-  reactive ({
    if(input$prov== "Select Province"){
      mapdata2()
    } else {
      mapdata2() %>% 
        filter(parcel_address_prv == input$prov)
    }
  })
  
  
  

  
  mapdata5 <- reactive ({
    switch(input$mapgroup,
           "Regional" = mapdata2() %>% 
             rename("Region"=parcel_address_reg) %>% 
             arrange(Region,farm_type),
           "Provincial"=mapdata3() %>% 
             rename("Region"=parcel_address_reg,
                    "Province"=parcel_address_prv) %>% 
             arrange(Region,Province,farm_type), 
           "Municipality/City"=mapdata3() %>% 
             rename("Region"=parcel_address_reg,
                    "Province"=parcel_address_prv,
                    "MuniCity"=parcel_address_mun) %>% 
             arrange(Region,Province,MuniCity,farm_type)
    )
  })
  
  mapdata6 <- reactive({
    if (input$eco=="Select ecosystem:"){
      mapdata5()
    } else {
      mapdata5() %>% 
        filter(farm_type == input$eco)
    }
  })
  

  
  
  #output: table 1
  
  output$table1 <- renderDataTable({
    datatable(mapdata6() %>%
                rename("Ecosystem"=farm_type,
                       "Number of parcels"=parcelno,
                       "Number of farmers"=farmerno,
                       "Sum of area" =area),
              filter = 'top',
              extensions = "Buttons",
              options = list(paging = TRUE,
                             lengthMenu = c(10,25,50,100),
                             scrollX=TRUE, 
                             searching = TRUE,
                             ordering = TRUE,
                             dom = 'Bfrtip',
                             buttons = c('download','copy','csv','excel'))
    )
  })
  
  # 
  # ridgedata0 <-  reactive ({
  #   switch(input$mapgroup,
  #          "Regional" = data1$province,
  #          "Provincial"=data1$municity, 
  #          "Municipality/City"=data1$brgy)
  # })
  # 
  # ridgedata1 <-  reactive({
  #   if(input$reg=="Select Region"){
  #     ridgedata0()
  #   } else {
  #     ridgedata0()%>% 
  #       filter(parcel_address_reg == input$reg)
  #   }
  # })
  # 
  # ridgedata2 <-  reactive({
  #   if(input$prov=="Select Province"){
  #     ridgedata1()
  #   } else {
  #     ridgedata1() %>% 
  #       filter(parcel_address_prv == input$prov)
  #   }
  # })
  # 
  # ridgedata3 <-  reactive({
  #   if(input$city=="Select City"){
  #     ridgedata2()
  #   } else {
  #     ridgedata2() %>% 
  #       filter(parcel_address_mun == input$city)
  #   }
  # })
  # 
  # ridgedata4 <-  reactive({
  #   if(input$city=="Select ecosystem"){
  #     ridgedata3()
  #   } else {
  #     ridgedata3() %>% 
  #       filter(farm_type == input$eco)
  #   }
  # })
  # 
  # 
  # 
  # 
  # ridgedata6 <- reactive ({
  #   switch(input$mapgroup,
  #          "Regional" = ridgedata0() %>% 
  #            filter(farm_type == input$eco) %>% 
  #            rename("Region"=parcel_address_reg) %>% 
  #            mutate(var = !!sym(mapvar())) %>% 
  #            arrange(Region,farm_type),
  #          "Provincial"=ridgedata1() %>%
  #            filter(farm_type == input$eco) %>% 
  #            rename("Region"=parcel_address_reg,
  #                   "Province"=parcel_address_prv) %>% 
  #            mutate(var = !!sym(mapvar())) %>% 
  #            arrange(Region,Province,farm_type), 
  #          "Municipality/City"=ridgedata2() %>% 
  #            filter(farm_type == input$eco) %>% 
  #            rename("Region"=parcel_address_reg,
  #                   "Province"=parcel_address_prv,
  #                   "MuniCity"=parcel_address_mun) %>%  
  #            mutate(var = !!sym(mapvar())) %>% 
  #            arrange(Region,Province,MuniCity,farm_type)
  #   )
  # })
  # 
  # 
  # 
  # output$ridge1 <- 
  #   renderPlot({
  #     switch(input$mapgroup,
  #            "Regional"=ridgedata6() %>% 
  #              ggplot(aes(x=var,y=Region, fill=Region))+
  #              geom_density_ridges()+
  #              geom_vline(xintercept = median(ridgedata6()$var, na.rm=TRUE), col = "black", lty = "dashed")+
  #              scale_y_discrete(limits=rev)+
  #              scale_x_binned(n.breaks =5)+
  #              theme_minimal(base_family = "Fira Sans")+
  #              guides(fill="none"),
  #            "Provincial"=ridgedata6() %>% 
  #              ggplot(aes(x=var,y=Province, fill=Province))+
  #              geom_density_ridges()+
  #              geom_vline(xintercept = median(ridgedata6()$var, na.rm=TRUE), col = "black", lty = "dashed")+
  #              scale_y_discrete(limits=rev)+
  #              scale_x_binned(n.breaks =5)+
  #              theme_minimal(base_family = "Fira Sans")+
  #              guides(fill="none"),
  #            "Municipality/City"=ridgedata6() %>% 
  #              ggplot(aes(x=var,y=MuniCity, fill=MuniCity))+
  #              geom_density_ridges()+
  #              geom_vline(xintercept = median(ridgedata6()$var, na.rm=TRUE), col = "black", lty = "dashed")+
  #              scale_y_discrete(limits=rev)+
  #              scale_x_binned(n.breaks =5)+
  #              theme_minimal(base_family = "Fira Sans")+
  #              guides(fill="none")
  #     )
  #   })
  # 
  #joining with shpfiles
  
  ##basemap
  map0 <- reactive ({
    switch(input$mapgroup,
           "Regional" = basemaps[["region"]]%>% 
             left_join(mapdata6(),
                       by=c("psgc_region"="psgc.reg")
             ) %>% 
             mutate(areag = case_when(area<50000~"Less than 50,000 ha",
                                      area<=150000~"50,000 to 150,000 ha",
                                      area<=300000~"150,000 to 300,000 ha",
                                      area>300000~"Greater than 300,000 ha"
             ),
             farmerg = case_when(farmerno<50000~"Less than 50,000 farmers",
                                 farmerno<=150000~"50,000 to 150,000 farmers",
                                 farmerno<=300000~"150,000 to 300,000 farmers",
                                 farmerno>300000~"Greater than 300,000 farmers"
             ),
             parcelg = case_when(parcelno<50000~"Less than 50,000 parcels",
                                 parcelno<=150000~"50,000 to 150,000 parcels",
                                 parcelno<=300000~"150,000 to 300,000 parcels",
                                 parcelno>300000~"Greater than 300,000 parcels"
             )
             ),
           
           "Provincial"=basemaps[["province"]] %>% 
             left_join(mapdata6(),
                       by = c("psgc_province"="psgc.prov")) %>% 
             mutate(areag = case_when(area<10000~"Less than 10,000 ha",
                                      area<=20000~"10,000 to 20,000 ha",
                                      area<=50000~"20,000 to 50,000 ha",
                                      area>50000~"Greater than 50,000 ha"
             ),
             farmerg = case_when(farmerno<10000~"Less than 10,000 farmers",
                                 farmerno<=20000~"10,000 to 20,000 farmers",
                                 farmerno<=50000~"20,000 to 50,000 farmers",
                                 farmerno>50000~"Greater than 50,000 farmers"
             ),
             parcelg = case_when(parcelno<10000~"Less than 10,000 parcels",
                                 parcelno<=20000~"10,000 to 20,000 parcels",
                                 parcelno<=50000~"20,000 to 50,000 parcels",
                                 parcelno>50000~"Greater than 50,000 parcels"
             )
             ), 
           
           "Municipality/City"=basemaps[["municity"]]%>%
             left_join(mapdata6(),
                       by=c("psgc_municity"="psgc.mun")
             )%>%
             mutate(areag = case_when(area<100~"Less than 100 ha",
                                      area<=500~"100 to 500 ha",
                                      area<=1000~"500 to 1,000 ha",
                                      area<=5000~"1,000 to 5,000 ha",
                                      area>5000~"Greater than 5,000 ha"
             ),
             farmerg = case_when(farmerno<100~"Less than 100 farmers",
                                 farmerno<=500~"100 to 500 farmers",
                                 farmerno<=1000~"500 to 1,000 farmers",
                                 farmerno<=5000~"1,000 to 5,000 farmers",
                                 farmerno>5000~"Greater than 5,000 farmers"
             ),
             parcelg = case_when(parcelno<100~"Less than 100 parcelno",
                                 parcelno<=500~"100 to 500 parcelno",
                                 parcelno<=1000~"500 to 1,000 parcelno",
                                 parcelno<=5000~"1,000 to 5,000 parcelno",
                                 parcelno>5000~"Greater than 5,000 parcelno"
             )
             )  
           
    )
  })
  
  
  
  output$map1 <- 
    renderLeaflet({
      leaflet() %>%
        addMapPane("left", zIndex = 0) %>%
        addMapPane("right", zIndex = 0) %>%
        addTiles(layerId = 'base',
                 options = pathOptions(pane="right")) %>%
        addTiles(layerId = 'base2',
                 options = pathOptions(pane="left")) %>% 
        setView(lat = 12.8797, lng = 121.774, zoom=6)
    })

  
  ##reactive map elements
  
  # mybins <- c(0,1000,3000,5000,7000,10000,15000,20000,Inf)
  # 
  # binpal <- reactive({
  #   colorBin( palette="RdYlGn",
  #             domain=MapData()$mapvar, 
  #             na.color="transparent", 
  #             bins=mybins,
  #             reverse = T)
  # })
  
  
  map.labels1 <- reactive({
    switch(input$mapgroup,
           "Regional" =
             paste("<b> Region: </b> ",str_to_upper(map0()$Region), "<br>",
                   "<b> Total crop area </b>",format(map0()$area, big.mark=",")) %>%
             lapply(htmltools::HTML),
           "Provincial"=
             paste("<b> Region: </b> ",str_to_upper(map0()$Region), "<br>",
                   "<b> Province: </b> ",str_to_upper(map0()$Province), "<br>",
                   "<b> Total crop area </b>",format(map0()$area, big.mark=",")) %>%
             lapply(htmltools::HTML),
           "Municipality/City"=paste("<b> Region: </b> ",str_to_upper(map0()$Region), "<br>",
                                     "<b> Province: </b> ",str_to_upper(map0()$Province), "<br>",
                                     "<b> MuniCity: </b> ",str_to_upper(map0()$MuniCity), "<br>",
                                     "<b> Total crop area </b>",format(map0()$area, big.mark=",")) %>%
             lapply(htmltools::HTML))
  })
  
  map.labels2 <- reactive({
    switch(input$mapgroup,
           "Regional" =
             paste("<b> Region: </b> ",str_to_upper(map0()$Region), "<br>",
                   "<b> Number of farmers </b>",format(map0()$farmerno, big.mark=",")) %>%
             lapply(htmltools::HTML),
           "Provincial"=
             paste("<b> Region: </b> ",str_to_upper(map0()$Region), "<br>",
                   "<b> Province: </b> ",str_to_upper(map0()$Province), "<br>",
                   "<b> Number of farmers </b>",format(map0()$farmerno, big.mark=",")) %>%
             lapply(htmltools::HTML),
           "Municipality/City"=paste("<b> Region: </b> ",str_to_upper(map0()$Region), "<br>",
                                     "<b> Province: </b> ",str_to_upper(map0()$Province), "<br>",
                                     "<b> MuniCity: </b> ",str_to_upper(map0()$MuniCity), "<br>",
                                     "<b> Number of farmers </b>",format(map0()$farmerno, big.mark=","))  %>%
             lapply(htmltools::HTML))
  })
  
  map.legendtitle <- reactive({
    paste("<b>", input$var1,"</b> ","<br>",
          "<b>",input$eco," </b>") %>%
      lapply(htmltools::HTML)
  })
  
  #   
  numpal <-  reactive({
    colorNumeric(palette ="RdYlGn",
                 domain = map0()$var,
                 na.color = "transparent",
                 alpha=0.5)
  })
  
  
  binPal <- reactive({
    switch(input$mapgroup,
           "Regional"=colorBin(palette = c("firebrick4","#E69F00","#F0E442","#117733","#56B4E9"),
                               domain = map0()$area,
                               bins = c(0, 50000,150000,300000,450000,Inf),
                               na.color = "transparent",
                               alpha=0.5),
           "Provincial"=colorBin(palette = c("firebrick4","#E69F00","#F0E442","#117733","#56B4E9"),
                                 bins = c(0, 10000,20000,40000,60000,Inf),
                                 domain = map0()$area,
                                 na.color = "transparent",
                                 alpha=0.5),
           "Municipality/City"=colorBin(palette = c("firebrick4","#E69F00","#F0E442","#117733","#56B4E9"),
                                        bins = c(0, 100,500,1000,5000,Inf),
                                        domain=map0()$area,
                                        na.color = "transparent",
                                        alpha=0.5)
    )
  })
  
  legendlabels <- reactive ({
    switch(input$mapgroup,
           "Regional" = c("Less than 50,000",
                          "50,000 to 150,000",
                          "150,000 to 300,000",
                          "300,000 to 450,000",
                          "Greater than 450,000"),
           "Provincial"= c("Less than 10,000",
                           "10,000 to 20,000",
                           "20,000 to 40,000",
                           "40,000 to 60,000",
                           "Greater than 60,000"),
           "Municipality/City"= c("Less than 100",
                                  "100 to 500",
                                  "500 to 1,000",
                                  "1,000 to 5,000",
                                  "Greater than 5,000"))
  })
  
  
  
  observe({
    
    labelc <- input$mapgroup
    leg.label <-  legendlabels()
    npal <-  numpal()
    bpal <-  binPal()
    data <- map0()
    
    # legendtitle <- map.legend.title()
    
    leafletProxy("map1",data=data) %>%
      clearControls() %>% 
      clearShapes() %>% 
      addPolygons(layerId = ~area,
                  stroke=TRUE,
                  fillOpacity = 0.9,
                  fillColor = ~bpal(area),
                  color='white',
                  weight=0.5,
                  label = map.labels1(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                  ),
                  group="area",
                  options = pathOptions(pane = "right")
      ) %>%
      addPolygons(layerId = ~farmerno,
                  stroke=TRUE,
                  fillOpacity = 0.9,
                  fillColor = ~bpal(farmerno),
                  color='white',
                  weight=0.5,
                  label = map.labels2(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                  ),
                  group="farmer",
                  options = pathOptions(pane = "left")
      ) %>% 
      addLayersControl(overlayGroups = c("area","farmer")) %>%
      addSidebyside(layerId = "sidecontrols",
                    rightId = "base",
                    leftId = "base2") %>% 
      addLegend( pal=bpal,
                 values=~area,
                 opacity=0.9,
                 labels = leg.label,
                 title = 'Total crop area',
                 position = "bottomright" ) %>% 
      addLegend( pal=bpal,
                 values=~farmerno,
                 opacity=0.9,
                 labels = leg.label,
                 title = 'Number of farmers',
                 position = "bottomleft" )
  })


}


# run server --------------------------------------------------------------

shinyApp(ui, server)

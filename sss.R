data1$region

basemaps$region

leaflet) %>% 
  addTiles() %>% 
  setView(lat = 12.8797, lng = 121.774, zoom=6)



basemaps[["region"]] %>% 
  left_join(maodata %>% 
              filter(farm_type == "All ecosystem"),
            by = c("psgc_region" = "psgc.reg"))
maodata <- basemaps$region %>% 
  left_join(,
            by= c("psgc_region"="psgc.reg"))

bpal <- colorBin(palette = c("firebrick4","#E69F00","#F0E442","#117733","#56B4E9"),
                 bins = c(0, 50000,150000,300000,450000,Inf),
                 domain = maodata$area,
                 na.color = "transparent",
                 alpha=0.5)


numpal <-  reactive({
  colorNumeric(palette ="RdYlGn",
               domain = maodata$area,
               na.color = "transparent",
               alpha=0.5)
})


leaflet(maodata) %>% 
  addTiles() %>% 
  addPolygons( stroke=TRUE,
               fillOpacity = 0.9,
               fillColor=~bpal(area),
              dashArray = NULL,
              label = ~region,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
              )
  )

leaflet(maodata) %>% 
  addTiles() %>% 
  addPolygons(layerId =~area,
              stroke=TRUE,
              fillOpacity = 0.9,
              fillColor = ~bpal(area),
              color='white',
              weight=0.5,
              label = ~region,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
              )
  )



names(data1)

for (i in names(data1)) {
  data1[[i]] %>% 
    ungroup() %>% 
    pivot_wider(names_from = farm_type,
                values_from = c(parcelno,farmerno,area),
                names_glue = "{farm_type}_{.value}") %>% 
    write_csv(paste0(i,"_v2.csv"))
}



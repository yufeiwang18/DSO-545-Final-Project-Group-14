library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggmap)

# Files we need to run this:
# Census-Tracts,Council-Districts,CRIME, CALLS



crime=readRDS("CRIME")
calls=readRDS("CALLS")



#council=read.csv("tracttotothers.csv") %>% select("censusTract","CD")


ui=fluidPage(
  titlePanel("Geo-Spatial Visulization",windowTitle="Map"),
  sidebarPanel(
    textInput(inputId = "target_zone", label = "Search Bar" , ""),
    
    selectInput(inputId = "select", label = "Select the Measure to Show", 
                choices = list("Crime"="num_crime" , "Shelter"="num_shelt","311 Calls"="num_call","Homeless Count"="num_homeless")),
    
    conditionalPanel(
      condition = "input.select == 'num_crime'",
      dateRangeInput(inputId = 'DateRange_Crime',
                     label = 'Date range for crime data: yyyy-mm-dd',
                     start = min(crime$DateOccurred), end = max(crime$DateOccurred)
      )
    ),
    
    conditionalPanel(
      condition = "input.select == 'num_call'",
      dateRangeInput(inputId = 'DateRange_Call',
                     label = 'Date range for 311 calls: yyyy-mm-dd',
                     start = min(calls$CreatedDate), end = max(calls$CreatedDate)
      ),
      radioButtons("call_var", label = "Choose a Variable",
                   choices = list("Number of Calls" = "num_call", "Response Time" = "response")
      )
    ),
    conditionalPanel(
      condition = "input.select == 'num_homeless'",
      radioButtons("homeless_year", label = "Choose a Year",
                   choices = list("2015", "2016","2017")
      ),
      radioButtons("homeless_var", label = "Choose a Variable",
                   choices = list("Homeless Count" = "num_homeless", "Unsheltered Rate" = "unshelt_rate")
      )
    ),
    
    selectInput(inputId = "mapping",
                label = "Choose the Mapping Level",
                choices = list("Census Tract","Council District"),
                selected="Census Tract"
    ),
    checkboxInput("city", label = "Narrow down to LA City", value = TRUE)
  ),
  mainPanel(
    leafletOutput(outputId = "map")
  )
)


server = function(input,output) {
  
  
  View=reactive({
    if(input$target_zone==""){
      return(c(9,34.16,-118.2450))
    }else{
      target_pos=geocode(input$target_zone)
      return(c(12,target_pos$lat,target_pos$lon))
    }
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.BlackAndWhite")
    
  })
  
  shape= reactive({
    if (input$mapping=="Census Tract"){   #Census Tract Level
      shape=readRDS(file="Census-Tracts")
      b=crime%>%
        filter(DateOccurred<=input$DateRange_Crime[2]&DateOccurred>=input$DateRange_Crime[1])%>%
        group_by(CT10)%>%
        summarize(num_crime=n())
      c=calls%>%
        filter(CreatedDate<=input$DateRange_Call[2]&CreatedDate>=input$DateRange_Call[1])%>%
        group_by(CensusTract)%>%
        summarize(num_call=n(),response=as.numeric(mean(ResponsePeriod,na.rm=TRUE)))
      shape=merge(shape,b,by.x="ct10",by.y="CT10")
      shape@data$num_crime[is.na(shape@data$num_crime)]=0
      shape=merge(shape,c,by.x="ct10",by.y="CensusTract")
      shape@data$num_call[is.na(shape@data$num_call)]=0
      shape@data$response[is.na(shape@data$response)]=0
    }else{
      shape=readRDS(file="Council-Districts")
      b=crime%>%
        filter(DateOccurred<=input$DateRange_Crime[2]&DateOccurred>=input$DateRange_Crime[1])%>%
        group_by(CD)%>%
        summarize(num_crime=n())
      shape=merge(shape,b,by.x="district",by.y="CD")
      shape@data$num_crime[is.na(shape@data$num_crime)]=0
      
      c=calls%>%
        filter(CreatedDate<=input$DateRange_Call[2]&CreatedDate>=input$DateRange_Call[1])%>%
        group_by(CD)%>%
        summarize(num_call=n(),response=as.numeric(mean(ResponsePeriod,na.rm=TRUE)))
      shape=merge(shape,c,by.x="district",by.y="CD")
      shape@data$num_call[is.na(shape@data$num_call)]=0
      shape@data$response[is.na(shape@data$response)]=0
      return(shape)
    }
    
    if (input$city==TRUE){
      shape=shape[shape$CD != 0,]
    }
    return(shape)
  })
  
  labels= reactive({
    if (input$mapping=="Census Tract"){   #Census Tract Level
      labels=
        sprintf(
          "<strong>Tract Number: </strong>%s<br/>
          <strong>Number of Shelters: </strong>%g<br/>
          <strong>Number of Crimes: </strong>%g<br/>
          <strong>Number of 311 Calls: </strong>%g<br/>
          <strong>Average Response Time: </strong>%g
          <strong>Total count of Homeless: </strong>%g<br/>
          <strong>Rate of Unshelteredness : </strong>%g",
          shape()$ct10, shape()$num_shelt, shape()$num_crime,
          shape()$num_call, shape()$response,
          shape()[[paste("num_homeless",input$homeless_year,sep="_")]],
          shape()[[paste("rate",input$homeless_year,sep="_")]]) %>% 
        lapply(htmltools::HTML)
      return(labels)
    }else{
      labels=
        sprintf(
          "<strong>Council District Number: </strong>%g<br/>
          <strong>Number of Shelters: </strong>%g<br/>
          <strong>Number of Crimes: </strong>%g<br/>
          <strong>Number of 311 Calls: </strong>%g<br/>
          <strong>Average Response Time: </strong>%g<br/>
          <strong>Total count of Homeless: </strong>%g<br/>
          <strong>Rate of Unshelteredness : </strong>%g",
          shape()$district, shape()$num_shelt, shape()$num_crime,
          shape()$num_call, shape()$response,
          shape()[[paste("num_homeless",input$homeless_year,sep="_")]],
          shape()[[paste("rate",input$homeless_year,sep="_")]])   %>% 
        lapply(htmltools::HTML)
      return(labels)
    }
  })
  
  Data <- reactive({
    if(input$select=="num_call"){
      return(shape()[[input$call_var]])
    }else if(input$select=="num_homeless"){
      if(input$homeless_var=="num_homeless"){
        return(shape()[[paste("num_homeless",input$homeless_year,sep="_")]])
      }else{
        return(shape()[[paste("rate",input$homeless_year,sep="_")]])
      }
    }else{
      return(shape()[[input$select]])
    }
  })
  
  pal= reactive({
    
      colorBin("YlOrRd",domain = Data(),bins =4)
    
  })
  
  observe({
    
    leafletProxy("map") %>%
      
      clearShapes() %>%
      
      addPolygons(data=shape(),weight=1,col = 'black',
                  fillColor = ~pal()(Data()),
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      fillOpacity = .03,bringToFront = TRUE),
                  label = labels(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
    
  })
  
  observe({
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(pal = pal(),values =Data() , opacity = 0.7, title =NULL,
                position = "bottomright")
  })
  
  observe({
    leafletProxy("map") %>%
      setView(lng = View()[3],lat = View()[2],zoom = View()[1])
  })
  
  
  
}

shinyApp(ui,server)
# Check PIT datasets. CD s are not probably assigned correctly.
# change to extend to LA county so the default is LA city



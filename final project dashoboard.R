library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(ggmap)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

load("calls311.RData")
load("crime.Rdata")
load("homeless_count.Rdata")

ui <- dashboardPage(
  dashboardHeader(title = "LA Homeless Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description Analysis", tabName = "Description_app"),
      menuItem("Regression Analysis", tabName = "Regression_app"),
      menuItem("GeoSpatial Analysis", tabName = "GeoSpatial_app")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Description_app",
              {
                
                titlePanel("Descriptive Analysis",
                           windowTitle = "Descriptive Analysis")
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "select",
                                label = "Choose the Dataset",
                                choices = list("311 Calls","Crime","Homeless Count"),
                                selected="311 Calls"
                    ),
                    textInput(inputId = "tract", label = "Enter Tract Number (Leave it empty for having the whole data)" , ""),
                    textInput(inputId = "CD", label = "Enter Council District Number (Leave it empty for having the whole data)" , ""),
                    #############################    #############################    #############################
                    conditionalPanel(
                      condition = "input.select == 'Crime'",
                      radioButtons(inputId = "year",
                                   label = "Choose a Year",
                                   choices = list("2017",
                                                  "2016",
                                                  "2015"),
                                   selected = "2017"),
                      selectInput(inputId = "crime_var",
                                  label = "Choose a Variable",
                                  choices = list("Month",
                                                 "Week",
                                                 "Age",
                                                 "Race"),
                                  selected = "month")
                    ),
                    #############################    #############################    #############################
                    conditionalPanel(
                      condition = "input.select == '311 Calls'",
                      radioButtons(inputId = "year_obs",
                                   label = "Choose a Year: ",
                                   choices = list("2017" = 2017,
                                                  "2016" = 2016,
                                                  "2015" = 2015),
                                   selected = "2017"),
                      selectInput(inputId = "category",
                                  label = "Source and Time Analysis",
                                  choices = list("Request by Source" = "a_req",
                                                 "Efficiency by Source" = "b_req",
                                                 "Total Request by Month" = "c_req",
                                                 "Total Request by Weekday" = "d_req",
                                                 "Request Source by Month" = "e_req",
                                                 "Request Source by Weekday" = "f_req"),
                                  selected = "Request by Source")
                      
                      
                    )
                    
                  ),
                  
                  mainPanel(
                    plotOutput(outputId = "plot")
                  )
                  
                )
              }
      ),
      tabItem(tabName = "Regression_app",
              {
                titlePanel("Basic Variable Regression",
                           windowTitle = "Regression")
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("Create regression lines with data from the census information"),
                    
                    selectInput(inputId = "var1",
                                label = "Choose a variable to display",
                                choices = list ("count_311",
                                                "count_crime",
                                                "homeless_2017",
                                                "count_shelter",
                                                "count_population"),
                                selected = "count_311"),
                    selectInput(inputId = "var2",
                                label = "Choose a variable to display",
                                choices = list ("count_311",
                                                "count_crime",
                                                "homeless_2017",
                                                "count_shelter",
                                                "count_population"),
                                selected = "count_shelter")
                    
                  ),
                  mainPanel(
                    # textOutput(outputId = "selected_var")
                    plotOutput(outputId = "plot_reg"),
                    textOutput(outputId = "text")
                  )
                )
              }),
      tabItem(tabName = "GeoSpatial_app",
              {
                titlePanel("Geo-Spatial Visulization",windowTitle="Geo-Spatial Visulization")
                sidebarLayout(
                sidebarPanel(
                  textInput(inputId = "target_zone", label = "Search For a Place" , ""),
                  
                  selectInput(inputId = "mapping",
                              label = "Choose the Mapping Level",
                              choices = list("Census Tract","Council District"),
                              selected="Census Tract"
                  ),
                  
                  checkboxInput("county", label = "Show the Whole LA County", value = FALSE),
                  
                  selectInput(inputId = "select1", label = "Select the Measure to Show", 
                              choices = list("Crime"="num_crime" , "Shelter"="num_shelt","311 Calls"="num_call","Homeless Count"="num_homeless","Combined Measure")),
                  
                  conditionalPanel(
                    condition = "input.select1 == 'num_shelt'",
                    radioButtons("shelt_var", label = "Choose a Variable",
                                 choices = list("Absolute Number of Shelters" = "num_shelt_abs", "Number of shelters/Number of Homeless")
                    )
                  ),    
                  conditionalPanel(
                    condition = "input.select1 == 'num_crime'",
                    dateRangeInput(inputId = 'DateRange_Crime',
                                   label = 'Date range for crime data: yyyy-mm-dd',
                                   start = min(crime$DateOccurred), end = max(crime$DateOccurred)
                    ),
                    radioButtons("crime_var", label = "Choose a Variable",
                                 choices = list("Absolute Number of Crimes" = "num_crime_abs", "Number of Crimes/Number of Homeless")
                    )
                  ),
                  
                  
                  conditionalPanel(
                    condition = "input.select1 == 'num_call'",
                    dateRangeInput(inputId = 'DateRange_Call',
                                   label = 'Date range for 311 calls: yyyy-mm-dd',
                                   start = min(calls$CreatedDate), end = max(calls$CreatedDate)
                    ),
                    radioButtons("call_var", label = "Choose a Variable",
                                 choices = list("Absolute Number of Calls" = "num_call_abs","Number of Calls/Number of Homeless" = "num_call_normal", "Average Response Time" = "response")
                                 
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.select1 == 'num_homeless'",
                    radioButtons("homeless_year", label = "Choose a Year",
                                 choices = list("2017", "2016","2015")
                    ),
                    radioButtons("homeless_var", label = "Choose a Variable",
                                 choices = list("Homeless Count" = "num_homeless","Number of Homeless/Population"="num_homeless_normal","Unsheltered Rate" = "unshelt_rate")
                    )
                  ),
                  
                  conditionalPanel(
                    condition="input.select1 == 'Combined Measure'",
                    radioButtons("measure_year", label = "Choose a Year",
                                 choices = list("2017", "2016","2015")
                    ),
                    helpText("Choose the Weights For Different Variables"),
                    numericInput(inputId="crime_w", label="Crime Weight", value=1,min = 0,max=100),
                    numericInput(inputId="shelt_w", label="Shelter Weight", value=1,min = 0,max=100),
                    numericInput(inputId="homeless_w", label="Homeless Count Weight", value=1,min = 0,max=100),
                    numericInput(inputId="call_w", label="311 Calls Weight", value=1,min = 0,max=100)
                  )
                  
                ),
                mainPanel(
                  leafletOutput(outputId = "map",height = 340),
                  plotOutput(outputId = "bar",height = 140)
                  
                ))
              })
    )
  )
)

server <- function(input,output,session){
  ################################# description dashboard #################################
  
  tract=reactive({
    as.numeric(input$tract)
  })
  cd=reactive({
    as.numeric(input$CD)
  })
  
  output$plot<- renderPlot({
    
    if(input$select=="311 Calls"){
      
      whichData = reactive ({
        a= calls %>%
          filter(years == as.numeric(input$year_obs))
        
        if(!is.na(tract())&tract()%in%calls$CensusTract ){
          a = a %>%
            filter(CensusTract==tract())
        }else if(!is.na(cd()) & 0<cd() & cd()<16){
          a = a %>%
            filter(CD==cd())
        }
        return(a)
      })
      
      if(input$category == "a_req"){
        # Request by Source
        ggplot(data = whichData() %>%
                 group_by(RequestSource) %>%
                 summarise(count = n()) %>%
                 arrange(-count), 
               aes(x=reorder(RequestSource,-count),y=count)) +
          geom_bar(stat="identity",fill = "darkred")+
          theme(axis.text.x = element_text(size= 16,angle = 30, hjust = 1),
                axis.text.y = element_text(size = 16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          xlab("")+
          ylab("Number of Requests")+
          ggtitle("Total Requests By source")+
          geom_text(aes(label = count),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold") 
      }else if(input$category == "b_req"){
        # Efficiency by Source
        ggplot(data = whichData() %>%
                 group_by(RequestSource)%>%
                 summarise(mean=round(mean(UpdateRate),2)),
               aes(x=reorder(RequestSource,-mean),y=mean))+
          geom_bar(stat="identity",fill = "darkred")+
          ggtitle("Request Efficiency by Request Source") +
          theme(axis.text.x = element_text(size = 16,angle = 30, hjust = 1),
                axis.text.y = element_text(size = 16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          xlab("Reqeust Source")+
          ylab("Update Duration (Hours)")+
          geom_text(aes(label = round(mean,0)),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
      }else if(input$category == "c_req"){
        # Total Request by Month 
        ggplot(data = whichData(), aes(x = factor(MonthCreated))) +
          geom_bar(fill = "darkred") +
          ylab("Number of Requests") +
          xlab("") +
          ggtitle("Total Requests by Month") +
          theme_classic() +
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          geom_text(stat='count',aes(label = ..count..),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
        
        
      }else if(input$category == "d_req"){
        # Total Request by Weekday
        ggplot(data = whichData(),aes(x = factor(WeekdayCreated))) +
          geom_bar(fill = "darkred") +
          ylab("Number of Requests") +
          xlab("") +
          ggtitle("Total Requests by Weekday") +
          theme_classic() +
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          geom_text(stat='count',aes(label = ..count..),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
        
        
      }else if(input$category == "e_req"){
        # Request Source by Month
        request_cbm = whichData() %>%
          group_by(RequestSource, MonthCreated) %>%
          summarise(count = n())
        
        ggplot(request_cbm, aes(x = reorder(RequestSource, -count), 
                                y = factor(MonthCreated), 
                                fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "mistyrose", high = "deeppink3", 
                              breaks = NULL, labels = NULL) +
          theme_classic() +
          ylab("") +
          xlab("") +
          ggtitle("Request Source by Month") +
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))
        
      }else if(input$category == "f_req"){
        # Request Source by Weekday
        request_cbwh = whichData() %>%
          group_by(RequestSource, WeekdayCreated) %>%
          summarise(count = n())
        
        ggplot(request_cbwh, aes(x = reorder(RequestSource,-count), 
                                 y = factor(WeekdayCreated), 
                                 fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "mistyrose", high = "deeppink3", 
                              breaks = NULL, labels = NULL) +
          theme_classic() +
          ylab("") +
          xlab("") +
          ggtitle("Request Source by Weekday") +
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))
      }  
      
        
    }else if(input$select=="Crime"){
      
      ###First plot
      data1=crime %>%
        filter(year == as.numeric(input$year))
      
      if(!is.na(tract())&tract()%in%crime$CT10 ){
        data1 = data1 %>%
          filter(CT10==tract())
      }else if(!is.na(cd()) & 0<cd() & cd()<16){
        data1= data1 %>%
          filter(CD==cd())
      }
      
      month=ggplot(data1,aes(x=month(DateOccurred,label=T,abbr=T)))+
        geom_bar(fill="darkred")+
        xlab("")+
        ylab("Count")+
        ggtitle("Number of Crimes by Month")+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
              axis.text.y = element_text(size = 16),
              axis.title = element_text(size=16),
              title = element_text(size=17, face='bold', hjust = 0.5),
              panel.background = element_rect(fill="grey"),
              panel.grid = element_line(size = .1))+
        geom_text(stat='count',aes(label = ..count..),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
      
      
      
      #second plot
      
      week=ggplot(data1,aes(x=wday(DateOccurred,label=T,abbr=T)))+
        geom_bar(fill="darkred")+
        xlab("")+
        ylab("Count")+
        ggtitle("Number of Crimes by Weekday")+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
              axis.text.y = element_text(size = 16),
              axis.title = element_text(size=16),
              title = element_text(size=17, face='bold', hjust = 0.5),
              panel.background = element_rect(fill="grey"),
              panel.grid = element_line(size = .1))+
        geom_text(stat='count',aes(label = ..count..),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
      
      
      # data
      
      data3=data1 %>%
        filter(!is.na(VictimAge))%>%
        filter(!is.na(VictimSex))%>%
        filter(!VictimSex=="X")
      
      # data1$VictimSex=as.factor(data1$VictimSex)
      # levels(data1$VictimSex)=c("Female","Male")
      
      ##third plot
      age=ggplot(data3,aes(x=VictimAge))+
        geom_histogram(binwidth=10,fill="darkred",color="black")+
        scale_x_continuous(breaks=seq(0,100,by=10))+
        labs(x="Victim Age",title="Male and Female Victims Age Distribution",y="Count")+
        facet_wrap(~VictimSex)+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
              axis.text.y = element_text(size = 16),
              axis.title = element_text(size=16),
              title = element_text(size=17, face='bold', hjust = 0.5),
              strip.text = element_text(size=16),
              panel.background = element_rect(fill="grey"),
              panel.grid = element_line(size = .1))
      
      #data
      data4=data1 %>%
        group_by(VictimDescent)%>%
        summarise(count=n())%>%
        arrange(count)
      
      # fourth plot
      decent=ggplot(data4,aes(x=reorder(factor(VictimDescent),-count),y=count))+
        geom_bar(stat="identity",fill="darkred")+
        ylab("Count")+
        xlab("Race")+
        ggtitle("Number of Crime by Race")+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
              axis.text.y = element_text(size = 16),
              axis.title = element_text(size=16),
              title = element_text(size=17, face='bold', hjust = 0.5),
              panel.background = element_rect(fill="grey"),
              panel.grid = element_line(size = .1))+
        geom_text(aes(label = count),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
      
      
      showgraph = switch(input$crime_var, 
                         "Month" = month,
                         "Week" = week, 
                         "Age" = age,
                         "Race" = decent)
      showgraph
      
    }else{
      
      data1=homeless_count
      
      if(!is.na(tract())&tract()%in%homeless_count$Tract ){
        data1 = data1 %>%
          filter(Tract==tract())
      }else if(!is.na(cd()) & 0<cd() & cd()<16){
        data1= data1 %>%
          filter(CD==cd())
      } 
      
      
      data1 %>%
        group_by(year)%>%
        summarise(Unsheltered=sum(Unsheltered),Sheltered=sum(Sheltered))%>%
        gather(2:3,key = "condition",value = "count") %>%
        ggplot(aes(x=as.factor(year),y=count,fill=condition,label=count))+
        geom_col()+
        labs(x="",y="Count",title="Homeless Count by Year")+
        geom_text(size = 9,color="white" ,position = position_stack(vjust = 0.5),fontface = "bold")+
        scale_fill_manual(name="",values=c("Darkblue","darkred"))+
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
              axis.text.y = element_text(size = 16),
              axis.title = element_text(size=16),
              title = element_text(size=17, face='bold', hjust = 0.5),
              strip.text = element_text(size=16),
              panel.background = element_rect(fill="grey"),
              panel.grid = element_line(size = .1))
      
      
    }
    
  })   
  
  ################################# regression dashboard #################################
  output$plot_reg = renderPlot({
    variable=reactive({
      data_reg = read.csv("unit_data_for_regression.csv")
      
    })    
    
    choice1 = switch(input$var1, 
                     "count_311" = variable()$count_311,
                     "count_crime" = variable()$count_crime, 
                     "homeless_2017" = variable()$homeless_2017,
                     "count_shelter" = variable()$count_shelter,
                     "count_population" = variable()$count_population
    )
    
    choice2 = switch(input$var2, 
                     "count_311" = variable()$count_311,
                     "count_crime" = variable()$count_crime, 
                     "homeless_2017" = variable()$homeless_2017,
                     "count_shelter" = variable()$count_shelter,
                     "count_population" = variable()$count_population
    )
    
    ggplot(variable(), aes(x = choice1, y = choice2))+ 
      xlab(input$var1)+
      ylab(input$var2)+
      geom_point()+
      geom_smooth(method="lm",color="red",linetype=2)+
      ggtitle("Regression between key variables and confidence interval")+
      theme_wsj()+theme(axis.text = element_text(size = 13),
                        axis.title = element_text(size = 14),
                        title=element_text(size=13))
    #theme(legend.position='none',axis.ticks.length=unit(0.0001,'cm'))
    
    
  })
  
  output$text = renderPrint({
    
    variable=reactive({
      data_reg = read.csv("unit_data_for_regression.csv")
      
    })    
    
    choice1 = switch(input$var1, 
                     "count_311" = variable()$count_311,
                     "count_crime" = variable()$count_crime, 
                     "homeless_2017" = variable()$homeless_2017,
                     "count_shelter" = variable()$count_shelter,
                     "count_population" = variable()$count_population
    )
    
    choice2 = switch(input$var2, 
                     "count_311" = variable()$count_311,
                     "count_crime" = variable()$count_crime, 
                     "homeless_2017" = variable()$homeless_2017,
                     "count_shelter" = variable()$count_shelter,
                     "count_population" = variable()$count_population
    )
    
    
    lmp <- function (modelobject) {
      if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
      f <- summary(modelobject)$fstatistic
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(p) <- NULL
      return(p)
    }
    
    mod1 = lm(formula = choice1 ~ choice2, data = variable())
    print("P-value:")
    lmp(mod1)
    
    
  })
  
  ################################# geo dashboard #################################
  View=reactive({
    if(input$target_zone==""){
      return(c(9,34,-118.3650))
    }else{
      target_pos=geocode(input$target_zone)
      return(c(13,target_pos$lat,target_pos$lon))
    }
  })

  output$map <- renderLeaflet({

    leaflet() %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite")%>%
      setView(lng = View()[3],lat = View()[2],zoom = View()[1])%>%
      clearControls() %>%
      addLegend(pal = pal(),values =Data() , opacity = 0.7, title =NULL,
                position = "bottomright")
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
        summarize(num_call_abs=n(),response=as.numeric(mean(ResponsePeriod,na.rm=TRUE)))
      shape=merge(shape,b,by.x="ct10",by.y="CT10")
      shape@data$num_crime[is.na(shape@data$num_crime)]=0
      shape=merge(shape,c,by.x="ct10",by.y="CensusTract")
      shape@data$num_call_abs[is.na(shape@data$num_call_abs)]=0
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
        summarize(num_call_abs=n(),response=as.numeric(mean(ResponsePeriod,na.rm=TRUE)))
      shape=merge(shape,c,by.x="district",by.y="CD")
      shape@data$num_call_abs[is.na(shape@data$num_call_abs)]=0
      shape@data$response[is.na(shape@data$response)]=0
      return(shape)
    }

    if (input$county==FALSE){
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
          <strong>Average Response Time: </strong>%g<br/>
          <strong>Total count of Homeless: </strong>%g<br/>
          <strong>Rate of Unshelteredness : </strong>%g",
          shape()$ct10, shape()$num_shelt, shape()$num_crime,
          shape()$num_call_abs, shape()$response,
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
          shape()$num_call_abs, shape()$response,
          shape()[[paste("num_homeless",input$homeless_year,sep="_")]],
          shape()[[paste("rate",input$homeless_year,sep="_")]])   %>%
        lapply(htmltools::HTML)
      return(labels)
    }
  })

  ################################## Selected DATA  #################################
  observe({
    if(input$select=="Combined Measure"){
      updateDateRangeInput(session,inputId = "DateRange_Call",start=paste(input$measure_year,"-01-01",sep=""),end=paste(input$measure_year,"-12-31",sep=""))
      updateDateRangeInput(session,inputId = "DateRange_Crime",start=paste(input$measure_year,"-01-01",sep=""),end=paste(input$measure_year,"-12-31",sep=""))
    }
  })


  Data <- reactive({
    if(input$select1=="num_call"){
      if(input$call_var=="num_call_normal"){
        a=shape()[["num_call_abs"]]/shape()[["num_homeless_2017"]]
        a[which(a==Inf)]=(shape()[["num_call_abs"]][which(a==Inf)]+1)/(shape()[["num_homeless_2017"]][which(a==Inf)]+1)  # To avoid Infinity
        return(a)
      }else{
        return(shape()[[input$call_var]])
      }
    }else if(input$select1=="num_homeless"){
      if(input$homeless_var=="num_homeless"){
        return(shape()[[paste("num_homeless",input$homeless_year,sep="_")]])
      }else if(input$homeless_var=="unshelt_rate"){
        return(shape()[[paste("rate",input$homeless_year,sep="_")]])
      }else{
        a=shape()[[paste("num_homeless",input$homeless_year,sep="_")]]/shape()[["Pop"]]
        a[which(a==Inf)]=(shape()[[paste("num_homeless",input$homeless_year,sep="_")]]+1)/(shape()[["Pop"]][which(a==Inf)]+1)
        return(a)
      }

    }else if(input$select1=="num_crime"){
      if(input$crime_var=="num_crime_abs"){
        return(shape()[["num_crime"]])
      }else{
        a=shape()[["num_crime"]]/shape()[["num_homeless_2017"]]
        a[which(a==Inf)]=(shape()[["num_crime"]][which(a==Inf)]+1)/(shape()[["num_homeless_2017"]][which(a==Inf)]+1)
        return(a)
      }

    }else if(input$select1=="num_shelt"){
      if(input$shelt_var=="num_shelt_abs"){
        return(shape()[["num_shelt"]])
      }else{
        a=shape()[["num_shelt"]]/shape()[["num_homeless_2017"]]
        a[which(a==Inf)]=(shape()[["num_shelt"]][which(a==Inf)]+1)/(shape()[["num_homeless_2017"]][which(a==Inf)]+1)
        return(a)
      }
    }else{
      a=input$shelt_w*(1-shape()[["num_shelt"]]/sum(shape()[["num_shelt"]])) +
        input$crime_w*shape()[["num_crime"]]/sum(shape()[["num_crime"]]) +
        input$call_w*shape()[["num_call_abs"]]/sum(shape()[["num_call_abs"]]) +
        input$homeless_w*shape()[[paste("num_homeless",input$measure_year,sep="_")]]/sum(shape()[[paste("num_homeless",input$measure_year,sep="_")]])
      return(a)
    }
  })


  pal= reactive({

    colorBin("YlOrRd",domain = Data(),bins =4)

  })

  observe({

    leafletProxy("map") %>%

      clearShapes() %>%

      addPolygons(data=shape(),weight=.5,col = 'black',
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

  plot_data=reactive({
    if(input$mapping=="Census Tract"){
      return(cbind(id=shape()$ct10,var=Data())%>%
               as.data.frame()%>%
               filter(!is.na(var))%>%
               arrange(-var)%>%
               slice(1:15))
    }else{
      return(cbind(id=shape()$district,var=Data())%>%
               as.data.frame()%>%
               filter(!is.na(var))%>%
               arrange(-var)%>%
               slice(1:5))
    }
  })

  output$bar=renderPlot({
    plot_data() %>%
      ggplot(aes(x=reorder(id,-var),y=var,label=round(var,digits = 2)))+
      geom_col(fill="darkred")+
      labs(x="Area Number",y="Selected Measure",title="Top Areas based on the Selected Measure")+
      geom_text(size = 4,color="white" ,position = position_stack(vjust = .5),fontface = "bold")+
      theme(panel.background = element_rect(fill="grey"),
            panel.grid = element_line(size = .1))
  })
  
}

shinyApp(ui,server)
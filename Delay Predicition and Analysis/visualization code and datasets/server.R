#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    concatPath=c(input$var,".csv");
    path=paste(concatPath,collapse ='');
    path=gsub(" ","_",path)
    myData=fread(path); 
    if(input$origin!="All"){
    new_data = filter(myData,myData$origin==input$origin);
    
    if(path=="By_Day_of_Week.csv"){
    plot_ly(x =new_data$DayOfWeek,y= new_data$numberOfFlightsDelayed,type ="bar")%>%
        layout(
               title = "Delay Analysis by Day of week",
               xaxis = list(title = "Day of Week"),
               yaxis = list(title = "Number of flights delayed"));
                            
    }
    else if(path=="By_Month_of_Year.csv"){
      new_data_2006=filter(new_data,new_data$year=="2006");
      new_data_2007=filter(new_data,new_data$year=="2007");
      plot_ly(new_data, x = new_data_2006$month, y = new_data_2006$number_of_delayed_flights, type = 'bar', name = '2006') %>%
        add_trace(y = new_data_2007$number_of_delayed_flights, name = '2007') %>%
        layout(title = "Delay Analysis by Month of Year",
               xaxis = list(title = "Month"),
               yaxis = list(title = 'Number of Delayed flights'), barmode = 'group')
    }
    else if(path=="By_Hour_of_Day.csv"){
      plot_ly(x =new_data$timeOfDay,y= new_data$numberOfDelayedFlights,type ="scatter" ,mode="line")%>%
        layout(title = "Delay Analysis by Hour of Day",
        yaxis = list(title="Number of flights delayed"),
        xaxis = list(title = "Hour of Day"));
      
    }
    else if(path=="By_Carrier.csv"){
      plot_ly(y =new_data$UniqueCarrier,x= new_data$number_of_delayed_flights,type ="bar",orientation="h")%>%
      layout(title = "Delay Analysis by Carrier",
             yaxis = list(title="Unique Carrier"),
             xaxis = list(title = "Number of flights delayed"));
      
    }
    }
    else{
      new_data_JFK=filter(myData,myData$origin=="JFK");
      new_data_SFO=filter(myData,myData$origin=="SFO");
      new_data_LAX=filter(myData,myData$origin=="LAX");
      if(path=="By_Day_of_Week.csv"){
        plot_ly(x =new_data_JFK$DayOfWeek,y= new_data_JFK$numberOfFlightsDelayed,type ="bar" ,name="JFK")%>%
          add_trace(y = new_data_SFO$numberOfFlightsDelayed, name = 'SFO')%>% 
        add_trace(y = new_data_LAX$numberOfFlightsDelayed, name = 'LAX')%>% 
          layout(title="Delay Analysis by Day of Week",xaxis=list(title='Day of Week'),yaxis = list(title = 'Number of Delayed flights'), barmode = 'group')
      }
      else if(path=="By_Month_of_Year.csv"){
        new_data_JFK_2006=filter(new_data_JFK,new_data_JFK$year=="2006");
        new_data_JFK_2007=filter(new_data_JFK,new_data_JFK$year=="2007");
        new_data_LAX_2006=filter(new_data_LAX,new_data_LAX$year=="2006");
        new_data_LAX_2007=filter(new_data_LAX,new_data_LAX$year=="2007");
        new_data_SFO_2006=filter(new_data_SFO,new_data_SFO$year=="2006");
        new_data_SFO_2007=filter(new_data_SFO,new_data_SFO$year=="2007");
        
        
        p1=plot_ly(new_data, x = new_data_JFK_2006$month, y = new_data_JFK_2006$number_of_delayed_flights, type = 'bar', name = 'JFK-2006') %>%
          add_trace(y = new_data_LAX_2006$number_of_delayed_flights, name = 'LAX-2006') %>%
          add_trace(y = new_data_SFO_2006$number_of_delayed_flights, name = 'SFO-2006') %>%
          layout(title = 'Delay Analysis by Month in 2006-2007 of 3 origin Airports',
                 xaxis = list(title = "Month",
                              showgrid = FALSE),
                 yaxis = list(title = "Number of flights delayed",
                              showgrid = FALSE));
        
        p2=plot_ly(new_data, x = new_data_JFK_2007$month, y = new_data_JFK_2007$number_of_delayed_flights, type = 'bar', name = 'JFK-2007') %>%
          add_trace(y = new_data_LAX_2007$number_of_delayed_flights, name = 'LAX-2007') %>%
          add_trace(y = new_data_SFO_2007$number_of_delayed_flights, name = 'SFO-2007') %>%
          layout(xaxis = list(title = "Month",
                              showgrid = FALSE),
                 yaxis = list(title = "Number of flights delayed",
                              showgrid = FALSE));
        subplot(p1,p2,nrows = 2,shareX = TRUE);
      
        }
      
      else if(path=="By_Hour_of_Day.csv"){
        plot_ly(x =new_data_LAX$timeOfDay,y= new_data_LAX$numberOfDelayedFlights,type ="scatter" ,mode="none", fill = 'tozeroy', fillcolor = '#F5FF8D',name="LAX") %>%
          add_trace(y = new_data_SFO$numberOfDelayedFlights, name = 'SFO', fillcolor = '#50CB86') %>%
          add_trace(y = new_data_JFK$numberOfDelayedFlights, name = 'JFK', fillcolor = '#4C74C9') %>%
          layout(title = 'Delay Analysis by Hour of Day in 3 origin Airports',
                 xaxis = list(title = "Hour of Day",
                              showgrid = FALSE),
                 yaxis = list(title = "Number of flights delayed",
                              showgrid = FALSE))
        
      }
      else if(path=="By_Carrier.csv"){
        
        p1=plot_ly(y =new_data_JFK$UniqueCarrier,x= new_data_JFK$number_of_delayed_flights,type ="bar",orientation="h" ,name="JFK")
        
        p2=plot_ly(y =new_data_LAX$UniqueCarrier,x= new_data_LAX$number_of_delayed_flights,type ="bar",orientation="h",name="LAX")
        
        p3=plot_ly(y =new_data_SFO$UniqueCarrier,x= new_data_SFO$number_of_delayed_flights,type ="bar",orientation="h",name="SFO")%>%
        layout(title = 'Delay Analysis by Carrier in 3 origin Airports',
               xaxis = list(title = "Number of Flights delayed"),
               yaxis = list(title = "Unique Carrier Code"))
        
          subplot(p1,p2,p3,nrows = 3, shareX = TRUE);
        
        
      }
      
      
    }
    
  })
  
})

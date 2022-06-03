#import the required packages for this R script. Usually done at the beginning of the script.
#install the necessary packages
library(dplyr)
library(ggplot2)
library(raster)
library(leaflet)
library(stringr)
library(rgdal)
library(grid)
library(rworldmap)
library(broom)
#Load the shinypackage
library(shiny)


#What is below is needed when we combine raster and dplyr because some functions have similar names in these packages.
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

##################################################################################################
#we first redo the steps seen previously to extract information out of the raw COVID data: #######
##################################################################################################
#we want to visually give some insights, using a map, on the casualities due to COVID by England Map from geostate of its government site.
#first, we need to generate the information by department.

#load COVID dataset
new_data_frame <- read.csv("https://audencia0-my.sharepoint.com/:x:/g/personal/ahmed_tabassam_audencia_com/EeEjNk8UkaRIvAS3wpi_IyEBR9yxsxez7eCBzb9oE4Df_g?e=pOBMcx", header=T, sep=",")
# We change column names so that they are more telling
colnames(new_data_frame)<-c("Area_Code","Region","Date","Case_Number","Death_Number","First_Vac_Injected")
#define gender as a category, i.e., a factor in R vocabulary
new_data_frame$Date<-as.Date(new_data_frame$Date, tryFormats = c("%d/%m/%Y"))



#retrieve department population for different countries
pop_data <- read.csv("Population.csv", sep=",")
head(pop_data)
str(pop_data)



#Next we merge the data we have: we add the population of countries to the COVID dataset. To do so, we use the dplyr function *merge()*:
#then we choose the latest data of each region every day
new_data <- merge(new_data_frame, pop_data, by.x = "Region", by.y = "Region")
new_data <- new_data %>% group_by(Region,Date) 
new_data <- new_data [order(new_data$Date),]


##########################################################################################
#We tried to extract the data that we want to use in the map
new_data1<-new_data %>% filter(Date=="2022-01-01")%>% select(c(Area_Code,Region,Date,Case_Number,Death_Number,First_Vac_Injected,Population))
new_data1_1<- new_data1%>% mutate(death_ratio=(Death_Number/Population)*100) %>% select(c(Date,death_ratio,Region))
#The Percentage of First Vac Injected is also an interesting data
new_data1_4<- new_data %>% filter(Date=="2022-03-30")%>% select(c(Area_Code,Region,Date,Case_Number,Death_Number,First_Vac_Injected,Population))



#Load the shapefile
shapefile <- readOGR(dsn="https://audencia0-my.sharepoint.com/:u:/g/personal/ahmed_tabassam_audencia_com/Ee5mUgXu8dxLgiMHzd-VENsB9tDHahQ34EZl2axUcXKZbQ?e=VFKKmQ", layer="RGN_DEC_2021_EN_BFC")
#Reshape for ggplot2 using the Broom package
mapdata <- tidy(shapefile, Region="RGN21NM")

gg <- ggplot() + geom_polygon(data = mapdata, aes(x = long, y = lat, group = group), color = "#FFFFFF", size = 0.25)
gg <- gg + coord_fixed(1) #This gives the map a nice 1:1 aspect ratio to prevent the map from appearing squashed
print(gg)


data <- read.csv("https://audencia0-my.sharepoint.com/:x:/g/personal/ahmed_tabassam_audencia_com/EdZzaKSA1AdMjp6F9e6Nm38BQIwz7nY-1sNUwFF1xLSYrA?e=xVTEqO", header=T, sep=",")
colnames(data)[1] <- "id" #rename the first column to mirror column names in mapdata
colnames(new_data1_1)[3]<-"RGN21NM"
data <- merge(data,new_data1_1,by="RGN21NM")
mapdata <- merge(mapdata, data, by="id") #merge the two datasets


##########################################################################################
#create the user interface, this time we specify more things

new_data2<- new_data%>% mutate(death_ratio=(Death_Number/Population)*100) 

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel("England 2022 Covid Data"),
  

  sidebarLayout(
    position = "right",
    sidebarPanel(width = 3,
                 
                 varSelectInput(inputId="variable", label =   "Select the variable",new_data2[,4:7]),
                 
                 checkboxInput("check",  "London only" , value = FALSE),
                 
                 sliderInput("condition","Keep if nb death>=",min= 0, max= 20000,value=0),
                 
                 dateInput(inputId="date", label = "Date :", value = "2022-01-01", min = "2022-01-01", max = "2022-04-29", format = "yyyy-mm-dd", language="en")),
    
    mainPanel( p("Please choose the chart:"),
              width = 9,
              tabsetPanel(
                #Function to check the Min, Max, Variation and other info of Each Data
                tabPanel("Variable Summary",verbatimTextOutput("summary"),),
                #the user can pick a time range
                tabPanel("Different regions", plotOutput("plot"),),
                #We demonstrate the trend of each data with line
                tabPanel("Plot over time", plotOutput("plot2"),),
                #The Death Ratio of Different Region of England
                tabPanel("Maps",plotOutput("plot3"),),
                #The Percentage of Death of each Region in England
                tabPanel("Death_Percentage",plotOutput("plot4"),),
                #The Percentage of Vac injected
                tabPanel("Vaccined_People_Percentage",plotOutput("plot5"),)
              ),
                img(src = "Audencia.png", height = 180, width = 280)
            )
  ))
  
  
#we store the server functions here - where we perform calculations based on the user's choices
  server <- function(input, output, session) {

    #for the theme of the outputs to be consistent with the theme of the ui
    thematic::thematic_shiny()
    
##############################################################################################################################
    
    #Establish the database for Panel1-Trend over time
    #Define the Trend of last 5 months by sector
    #Make the capital__London as an independent example
    dataset <- reactive({
      if(input$check==TRUE){
        new_data2 %>% filter(Date==input$date & `Death_Number`>=input$condition & `Region`=="London")%>% group_by(Region,Date) %>%select(c(Region,!!input$variable))
      }
      else{
        new_data2 %>% filter(Date==input$date & `Death_Number`>=input$condition)%>% group_by(Region,Date) %>% select(c(Region,!!input$variable))
      }
    })
    
    dataset2 <- reactive({
      if(input$check==TRUE){
        new_data2 %>% filter(Date>="01/01/2022" & `Death_Number`>=input$condition & `Region`=="London")%>% group_by(Region,Date)  %>% select(c(Date,death_ratio,Region))
      }
      else{
        new_data2 %>% filter(Date>="01/01/2022" & `Death_Number`>=input$condition ) %>% group_by(Region,Date) %>% select(c(Date,death_ratio,Region))
      }
    })
    ##########################################################################################
    #We add a pie chart to indicate the death percentage of each region in England
    #The second pie chart will also be added to indicated the percentage of vaccine injected situation on 30-03-2022 in England
    
    gg_2 <- ggplot(data=new_data1[0:6,], aes(x="", y=Death_Number, fill=Region)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void()
    
    gg_3 <- ggplot(data=new_data1_4[0:7,], aes(x="", y=First_Vac_Injected, fill=Region)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() 
    
    
    
    
    #####################################################################################################################
    
  gg_1 <- ggplot() + geom_polygon(data = mapdata, aes(x = long, y = lat, group = group, fill = death_ratio), color = "#FFFFFF", size = 0.25)
  gg_1 <- gg_1 + coord_fixed(1)
  
  
  

  
  
######################################################################################################################################
    #We create histograms and the charts
  
    output$plot <- renderPlot({
      ggplot(data = dataset(),aes(x=Region,y=!!input$variable)) + geom_col(aes(fill=Region))+ theme_classic() +  ggtitle("the number by region")+labs(fill = "Region",x="Region", y="number")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    output$plot2 <- renderPlot({
      ggplot(data = dataset2(),aes(x=Date,y=death_ratio)) + geom_line(aes(color=Region))+ theme_classic() + labs(x = "Date", y="death_ratio")+ ggtitle("death pop ratio of COVID in different region over time")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+theme(plot.title = element_text(hjust = 0.5))+scale_x_date(date_breaks = "1 month")+scale_y_continuous(n.breaks=30)+scale_color_discrete("")
    })
    
    output$plot3 <- renderPlot({
      gg_1
    })
    
    output$plot4 <- renderPlot({
      gg_2
    })
    
    output$plot5 <- renderPlot({
      gg_3
    })
    
    output$summary <- renderPrint({
      summary(new_data2 %>% select(c(!!input$variable)))
    })
  }
  
  
shinyApp(ui, server)
  
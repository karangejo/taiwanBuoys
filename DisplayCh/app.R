library(shiny)
library(ggplot2)
library(ggcorrplot)




# Define UI for app that draws a histogram ----
ui <- fluidPage(
 # shinythemes::themeSelector(),
  # App title ----
  titlePanel(a("台灣浮標", href="http://taiwanbuoys.com"),windowTitle = "台灣浮標"),
  img(src = "lighthouseFinal.png",width = 200, height =100),
  
  sidebarLayout(
    
    selectInput(
      inputId = "plot",
      label = "你要展示哪個浮標？",
      choices = c("台東","花蓮","蘇澳","宜蘭","小琉球"),
      selected = "台東"
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "waveplot"),
      plotOutput(outputId = "windplot"),
      plotOutput(outputId = "corrPlot"),
      htmlOutput(outputId = "windy"),
      tableOutput(outputId = "tidetable")
      
    )
  )
)



# Shiny server function
server <- function(input, output) {
  
  #dataset <- reactive({get(paste("Get",as.character(input$plot),sep = ""))()})
  
  #  loadData <- reactive({get(paste("load",as.character(input$plot),sep = ""))()})
  
  #  predset <- reactive({
  #    loadData()
  #    get(paste(as.character(input$plot),"TideData",sep = ""))})
  
  # dataset <-reactive({
  #   loadData()
  #    get(paste(as.character(input$plot),"Data",sep=""))})
  
  dataset <- reactive({
    rm(list = setdiff(ls(), lsf.str()))
    filename <- paste(as.character(input$plot),"浮標.Rda",sep="")
    load(filename)
    get(paste(as.character(input$plot),"浮標",sep=""))
  })
  
  predset <- reactive({
    filename <- paste(as.character(input$plot),"潮.Rda",sep="")
    load(filename)
    get(paste(as.character(input$plot),"潮",sep = ""))
  })
  
  # output plots
  output$waveplot <- renderPlot({
    
    dataset <- dataset()
    ggplot(dataset, aes(DateTime,WaveHeight)) +
      scale_x_datetime(date_breaks = "3 hours", date_labels = '%m-%d-%H:%M')+
      geom_point(aes(x = dataset$DateTime , y = dataset$WaveHeight, size = dataset$WavePeriod, color = dataset$WaveDir))+
      geom_smooth(span = 0.3, color = 'red')+
      guides(colour = guide_legend("浪方向"), size = guide_legend("時期"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),axis.title.x=element_blank())+
      ylab('浪大小')+
      ggtitle("浪大小，時期(點大小)和方向(點顏色)")
  })
  
  output$windplot <- renderPlot({
    
    dataset <- dataset()
    ggplot(dataset, aes(DateTime,WindSpeed)) +
      scale_x_datetime(date_breaks = "3 hours", date_labels = '%m-%d-%H:%M')+
      geom_point(aes(x = dataset$DateTime , y = dataset$WindSpeed, size = 5, color = dataset$WindDirection))+
      geom_smooth(span = 0.2, color = 'black')+
      guides(colour = guide_legend("風向"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),axis.title.x=element_blank())+
      ylab('風速')+
      ggtitle("風速和風向(點顏色)")
    
  })

  output$corrPlot <- renderPlot({
    
    dataset <- dataset()
    dataset <- cor(dataset[,c(1,3,4,6)])
    ggcorrplot(dataset,lab="true",type="lower",title = "相關矩陣")
    
  })
  
  output$windy <- renderUI({
    myFrame <- tags$iframe(src="https://embed.windy.com/embed2.html?lat=24.527&lon=127.529&zoom=5&level=surface&overlay=wind&menu=&message=&marker=&calendar=&pressure=&type=map&location=coordinates&detail=&detailLat=50.067&detailLon=14.383&metricWind=default&metricTemp=default&radarRange=-1", width=650, height=450 )
  })
  
  output$tidetable <- renderTable({
    tidetable <- predset()
    return(tidetable)
  })
  
}

shinyApp(ui, server)


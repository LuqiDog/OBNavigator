# https://i.postimg.cc/mgxV4Gz9/background4.png
data <- read.csv("shiny_data_OB.csv")
rownames(data) <- data[,1]
library(shiny)
library(shinyWidgets)
ui <- fluidPage(
  setBackgroundImage(
    src = "https://i.postimg.cc/mgxV4Gz9/background4.png" ),
  fluidRow(
    titlePanel("OB Navigator Decision Support")),
  fluidRow(
    column(3,selectInput(inputId = "county", 
                         label = "Please choose a county",
                         choices = as.list(data[1])),
           radioButtons(inputId = "status",
                        label = "Please choose plot status",
                        choices = c("current","optimized comparison")),
           tags$h3(textOutput("target"))),
    column(4,plotOutput("uninsured"),offset = 1),
    column(4,plotOutput("sti")),
    style = "opacity:0.8"
  ),
  fluidRow(
    column(4,actionButton(inputId = "clicks",label = "Recommendation"),tags$h4(textOutput("rec"))),
    column(4,plotOutput("teen_birth")),
    column(4,plotOutput("hospital")),
    style = "opacity:0.8"
  )
)

server <- function(input,output){
  output$target <- renderText({c("This county belongs to Category", 
                                 data[as.character(input$county),2],
                                 "Target mortality for this county is",
                                 data[as.character(input$county),"TARGET_MORTALITY"],
                                 "Current mortality for this county is", 
                                 data[as.character(input$county),"MORTALITY"])})
  
  observeEvent(input$clicks,{output$rec <- isolate(renderText({as.character(data[as.character(isolate(input$county)),
                                                                                 "RECOMMENDATION"])}))})
  output$teen_birth <- renderPlot({
    if((input$status=="optimized comparison")){
      features <- c("TEEN_BIRTH","TARGET_TEEN_BIRTH")
      features <- factor(x = features, levels = c("TEEN_BIRTH","TARGET_TEEN_BIRTH"))
      dataplot_teen <- data.frame(features=features,
                                  rate=c(data[as.character(input$county),"TEEN_BIRTH"],
                                         data[as.character(input$county),"TARGET_TEEN_BIRTH"]))
      library(ggplot2)
      ggplot(data = dataplot_teen, aes(x=features, y=rate,fill=features))+
        geom_bar(stat = "identity", alpha=0.6)+
        theme(axis.text.x = element_text(face = "bold", angle = 45,hjust = 1))+
        geom_text(aes(label = rate), size = 3, hjust = 0.5, vjust = 3, position = "stack")
    } else{
      features <- c("TEEN_BIRTH")
      dataplot_teen <- data.frame(deatures=features,
                                  rate=c(data[as.character(input$county),"TEEN_BIRTH"]))
      library(ggplot2)
      ggplot(data = dataplot_teen, aes(x=features, y=rate))+
        geom_bar(stat = "identity",width = 0.3,fill = "#F7B9B4", alpha=0.8)+
        theme(axis.text.x = element_text(face = "bold", angle = 45,hjust = 1))+
        geom_text(aes(label = rate), size = 3, hjust = 0.5, vjust = 3, position = "stack")
    }
  })
  
  output$sti <- renderPlot({
    if((input$status=="optimized comparison")){
      features <- c("STI_RATES","TARGET_STI_RATE")
      features <- factor(x = features, levels = c("STI_RATES","TARGET_STI_RATE"))
      dataplot_sti <- data.frame(features=features,
                                 rate=c(data[as.character(input$county),"STI_RATES"],
                                        data[as.character(input$county),"TARGET_STI"]))
      library(ggplot2)
      ggplot(data = dataplot_sti, aes(x=features, y=rate,fill=features))+
        geom_bar(stat = "identity", alpha=0.6)+
        theme(axis.text.x = element_text(face = "bold", angle = 45,hjust = 1))+
        geom_text(aes(label = rate), size = 3, hjust = 0.5, vjust = 3, position = "stack")
    } else{
      features <- c("STI_RATES")
      dataplot_sti <- data.frame(deatures=features,
                                 rate=c(data[as.character(input$county),"STI_RATES"]))
      library(ggplot2)
      ggplot(data = dataplot_sti, aes(x=features, y=rate))+
        geom_bar(stat = "identity",width = 0.3,fill = "#F7B9B4", alpha=0.8)+
        theme(axis.text.x = element_text(face = "bold", angle = 45,hjust = 1))+
        geom_text(aes(label = rate), size = 3, hjust = 0.5, vjust = 3, position = "stack")
    }
  })
  
  output$uninsured <- renderPlot({
    if((input$status=="optimized comparison")){
      features <- c("UNINSURED_RATES","TARGET_UNINSURED_RATE")
      features <- factor(x = features, levels = c("UNINSURED_RATES","TARGET_UNINSURED_RATE"))
      dataplot_uninsured <- data.frame(features=features,
                                       rate=c(data[as.character(input$county),"UNINSURED_RATES"],
                                              data[as.character(input$county),"TARGET_UNINSURED_RATE"]))
      library(ggplot2)
      ggplot(data = dataplot_uninsured, aes(x=features, y=rate,fill=features))+
        geom_bar(stat = "identity", alpha=0.6)+
        theme(axis.text.x = element_text(face = "bold", angle = 45,hjust = 1))+
        geom_text(aes(label = rate), size = 3, hjust = 0.5, vjust = 3, position = "stack")
    } else{
      features <- c("UNINSURED_RATES")
      dataplot_uninsured <- data.frame(deatures=features,
                                       rate=c(data[as.character(input$county),"UNINSURED_RATES"]))
      library(ggplot2)
      ggplot(data = dataplot_uninsured, aes(x=features, y=rate))+
        geom_bar(stat = "identity", width = 0.3, fill = "#F7B9B4",alpha=0.8)+
        theme(axis.text.x = element_text(face = "bold", angle = 45,hjust = 1))+
        geom_text(aes(label = rate), size = 3, hjust = 0.5, vjust = 3, position = "stack")
    }
  })
  
  output$hospital <- renderPlot({
    if((input$status=="optimized comparison")){
      features <- c("PHYSICIANS_RATES","TARGET_PHYSICIANS_RATES")
      features <- factor(x = features, levels = c("PHYSICIANS_RATES","TARGET_PHYSICIANS_RATES"))
      dataplot_hospital <- data.frame(features=features,
                                      rate=c(data[as.character(input$county),"HOSPITAL_RATES"],
                                             data[as.character(input$county),"TARGET_HOSPITAL_RATES"]))
      library(ggplot2)
      ggplot(data = dataplot_hospital, aes(x=features, y=rate,fill=features))+
        geom_bar(stat = "identity", alpha=0.6)+
        theme(axis.text.x = element_text(face = "bold", angle = 45,hjust = 1))+
        geom_text(aes(label = rate), size = 3, hjust = 0.5, vjust = 3, position = "stack")
    } else{
      features <- c("PHYSICIANS_RATES")
      dataplot_hospital <- data.frame(deatures=features,
                                      rate=c(data[as.character(input$county),"HOSPITAL_RATES"]))
      library(ggplot2)
      ggplot(data = dataplot_hospital, aes(x=features, y=rate))+
        geom_bar(stat = "identity", width = 0.3, fill = "#F7B9B4", alpha=0.8)+
        theme(axis.text.x = element_text(face = "bold", angle = 45,hjust = 1))+
        geom_text(aes(label = rate), size = 3, hjust = 0.5, vjust = 3, position = "stack")
    }
  })
} 

shinyApp(ui = ui, server = server)
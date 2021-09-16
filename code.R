#### Yahui Peng ####
#Interactive Visualization Project
#Special Diet & Nutrition Analysis on Dog Food Products at Chewy.com

####Bring In Libraries and the RData####
library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(ggplot2)

#### Change the **** to the directory of file on your computer
load(url("https://*****/stats.RData"))

#### Define UI for application that plots features of fake data ----------- ####
ui <- fluidPage(
  
  titlePanel("Special Diet & Nutrition Analysis for Dog Food Products at Chewy.com"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # add textinput
      textInput(inputId = "plot_title",
                label= "Enter Title Here:",
                placeholder = "e.g. special diet + form + breed + lifestg "),
      
      hr(), #Horizontal Line for visual separation
      
      selectInput(inputId = "diet", 
                  label = "Choose Special Diet:",
                  choices = c("unspecified","grain-free","chicken-free","flax-free","gluten free","high-protein",
                              "hydrolyzed protein","low-protein","low glycemic","no corn no wheat no soy",
                              "pea-free","vegan","limited ingredient diet","natural","non-gmo","organic","raw",
                              "sensitive digestion","weight control","human-grade","vegetarian","veterinary diet"), 
                  selected ="unspecified"
      ),
      
      selectInput(inputId = "form", 
                  label = "Choose Food Form:",
                  choices = c("unspecified","dry food", "wet food","dehydrated","freeze-dried","food topping"), 
                  selected = "unspecified"),
      
      selectInput(inputId = "breed", 
                  label = "Choose Breed:",
                  choices = c("unspecified"," all breeds", "extra small & toy breeds", "medium breeds",
                              "large breeds","small breeds","giant breeds"), 
                  selected ="unspecified"),
      
      selectInput(inputId = "lifestg", 
                  label = "Choose Life Stage:",
                  choices = c("unspecified","puppy","adult","senior"), 
                  selected = "unspecified"),
      
      # Select Colors
      selectInput(inputId = "color_p", 
                  label = "Choose Point Color:",
                  choices = c("Red", "Blue", "Black", "Green","Pink","Grey","Yellow","Orange","Purple"), 
                  selected = "Red"),
      
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Point Transparency:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      hr(), #Horizontal Line for visual separation
      
      # Set min/max of x/y-axis Values
      sliderInput(inputId = "x_min",
                  label = "x-axis Range (Min):", 
                  min = 0, max = 5, step = 0.5,
                  value = 0),
      
      sliderInput(inputId = "x_max",
                  label = "x-axis Range (Max):", 
                  min = 0, max = 5, step = 0.5,
                  value = 5),
      
      
      sliderInput(inputId = "y_max",
                  label = "y-axis Range (Max):", 
                  min = 0, max = max(stats.final$Review_Number, na.rm = T), step = 500,
                  value = max(stats.final$Review_Number, na.rm = T))
    ),
    
    # Output: Show scatterplot, boxplot and tables--------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("Reviews&Ratings", 
                 plotlyOutput(outputId = "scatter_1")),
        tabPanel("Nutrition Facts", 
                 fluidRow(
                   plotlyOutput(outputId = "box_2"),
                   column(6,plotlyOutput(outputId = "box_3")),
                   column(6,plotlyOutput(outputId = "box_4")),
                   verbatimTextOutput("summary"))
        ),
        tabPanel("Selected Data",  DT::dataTableOutput(outputId="table")),
        
        tabPanel("Complete Data",  DT::dataTableOutput(outputId="datasheet"))
      )
    )
  )
)

#### Define server function required to create the scatterplot ---------
server <- function(input, output) {
  #output plot1
  dat1 <- reactive({
    ds1 <- stats.final%>% 
      .[grepl(ifelse(input$form=="unspecified","",input$form),.$Form)& 
          grepl(ifelse(input$lifestg=="unspecified","",input$lifestg),.$Life_Stage) &
          grepl(ifelse(input$breed=="unspecified","",input$breed),.$Breed)&
          grepl(ifelse(input$diet=="unspecified","",input$diet),.$Special_diet),]
    return(ds1)
  })
  
  output$scatter_1 <- renderPlotly({
    d1 <- dat1()
    p1 <- ggplot(data = d1, aes_string(x = d1$Rating, y = d1$Review_Number, text = d1$Price.per.lb)) +
      geom_point(colour=input$color_p, alpha=input$alpha) + 
      xlab("Rating")+ ylab("Review Counts")+
      labs(title=paste0(input$plot_title," (",as.character(nrow(d1))," products)"))+
      xlim(as.numeric(input$x_min), as.numeric(input$x_max))+ ylim(0, as.numeric(input$y_max))
  }) 

  #output plot2
  dat2 <- reactive({
    if(nrow(dat1())> 0){
      ds2 <- na.omit(gather(dat1()[,c(3:7)], factor_key=TRUE))
      ds2$key <- as.character(ds2$key)
      ds2$key<-factor(ds2$key, levels=c("Protein", "Fat", "Omega_6s","Calcium","Phosphorus"))
      return(ds2)
    }else{
      return(dat1())
    }
  })
  
  output$box_2 <- renderPlotly({
    if (nrow(dat2())>0){
        ggplot(dat2(), aes(x = key, y=value, fill=key)) +
        geom_boxplot(alpha=0.8)+ ylab("%")+
        theme(axis.title.x=element_blank())
    } else{
      ggplot(data=data.frame())+geom_blank()+labs(title = "No available data")
    }
  })
 
  #output plot3
   dat3 <- reactive({
     if(nrow(dat1())> 0){
       ds3 <- na.omit(gather(dat1()[,c(8:9)], factor_key=TRUE))
       ds3$key <- as.character(ds3$key)
       ds3$key <- gsub("CP_ratio", "Calcium/Phosphorus", ds3$key)
       ds3 <- filter(ds3,key == "Calcium/Phosphorus")
       return(ds3)
     }else{
       return(dat1())
     }
   })
  
  output$box_3 <- renderPlotly({
    if (nrow(dat3())>0){
      ggplot(dat3(), aes(x = key, y=value, fill=key)) +
        geom_boxplot(fill="#FF33CC",alpha=0.8)+ ylab("Ratio")+
        theme(axis.title.x=element_blank())
    } else{
      ggplot(data=data.frame())+geom_blank()+labs(title = "No available data")
    }
  }) 
  
  #output plot4
  dat4 <- reactive({
    if(nrow(dat1())> 0){
      ds4 <- na.omit(gather(dat1()[,c(8:9)], factor_key=TRUE))
      ds4$key <- as.character(ds4$key)
      ds4 <- filter(ds4,key == "Glucosamine")
      return(ds4)
    }else{
      return(dat1())
    }
  })
  
  output$box_4 <- renderPlotly({
  if (nrow(dat4())>0){
    ggplot(dat4(), aes(x = key, y=value, fill=key)) +
      geom_boxplot(fill="#660000",alpha=0.7)+ ylab("mg/kg")+
      theme(axis.title.x=element_blank())
  } else{
    ggplot(data=data.frame())+geom_blank()+labs(title = "No available data")
  }
}) 
  
  #output summary table of nutrition contents
  output$summary <- renderPrint({
    summary <- dat1()[,c(3:9)]
    summary(summary)
  })
  
  #output the sub-dataset
  output$table <- DT::renderDataTable({
    d3 <- dat1()
    DT::datatable(data=d3,
                  options=list(pageLength= 20),
                  rownames=FALSE)
  })
  
  #output the whole dataset
  output$datasheet<-DT::renderDataTable({
    DT::datatable(data=stats.final,
                  options=list(pageLength= 20),
                  rownames=FALSE)
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)

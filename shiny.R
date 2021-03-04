library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)

minval <- min(as.Date("2016-12-30"))
maxval <- max(as.Date("2022-07-30"))
startval <- min(as.Date(Sys.Date()-years(1)))
endval <- max(Sys.Date())

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


ui <- fluidPage(
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Open+Sans&display=swap');
      body {
        background-color: lightblue;
        color: black;
      }
      h2 {
        font-family: 'Open Sans', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))
  ),
  
  
  
  
  titlePanel("wplaty I wyplaty "),
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      fileInput("Zeszyt1", "Wczytaj plik",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", sep=";", dec=",", header=TRUE)),
      tags$hr(),
      
      dateRangeInput("r",
                     label = 'Zakres dat', 
                     start = startval, end = endval, 
                     min = minval, max = maxval, format="yyyy-mm"
      ),
      
      
      # Select variable for y-axis
      selectInput(inputId = "gog", 
                  label = "Zmienna na wykresie nr1 i nr3:",
                  choices = c("Ilosc wplat BM" = 'il_wp_bm', 
                              "Srednia wplata" = 'srednia_wp',
                              "Ilosc wyplat"="il_wyplat",
                              "wyplaty"="wyplaty",
                              "Srednia wyplata"="sr_wypl",
                              "Ilosc anulowanych wyplat"="il_an_wyplaty",
                              "Anulowane wyplaty"="an_wyplaty",
                              "roz_wp_zreal_wyp"="roz_wp_zreal_wyp",
                              "Ilosc rejestracji"="il_rejestracji",
                              "aktywnosc"="aktywnosc",
                              "Obrot"="obrot",
                              "Obrot2"="obrot2",
                              "Obrot b"="obrotb",
                              "Wygrana glowna"="wygrana_gl",
                              "Wygrana b"="wygrana_b",
                              "Wynik na zakladach"="wynik_na_zakladach",
                              "Koszt bonusow"="koszt_bonusow"
                              
                  ) ,
                  selected = "Ilosc wplat BM"),
      
      conditionalPanel(
        condition="output.file",
        h4("Suma wplat w wybranym okresie wynosi:"),
        h3(textOutput("tekst1")),
        
        #(div(verbatimTextOutput(outputId = "tekst1"),style="font-size:500px")),
        #h4("Srednia wplat w wybranym okresie na dzien wynosi:"),
        #verbatimTextOutput(outputId = "tekst2"),
        #  h3(textOutput("tekst2")),
        h4("Suma wyplat w wybranym okresie wynosi:"),
        #verbatimTextOutput(outputId = "tekst3"),
        h3(textOutput("tekst3")),
        #h4("Srednia wyplat w wybranym okresie na dzien wynosi:"),
        #h3(textOutput("tekst4")),
        #verbatimTextOutput(outputId = "tekst4"),
        h4("Ilosc rejestracji w wybranym okresie wynosi:"),
        h3(textOutput("tekst5")),
        #verbatimTextOutput(outputId = "tekst5"),
        h4("Roznica wplat i wyplat w wybranym okresie wynosi:"),
        h3(textOutput("tekst6")),
        #verbatimTextOutput(outputId = "tekst6"),
        h4("Wynik na zakladach w wybranym okresie wynosi:"),
        #verbatimTextOutput(outputId = "tekst7"),
        h3(textOutput("tekst7"))
        
      )
      
      
      
    ),
    
    
    # Output
    mainPanel( conditionalPanel(
      condition="output.file",
      plotOutput(outputId = "scatterplot2"),
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "boxplot"),
      tabPanel("Zeszyt1", DT::dataTableOutput("mytable2"))
    ))
  )  )



# Define server function required to create the scatterplot
server <- function(input, output, session) {
  output$file <- reactive({ 
    return(!is.null(input$Zeszyt1))     }) 
  
  outputOptions(output, "file", suspendWhenHidden = FALSE)
  
  
  data_1=reactive({
    Zeszyt1 <- data.frame(read.csv(input$Zeszyt1$datapath, sep=";", dec=",", header=TRUE))
    #return(rbind(Zeszyt1[Zeszyt1$DATA%in%input$s,]))
    return(Zeszyt1)
  })
  gog1=reactive({
    gog1 <- input$gog
    #return(rbind(Zeszyt1[Zeszyt1$DATA%in%input$s,]))
    return(gog1)
  })
  output$tekst1 <- renderText({
    req(data_1())
    text1 <- round((sum(as.numeric(data_1()$wplaty[data_1()$report_day >input$r[1]& data_1()$report_day < input$r[2]]))))
    paste(text1)
  })
  output$tekst2 <- renderText({
    req(data_1())
    text2 <- round((mean(as.numeric(data_1()$wplaty[data_1()$report_day >input$r[1]& data_1()$report_day < input$r[2]]))))
    paste(text2)
  })
  output$tekst3 <- renderText({
    req(data_1())
    text3 <- round((sum(as.numeric(data_1()$wyplaty[data_1()$report_day >input$r[1]& data_1()$report_day < input$r[2]]))))
    paste(text3)
  })
  output$tekst4 <- renderText({
    req(data_1())
    text4 <- round((mean(as.numeric(data_1()$wyplaty[data_1()$report_day >input$r[1]& data_1()$report_day < input$r[2]]))))
    paste(text4)
  })
  output$tekst5 <- renderText({
    req(data_1())
    text5 <- round((sum(as.numeric(data_1()$il_rejestracji[data_1()$report_day >input$r[1]& data_1()$report_day < input$r[2]]))))
    paste(text5)
  })
  output$tekst6 <- renderText({
    req(data_1())
    text6 <- round((sum(as.numeric(data_1()$wplaty[data_1()$report_day >input$r[1]& data_1()$report_day < input$r[2]])))-
                     (sum(as.numeric(data_1()$wyplaty[data_1()$report_day >input$r[1]& data_1()$report_day < input$r[2]]))))
    paste(text6)
  })
  output$tekst7 <- renderText({
    req(data_1())
    text7 <- round((sum(as.numeric(data_1()$wynik_na_zakladach[data_1()$report_day >input$r[1]& data_1()$report_day < input$r[2]]))))
    paste(text7)
  })
  tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
  )
  )
  
  
  
  output$scatterplot <- renderPlot({
    req(data_1())
    
    Zeszyt1 <- data.frame(read.csv(input$Zeszyt1$datapath, sep=";", dec=",", header=TRUE))
    q <- as.Date( data_1()[,1])
    #m <- data_1()[,1]
    v <- as.numeric( data_1()[,3])
    #h <- data_1()[,5]
    j <- as.numeric( data_1()[,6])
    
    ggplot(data = data_1(), aes(x = q, y = v)) +
      geom_line(aes(y=v, color='red')) + geom_ribbon( aes(ymin=0, ymax=v), alpha = 1/10) + geom_line(aes(y=j , color='blue')) +geom_ribbon( aes(ymin=0, ymax=j), alpha = 1/10)+geom_hline(yintercept=0)+ xlim(input$r[1], input$r[2]) + 
      scale_radius(range = c(1, 10)) + scale_y_continuous(limits = c(0, 500000)) + scale_x_date(date_breaks = "2 months", limits = c(input$r[1], input$r[2]) ) +ggtitle("wplaty i wyplaty w wybranym okresie")+ xlab(label="Data") +  ylab(label="wplaty/wyplaty") + theme(legend.position = "none") 
    
    
  })
  
  output$scatterplot2 <- renderPlot({
    
    req(data_1())
    Zeszyt1 <- data.frame(read.csv(input$Zeszyt1$datapath, sep=";", dec=",", header=TRUE))
    
    q <- as.Date(data_1()[,1])
    #m <- data_1()[,1]
    
    
    v <- as.numeric(data_1()[,input$gog])
    #data5 <- cbind(as.Date(data_1()[,1]),v)
    #data6 <- data.frame(na.omit(data5))
    #h <- data_1()[,5]
    
    ggplot(data = data_1(), aes_string(x = q, y = v)) +
      
      geom_line(aes(y= v  )) + xlim(input$r[1], input$r[2]) + 
      scale_radius(range = c(1, 10)) + scale_y_continuous(limits = c(0, max(v)*2)) + scale_x_date(date_breaks = "2 months", limits = c(input$r[1], input$r[2]) ) +ggtitle("Wybrana zmienna w wybranym okresie czasowym")+ xlab(label="Data") +  ylab(label="Zmienna")
  })
  
  output$boxplot <- renderPlot({
    req(data_1())
    
    Zeszyt1 <- data.frame(read.csv(input$Zeszyt1$datapath, sep=";", dec=",", header=TRUE))
    q <- (as.Date( data_1()[,1]))
    #m <- data_1()[,1]
    v <-  as.numeric(data_1()[,input$gog])
    
    
    # ggplot(data = data_1(), aes(x = as.factor(q), y = v)) + geom_bar(position="dodge")+ xlim(input$r[1], input$r[2]) + scale_y_continuous(limits = c(0, 2*max(v))) + scale_x_date(date_breaks = "2 months", limits = c(input$r[1], input$r[2]) ) + xlab(label="Data") +  ylab(label="Zmienna")
    #ggplot(data_1(), aes(x = month(q), y = v, fill = year(q), group = year(q)))+geom_bar(stat="identity", position="dodge")
    ggplot(data_1(), aes(x = year(q), y = v, group = year(q)))+geom_bar(stat="identity", position="dodge")+ggtitle("Wybrana zmienna w zaleznosci od roku")+ xlab(label="Data") +  ylab(label="Zmienna")
    
    
    
  })
  
  
  
  
  output$mytable2 <- DT::renderDataTable({
    Zeszyt1 <- data.frame(read.csv(input$Zeszyt1$datapath, sep=";", dec=",", header=TRUE))
    
    Zeszyt4 <- data.frame(read.csv(input$Zeszyt1$datapath, sep=";", dec=",", header=TRUE))
    
    Zeszyt3 = cbind(Zeszyt4[,1],Zeszyt4[,2], Zeszyt4[,4], Zeszyt4[,6], Zeszyt4[,7], Zeszyt4[,8], Zeszyt4[,9], Zeszyt4[,10], Zeszyt4[,11], Zeszyt4[,12], Zeszyt4[,13], Zeszyt4[,14], Zeszyt4[,15], Zeszyt4[,18], Zeszyt4[,19], Zeszyt4[,20], Zeszyt4[,21])
    DT::datatable(Zeszyt3, colnames=c("Data",  "Ilosc wplat BM","wplaty", "Ilosc wyplat", "wyplaty", "ilosc anulowanych wyplat",
                                      'anulowane wyplaty', "ILosc wyplat", 'wyplaty', 'roz_wp_zreal_wyp', "Ilosc rejestracji", "aktywnosc", "Obrot",
                                      "Wygrana glowna", "Wygrana B", "Wynik za zakladach", "Koszt bonusow")
                  , filter = list(position = 'top', clear = FALSE),
                  options = list(
                    search = list(regex = FALSE, caseInsensitive = TRUE),
                    pageLength = 100))
  })
  #colnames=c("Data",  "Ilosc wplat BM","wplaty","Srednia wplata", "Ilosc wyplat", "wyplaty", "Srednia wyplata", "ilosc anulowanych wyplat",
  #           'anulowane wyplaty', "ILosc wyplat", 'wyplaty', 'roz_wp_zreal_wyp', "Ilosc rejestracji", "aktywnosc", "Obrot", "Obrot2", "Obrotb",
  #           "Wygrana glowna", "Wygrana B", "Wynik za zakladach", "Koszt bonusow"
  
  output$outputId1 <- renderText({ halo
  })
}
# Create a Shiny app object
shinyApp(ui = ui, server = server)
#runApp(appDir = list(ui = ui,server = server), port = 7775, host = '10.38.16.40', launch.browser = TRUE)





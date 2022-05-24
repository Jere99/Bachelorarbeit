

options(java.parameters = "-Xmx8000m") 
library(shiny)
library(tidyverse)
library(xlsx)
library(reshape)


# path <- "PATH....PATH/Crypto_GARCH.xlsx"
path <- "~/R/projects/Bachelorarbeit/results/Crypto_GARCH.xlsx"

### Percentage plots
percentage_data_full <- function(data, vari, value){
  data <- data %>% select(name, p, q, distr, coin, value) %>% 
    pivot_wider(names_from = vari, values_from = value)
  return(data)
}
percentage_graph_full <- function(percentage_data, vari, value){
  
  min <- 5
  max <- length(percentage_data)
  mean_vect <- c()
  for(i in c(min:max)){
    for(j in c(min:max)){
      temp_df <- percentage_data[,c(i, j)] %>% na.omit()
      if(value == "LLH"){
        mean_vect <- c(mean_vect, round(mean(temp_df[,1] > temp_df[,2]),2))
      } else {
        mean_vect <- c(mean_vect, round(mean(temp_df[,1] < temp_df[,2]),2))
      }
      
    }
  }
  
  domination_mat <- matrix(mean_vect, ncol = sqrt(length(mean_vect)))
  
  colnames(domination_mat) <- names(percentage_data[5:max])
  rownames(domination_mat) <- names(percentage_data[5:max])
  for(i in c(1:nrow(domination_mat))){
    domination_mat[i, i] <- NA
  }
  mat_melted <- melt(domination_mat, na.rm = TRUE)
  
  if(value == "LLH"){
    crit <- "höheren LLH"
  } else if(value == "AIC"){
    crit <- "tieferen AIC"
  } else {
    crit <- "tieferen BIC"
  }
  
  if(vari == "name"){
    title2 <- "Modeltyp"
  } else if(vari == "distr"){
    title2 <- "Verteilung"
  } else if(vari == "p"){
    title2 <- "GARCH Parameter (p)"
  } else if(vari == "q"){
    title2 <- "ARCH Parameter (q)"
  }
  
  if(vari == "name"){
    varname <- "von Typ"
    axisname <- "Typ"
  } else if(vari == "distr"){
    varname <- "mit Verteilung"
    axisname <- "Verteilung"
  } else if(vari == "p"){
    varname <- "mit Parameterzah p"
    axisname <- "p"
  } else if(vari == "q"){
    varname <- "mit Parameterzahl q"
    axisname <- "q"
  }
  
  
  
  
  ggplot(data = mat_melted, aes(X1, X2, fill = value))+
    geom_tile(color = "white") +
    
    scale_fill_gradient2(low = "white", high = "dark red", 
                         limit = c(0,1), space = "Lab", 
                         name="relative Häufigkeit") +
    
    geom_text(aes(X1, X2,label=value), col = "black") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 60, vjust = 1,  hjust = 1))+
    coord_fixed() +
    labs(x = paste0(axisname, 2), y = paste0(axisname, 1), 
         title = paste("Vergleich nach: " ,title2),
         subtitle = paste0("Realtive Häufigkeit: Modelle ", varname,"1,\nwelche einen", crit, 
                           " haben, als Modelle\n", varname, "2, ceteris paribus."))
}
percentage_total_full <- function(data, vari, value){
  data <- percentage_data_full(data, vari, value)
  percentage_graph_full(data, vari, value)
}




all.analysis <- read.xlsx(path, "final_FINAL", stringsAsFactors=T)


error_vect <- (all.analysis$LLH < 0)
error_vect[is.na(error_vect)] <- TRUE

final_all.analysis <- all.analysis[!error_vect,]

onlygarch <- final_all.analysis %>% 
  filter(p != 0)

# user interface
ui1 <- fluidPage(
  titlePanel("GARCH Analysis"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("Comparison", 
                         h3("Compare:"), 
                         choices = list("model type" = "name", 
                                        "distribution" = "distr", 
                                        "p" = "p",
                                        "q" = "q"),
                         selected = "name"),
      
      radioButtons("Criteria", 
                         h3("Criteria:"), 
                         choices = list("LLH" = "LLH", 
                                        "AIC" = "AIC", 
                                        "BIC" = "BIC"),
                         selected = "LLH"),
      
      radioButtons("Coin", 
                   h3("Cryptocurrency:"), 
                   choices = list("all" = "all",
                                  "Bitcoin" = "Bitcoin", 
                                  "BNB" = "BNB", 
                                  "Cardano" = "Cardano",
                                  "Dogecoin" = "Dogecoin",
                                  "Ethereum" = "Ethereum",
                                  "Ripple" = "Ripple",
                                  "Tether" = "Tether"),
                   selected = "all")
      
      
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

# server
server1 <- function(input, output){
  
  options(warn = -1)
  
  getData <- reactive({
    if(input$Coin == "all"){
      onlygarch
    } else {
      onlygarch %>% 
        filter(coin == input$Coin)
    }
    

  })
  
  getInput <- reactive({
    
    c(input$Comparison, input$Criteria)
    
  })
  
  
  
  output$plot1 <- renderPlot(
    plot(percentage_total_full(getData(), getInput()[1], getInput()[2]))
  )
  
  

                             
                             
}


# Run the app
shinyApp(ui = ui1, server = server1)

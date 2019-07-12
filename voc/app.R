library(fastrtext)
library(jiebaR)
library(stringr)
library(readxl)
library(readr)
library(xlsx)
library(shiny)

Sys.setlocale("LC_ALL","chinese")
options(warn = -1)
options(shiny.maxRequestSize=30*1024^2) 

ui <-fluidPage(
  titlePanel(title = div("ABO Feedback Text Classification", img(src="R&D Logo.PNG", height=80, width=240, style = "float:right; padding-right:25px"))),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput("file", "Upload the Text File to Predict"),
      
      br(),
      helpText("Please upload your file in .txt format encoding with UTF-8.",
               "When you click the button below, you should see",
               "the predicted result on the right side of the panel."
      ),
      br(),
      actionButton("Load", "Output the Predicted Result"),
      
      br(),
      helpText("  
               "),
      downloadButton("downloadData","Download Predicted Result")
      
      
      ),
    # mainPanel(verbatimTextOutput("value"))
    mainPanel( uiOutput("tb") )
  )
)

server <- function(input, output, session){
  df <- reactive({
    
    if(input$Load == 0){return()}
    
    if (is.null(input$file)){return(NULL)}
    
    isolate({
      input$Load
      
      a<-read.csv(input$file$datapath, header = T)
      
      my_data <- data.frame(a)
      
      engine <- worker()
      
      for (m in 1:nrow(my_data)) {
        my_data$raw[m] = paste(my_data$text[m], collapse = "")
        my_data$text1[m] = paste(str_extract_all(my_data$text[m], "[\u4e00-\u9fa5]")[[1]], collapse = "")
      }
      
      for (n in 1:nrow(my_data)) {
        my_data$text2[n] = paste(segment(my_data$text1[n], engine), collapse = " ")
      }
      
      model <- load_model("/root/voc/txtcls_model.bin")
      
      predictions <- predict(model, enc2native(my_data[["text2"]]))
      
      for (k in 1:length(predictions)) {
        predictions$label[k] = substr(rownames(data.frame(predictions[k])),10,12)
        predictions$probability[k] = round(data.frame(predictions[k])[1,1], digits = 4)
      }
      
      pred_result <- data.frame(cbind(my_data$raw, predictions$label, predictions$probability))
      names(pred_result)[1] <- "ABO Feedback"
      names(pred_result)[2] <- "Predicted Defect Code"
      names(pred_result)[3] <- "Probability"
    })
    
    pred_result
    
  })  
  
  
  output$table <- renderTable({
    df()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Predicted Results for ABO Feedback.xlsx")
    },
    
    content = function(file) {
      write.xlsx2(df(), file, row.names = T, col.names = T, append = F)
    }
  )
  
  
  output$caption <- renderText({paste("This APP is developed by Jason Yang. It is used for ABO feedback text classification prediction. For any technical issue, please contact jason.jian.yang@amway.com, thank you!")})
  
  output$tb <- renderUI({
    if(is.null(df()))
      h5("Powered by", tags$img(src='aws_fastrtext.PNG', heigth=300, width=600))
    else
      tabsetPanel(tabPanel("Predicted Result", tableOutput("table")), tabPanel("About the APP", textOutput("caption")))
  })
  
}

shinyApp(ui = ui, server = server)




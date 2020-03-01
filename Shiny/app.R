
library(shiny)
library(KoNLP)
library(stringr)
library(wordcloud)

ui <- fluidPage(
    
    # App title
    titlePanel("Korean Word Cloud"),
    tags$hr(),
    
    fluidRow(
        column(4,
               fileInput("file", "Choose CSV File", placeholder="No File selected", multiple = FALSE, accept = c(".csv"))
        ),
        column(4,
               sliderInput("minFreq", "Minimum Frequency", min =1, max = 50, value = 4)
        ),
        column(4,             
               sliderInput("maxWord", "Maximum Number of Words", min = 1, max = 40, value = 10)
        )
    ),
    
    fluidRow(
        tags$hr() 
    ),
    
    fluidRow(
        h2("Word Cloud"),
        tags$hr(),
        column(4,
               h2("SejongDic"),
               plotOutput("plots1")
        ),
        column(4,
               h2("NIADic"),
               plotOutput("plots2")
        ),
        column(4,
               h2("SimplePos09"),
               plotOutput("plots3")
        )
    ),    
    
    fluidRow(
        h2("Word Analysis"),
        tags$hr(),
        column(4, 
               h2("SejongDic"),
               DT::dataTableOutput("tables1")
        ),
        column(4, 
               h2("NIADic"),
               DT::dataTableOutput("tables2")
        ),
        column(4, 
               h2("SimplePos09"),
               DT::dataTableOutput("tables3")
        )
    )
)

server <- function(input, output, session) {
    
    terms1 <- reactive({
        req(input$file)
        tryCatch(
            {
                df <- read.csv(input$file$datapath)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        isolate({
            withProgress(message="SejongDic Processing ...",{
                # setProgress(message="SejongDic Processing ...")
                
                colnames(df) <- c("contents")
                text <- as.character(data$contents)
                
                useSejongDic()
                
                raw_noun <- sapply(text, extractNoun, USE.NAMES = F)
                unlist_noun <- unlist(raw_noun)
                noun <- Filter(function(x){nchar(x)>=2}, unlist_noun)
                
                words <- table(noun)
                word_count <- words[nchar(names(words))>1]
                sort(word_count, decreasing = T)
            })
        })
    })

    terms2 <- reactive({
        req(input$file)
        tryCatch(
            {
                df <- read.csv(input$file$datapath)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        isolate({
            withProgress(message="NIADic Processing ...", {
                # setProgress(message="NIADic Processing ...")
                
                colnames(df) <- c("contents")
                text <- as.character(data$contents)
                
                useNIADic()
                
                raw_noun <- sapply(text, extractNoun, USE.NAMES = F)
                unlist_noun <- unlist(raw_noun)
                noun <- Filter(function(x){nchar(x)>=2}, unlist_noun)
                
                words <- table(noun)
                word_count <- words[nchar(names(words))>1]
                sort(word_count, decreasing = T)
            })
        })
    })
    
    terms3 <- reactive({
        req(input$file)
        tryCatch(
            {
                df <- read.csv(input$file$datapath)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        isolate({
            withProgress(message="SimplePos09 Processing ...", {
                # setProgress(message="SimplePos09 Processing ...")
                
                colnames(df) <- c("contents")
                text <- as.character(data$contents)
                
                raw_noun <- sapply(text, SimplePos09, USE.NAMES = F)
                unlist_noun <- unlist(raw_noun)
                split_noun <- sapply(str_split(unlist_noun, "/"), function(x){x[1]} )
                noun <- Filter(function(x){nchar(x)>=2}, split_noun)
                
                words <- table(noun)
                word_count <- words[nchar(names(words))>1]
                sort(word_count, decreasing = T)
        })  
    })
})
    

    output$plots1 <- renderPlot({
        count <- terms1()
        wordcloud(names(count), count, scale=c(4,0.5),
                      min.freq = input$minFreq, max.words=input$maxWord,
                      colors=brewer.pal(8, "Dark2"))

    })
    
    output$plots2 <- renderPlot({
        count <- terms2()
        wordcloud(names(count), count, scale=c(4,0.5),
                  min.freq = input$minFreq, max.words=input$maxWord,
                  colors=brewer.pal(8, "Dark2"))
        
    })

    output$plots3 <- renderPlot({
        count <- terms3()
        wordcloud(names(count), count, scale=c(4,0.5),
                  min.freq = input$minFreq, max.words=input$maxWord,
                  colors=brewer.pal(8, "Dark2"))
        
    })
    
    output$tables1 <- DT::renderDataTable({
        
        count <- terms1()
        count <- count[1:input$maxWord]
        count <- as.data.frame(count)
        
        DT::datatable(count)
        
    })
    
    output$tables2 <- DT::renderDataTable({
        
        count <- terms2()
        count <- count[1:input$maxWord]
        count <- as.data.frame(count)
        
        DT::datatable(count)
        
    })
    
    output$tables3 <- DT::renderDataTable({
        
        count <- terms3()
        count <- count[1:input$maxWord]
        count <- as.data.frame(count)
        
        DT::datatable(count)
        
    })
    
}

shinyApp(ui, server)

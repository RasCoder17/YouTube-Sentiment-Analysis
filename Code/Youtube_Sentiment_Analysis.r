library(shinyvalidate)
library(shinyjs)
library(SentimentAnalysis)
library(shiny)
library(vosonSML)
library(ggplot2)
library("tidytext")
library("widyr")
library(tidyverse)
library("ggraph")
library("tm")
library(dplyr)
library('ggplot2')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("RSentiment")
library(ggplot2)
library(lattice)
library(caret)
pageButtonUi <- function(id) {
  actionButton(NS(id, "page_change"),
               label="Change the Page")
}


# pageButtonServer <- function(id, parentSession) {
#   moduleServer(id, function(input, output, session) {
#     
#     observeEvent(input$page_change, {
#       updateNavbarPage(session=parentSession,
#                        inputId="pages",
#                        selected="second_page")
#     })
#   })
# background-color: #5F9EA0;}


ui <- navbarPage(
  title="YouTube Sentiment Analysis",
  id="pages",
  tabPanel(title="Home",
           mainPanel(
             tags$style('.container-fluid {
                             
              }'),
             div(
               (
                 tagList(
                   div(h2("Youtube Sentiment Analysis", align = "center")),
                   br(),
                   div(br(),
                       br(),
                       h3("See the likeability of YouTube Videos with just one click!",align="center"),
                       br(),
                       br(),
                       div(br(),h3("Enter a video link: "),br(),textInput("Name",label="Video Watch Link",placeholder="Enter YouTube Watch Link",width="400px"),conditionalPanel(condition = "(input.submit_info >= 1) & ((input.id == '') || (!input.agree))",
                                                                                                                                                                                 
                                                                                                                                                                                 textOutput('error_msg'),textOutput('success_msg'),
                       ),align="center",actionButton("submit_info","Submit"),br(),br())
                       ,style = "background-color: seashell"),br(),
                   tags$head(
                     tags$style(HTML(
                       "html {
             min-height: 100%;
             margin:0px;
             padding:0px;
           }
           body {
             margin-bottom: 60px; /* Margin bottom by footer height */
           }
           .footer {
    position: fixed;
    left: 0;
    bottom: 0;
    width: 100%;
    background-color: seashell;
    color: black;
    text-align: center;
    text-decoration: solid;
    margin: 0 auto;
    padding: 10px 10px;  
    }"))),br(),
                   tags$footer("©By Jafrin, Rahul and Pooja", class = "footer")
                 )
               )
             ),style = "background-color: skyblue;margin-left:200px")
  ),
  
  tabPanel(title="Output", 
           "Number of positive comments: ",textOutput("out_name1"), 
           br(),"Positive comments: ",br(),textOutput("out_name11"),
           "Number of negative comments: ",textOutput("out_name2"),"Negative comments: ",textOutput("out_name22"),br(),br(),"Term Document Matrix",tableOutput("out_name3"),br(),plotOutput("myBar5"),br(),"Most frequently used words",plotOutput("myBar1"),br(),br(),br(),br(),"Emotion Count",plotOutput("myBar2"),br(),br(),plotOutput("myBar4"),"Confusion Matrix",textOutput("out_name4"),br(),plotOutput("myBar3"),style = "background-color: seashell;  color: black;
    text-align: center;
    width: 100%;
    align-content: center;
    margin: 0 auto;
    padding: 0.1px 1px;  
    margin: 0 auto;"
           
  )
)


server <- function(input, output, session) {
  #iv <- InputValidator$new()
  #iv$add_rule("name", sv_required())
  #iv$enable()
  output$out_name1 <- renderText({
    shiny::validate(
      shiny::need(input$Name != '', '')
    )
    
  })
  
  output$out_name2 <- renderText({
    shiny::validate(
      shiny::need(input$Name != '', '')
    )
  })
  output$out_name11 <- renderText({
    shiny::validate(
      shiny::need(input$Name != '', '')
    )
  })
  output$out_name22 <- renderText({
    shiny::validate(
      shiny::need(input$Name != '', '')
    )
  })
  output$out_name4 <- renderText({
    shiny::validate(
      shiny::need(input$Name != '', '')
    )
  })
  
  
  
  # pageButtonServer("page", parentSession = session)
  observeEvent(input$submit_info, {
    
    shiny::req(input$Name)
    output$success_msg <- renderText({"Success"})
    file = renderText(input$Name)
    filecode = substr(toString(file()),18,29)
    videoID <- c(filecode,file())
    youtubeData <- auth_yt |>
      Collect(videoID, maxComments = 100, writeToFile = TRUE)
    youtubeData
    yt_df = as.data.frame(youtubeData)
    yt_df
    comments = yt_df$Comment
    comments
    commlikes = yt_df$LikeCount
    comments = yt_df$Comment
    head(comments,5)
    comments
    #checking for missing values
    anyNA(comments) 
    
    #checking for duplicate values
    sum(duplicated(comments))
    
    suppressWarnings({
      
      #removing duplicate entries
      comments=comments[duplicated(comments)!= "TRUE"]
      comments
      
      #Corpus for comments
      textcorpus= Corpus(VectorSource(comments))
      textcorpus
      
      #Replacing "/","@","|" with space
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      textcorpus <- tm_map(textcorpus, toSpace, "/")
      textcorpus <- tm_map(textcorpus, toSpace, "@")
      textcorpus <- tm_map(textcorpus, toSpace, "\\|")
      
      # Convert the text to lower case
      textcorpus <- tm_map(textcorpus, content_transformer(tolower))
      
      # Remove numbers
      textcorpus <- tm_map(textcorpus, removeNumbers)
      
      # Remove english common stopwords
      textcorpus <- tm_map(textcorpus, removeWords, stopwords("english"))
      
      # Remove your own stop word
      # specify your custom stopwords as a character vector
      textcorpus <- tm_map(textcorpus, removeWords, c("https","youtubecom","hai","danc"))
      
      # Remove punctuations
      textcorpus <- tm_map(textcorpus, removePunctuation)
      
      # Eliminate extra white spaces
      textcorpus <- tm_map(textcorpus, stripWhitespace)
      
      # Text stemming - which reduces words to their root form
      textcorpus <- tm_map(textcorpus, stemDocument)
    })
    
    # Building a term-document matrix
    textcorpus_tdm <- TermDocumentMatrix(textcorpus)
    tdm_matrix <- as.matrix(textcorpus_tdm)
    tdm2 <- tdm_matrix[1:10,1:20]
    tdm2
    tdm3 <- cbind(Word = rownames(tdm2), tdm2)
    tdm3
    # Sort by decreasing value of frequency
    tdm_v <- sort(rowSums(tdm_matrix),decreasing=TRUE)
    tdm_df <- data.frame(word = names(tdm_v),freq=tdm_v)
    tdm_df
    
    # w <- rowSums(tdm2)
    # w <- subset(w, w>=25)
    # barplot(w,
    #         las = 2,
    #         col = rainbow(50))
    
    # Display the top 5 most frequent words
    head(tdm_df, 5)
    
    output$myBar1 <- renderPlot({
      # This is the barchart representation of the Top 5 Words most appeared
      barplot(tdm_df[1:5,]$freq, las = 2, names.arg = tdm_df[1:5,]$word,
              col ="red", main ="5 most frequently used words",
              ylab = "Word frequencies")
    },height=400,width=400)
    output$myBar2 <- renderPlot({
      ggplot(emo_sum,aes(x=reorder(emotion,-count),y=count))+
        geom_bar(stat='identity',fill="blue")+
        geom_text(aes(label = emo_sum$count), vjust = 0)
    },height=400,width=500)
    output$myBar4 <- renderPlot({
      par(mfrow=c(2,2))
      barplot(yt_df$jockers, # sentiment
              main = "", # plot title
              xlab = "Jockers") # x-axis label
      barplot(yt_df$nrc, # sentiment
              main = "", # plot title
              xlab = "NRC") # x-axis label
      barplot(yt_df$bing, # sentiment
              main = "", # plot title
              xlab = "Bing") # x-axis label
      barplot(yt_df$afinn, # sentiment
              main = "", # plot title
              xlab = "Afinn") # x-axis label
      
    },height=400,width=500)
    output$myBar3 <- renderPlot({
      set.seed(1234)
      wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 3,
                max.words=200, random.order=FALSE, rot.per=0.40,
                colors=brewer.pal(8, "Dark2"))
      
    },height=700,width=700)
    output$myBar5 <- renderPlot({
      pie(piecount,labels=c("Positive comments","Negative comments"), col = c("green","red"), density = 50, angle = 45)
    },height=400,width=500)
    
    #Word Association for words that occur atleast 10 times
    findAssocs(textcorpus_tdm,terms=findFreqTerms(textcorpus_tdm,lowfreq = 10),corlimit=0.25)
    
    #Sentiment Score
    syuzhet_vector = get_sentiment(yt_df$Comment,method="syuzhet")
    head(syuzhet_vector) 
    #Sum of the sentiment scores of all meaningful words in the first line of
    #comments adds up to 0.5. It ranges from -1(most neg) to +1(most pos)
    
    #summary statistics of the vector
    summary(syuzhet_vector)
    #Median = 0.0, so there are positive as well as negative comments present
    
    
    # bing
    bing_vector = get_sentiment(yt_df$Comment, method="bing")
    
    #Emotion Classification
    d = get_nrc_sentiment(yt_df$Comment)
    # head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
    head (d,10)
    
    yt_df %>% count(yt_df$AuthorDisplayName,sort=TRUE)
    
    #converting dataframe to tibble
    library(tidyverse)
    youtube_tibble = tibble(text=str_to_lower(yt_df$Comment))
    
    
    youtube_tibble.df2 = gsub("#.*","",youtube_tibble$text)
    youtube_tibble.df2 = gsub("@.*","",youtube_tibble$text)
    
    #comments segmentation :0
    
    word.df = as.vector(youtube_tibble.df2)
    emotion.df = get_nrc_sentiment(word.df)
    emotion.df2 = cbind(youtube_tibble.df2, emotion.df) 
    
    sent.value = get_sentiment(word.df)
    most_positive = word.df[sent.value=max(sent.value)]
    
    #most negative comments not working properly
    most_negative = word.df[sent.value=min(sent.value)]
    
    positive_comments = word.df[sent.value>0]
    positive_count = as.character(length(positive_comments))
    negative_comments = word.df[sent.value<0]
    negative_count = as.character(length(negative_comments))
    total_count = (as.numeric(positive_count) + as.numeric(negative_count))
    piecount = c(as.numeric(positive_count),as.numeric(negative_count))
    pie(piecount,labels=c("Positive comments","Negative comments"), col = c("green","red"), density = 50, angle = 45)
    
    yt_df$jockers <- get_sentiment(char_v = yt_df$Comment, # text data
                                   method = "syuzhet") # 'jockers' lexicon
    yt_df$bing <- get_sentiment(char_v = yt_df$Comment, # text data
                                method = "bing") # bing lexicon
    yt_df$afinn <- get_sentiment(char_v = yt_df$Comment, # text data
                                 method = "afinn") # AFINN lexicon
    yt_df$nrc <- get_sentiment(char_v = yt_df$Comment, # text data
                               method = "nrc") # NRC lexicon
    
    
    nrc_emot <- get_nrc_sentiment(char_v = yt_df$Comment)
    colSums(nrc_emot[ ,1:8])
    nrc_emot
    

    sents_sub <- yt_df[ ,(ncol(yt_df)-3):ncol(yt_df)]
    sents_sub
    
    sents_sub[sents_sub == 0] <- 1
    sents_sub <- data.frame(lapply(X = sents_sub, 
                                   FUN = as.factor))
    
    
    
    conf <- confusionMatrix(as.factor(yt_df$bing), # assigned sentiment
                            as.factor(yt_df$nrc), # actual
                            positive = "1",
                            mode = "everything")
    conf
    
    
    #analyze sentiments using syuzhet package based on the NRC sentiment dictionary
    emotions = get_nrc_sentiment(youtube_tibble$text)
    emo_bar = colSums(emotions)
    emo_sum = data.frame(count=emo_bar,emotion=names(emo_bar))
    emo_sum
    
    
    sentiment <- analyzeSentiment(comments)
    sentiment
    
    
    # Extract dictionary-based sentiment according to the QDAP dictionary
    sentiment$SentimentQDAP
    convertToDirection(sentiment$SentimentQDAP)
    
    
    n1 = renderPrint(positive_count)
    output$out_name1 <-  n1
    
    n11=renderPrint(positive_comments)
    output$out_name11 <-n11
    
    n2=renderPrint(negative_count)
    output$out_name2 <-n2
    
    n22=renderPrint(negative_comments)
    output$out_name22 <-n22
    
    n4 = renderPrint(conf)
    output$out_name4 <-n4
    
    n3 = renderTable(tdm3, spacing = 'xs', align = 'l',width='60%')
    output$out_name3 <-n3
    
    
    
  })
  
  #yt_data = read.csv("C:\\Program Files\\Youtube Sentiment Analysis\\youtube_data_2.csv")
  #yt_df = data.frame(yt_data)
  #comments = yt_df$Comment
  #newComments = head(comments,5)
}


shinyApp(ui, server)
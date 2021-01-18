## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(tm)
library(wordcloud)
library(lubridate)

DataFile <- read_csv("Cleaned_Data.csv")
df_unique <- DataFile %>% select(title, url, Category, is_paid, num_subscribers, rating, num_reviews, num_published_lectures, created_date, published_date, price_detail__amount, discount_price__amount, C_Months, P_Months) 
df_unique <- mutate(df_unique, published_year = substr(df_unique$published_date,nchar(df_unique$published_date)-3,nchar(df_unique$published_date)))
df_unique <- mutate(df_unique, created_year = substr(df_unique$created_date,nchar(df_unique$created_date)-3,nchar(df_unique$created_date)))
ud_category <- unique(df_unique$Category)
freq_course <- DataFile %>% group_by(Category) %>%
      summarise(Category_num = n(), .groups = 'drop')

df_unique$published_year <- as.numeric(df_unique$published_year)
freq_course_mean <- DataFile %>% group_by(Category) %>%
  summarise(Category_mean = mean(rating), .groups = 'drop')
freq_course_price <- DataFile %>% group_by(Category) %>%
  summarise(Category_price = mean(price_detail__amount), .groups = 'drop')
freq_course_disc <- DataFile %>% group_by(Category) %>%
  summarise(Category_price = mean(discount_price__amount), .groups = 'drop')

ui <- dashboardPage(
  dashboardHeader(title = "Recommender"),
  
  dashboardSidebar(
    width = 180,
    sidebarMenu(id="tabs",
                menuItem("About", tabName="i_how", icon=icon("question"), selected=TRUE),
                menuItem("Dashboard", tabName="i_dsh", icon=icon("bar-chart")),
                menuItem("Top 10", tabName="i_top", icon=icon("star")),
                menuItem("Appendix", tabName="i_appx", icon=icon("globe")),
                menuItem("Comparison", tabName="i_compare",icon=icon("balance-scale-right"))
    )
    
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "i_how",
            fluidPage(
              titlePanel(h1(strong("Udemy Course Recommender"))),
              sidebarLayout(
                conditionalPanel(
                  condition = NULL),
                  
                mainPanel(h2(strong('About')),
                  h4('This is a Udemy Course Dashboard and Recommender.'),
                  h4('The categories that we provided are Business, Finance, IT-Software and Lifestyle.'),
                  h4('Do you want to take some online course but do not know where to start, what are the choices or want to compare?'),
                  h4('Feel free to try the following functions to explore the 62040 Udemy courses and enjoy!'),
                  br(),
                 p(),
                 h4('How to use this App?'),
                 h4("Dashboard:"),
                  p("Kindly please select the course category to view the dashboard of the category."),
                  br(),
                 h4("Top 10:"),
                 p("Kindly please select the course category and the price to view the TOP 10 Courses recommended."),
                 br(),
                 h4("Appendix: "),
                  p("There are 4 categories of Udemy Courses are available here. User may click on the column name to sort the column."),
                  p("When there is any interested course, you may find more details here! "),
                  br(),
                  h4("Comparison: "),
                  p("When you have some courses in mind but hesitate which should go for? "),
                  p("You may find make comparison for the rating, reviews and price here! "),
                  br(),
                  h5('The dataset: Combination of Business, Finance, IT-Software and Lifestyle dataset obtained from Kaggle.')

                 )
              )
            )
         ),  

    tabItem(tabName = "i_dsh",
            fluidPage(
              titlePanel(h2(strong("Dashboard & Analysis"))),
              sidebarLayout(
                conditionalPanel(
                  condition = NULL,
                  sidebarPanel(
                    width=2.5,
                    selectInput("v_category", "Category", choices = DataFile %>% 
                                  select(Category) %>% 
                                  distinct() %>% 
                                  arrange(Category) %>% 
                                  drop_na())
                    
                  ),
                  sidebarPanel(
                    width = 12,
                    fluidRow(valueBoxOutput("selected_var"),valueBoxOutput("selected_cou"),valueBoxOutput("selected_mean")),
                    br(),
                    fluidRow(box(title = 'Price-Rating Boxplot',plotOutput("price_boxplot"),solidHeader = TRUE),box(title = 'Pie Chart of Paid-Free',plotOutput("pie"),solidHeader = TRUE)),
                    br(),
                    fluidRow(box(title = 'Rating Histogram',plotOutput("rating_hist"),solidHeader = TRUE),box(title = 'Lectures Histogram',plotOutput("lectures_hist"),solidHeader = TRUE)),
                    br(),
                    fluidRow(box(title = 'Price Histogram',plotOutput("price_hist"),solidHeader = TRUE),box(title = 'Discounted Price Histogram',plotOutput("disc_hist"),solidHeader = TRUE)),
                    br(),
                    # THIS FOR LINE PLOT PUBLISHED AND CREATED DATE
                    fluidRow(box(title = 'Published Date Trend',plotOutput("pub_line"),solidHeader = TRUE),box(title = 'Created Date Trend',plotOutput("create_line"),solidHeader = TRUE)),
                    br()
                  )
                  ),
                  mainPanel(title = "Top 5 draft_2", solidHeader = T,
                          width = 8, collapsible = T,
                         plotOutput("macro_plot"))

              )
            )
         ),

          tabItem(tabName = "i_top",
            fluidPage(
              titlePanel(h2(strong("Top 10 Courses Recommendation"))),
              sidebarLayout(
                conditionalPanel(
                  condition = NULL,
                  sidebarPanel(
                    width=3,fluidRow(
                    selectInput("y_category", "1. Category", choices = DataFile %>% 
                                  select(Category) %>% 
                                  distinct() %>% 
                                  arrange(Category) %>% 
                                  drop_na()),sliderInput(
                                    'price_t','2. Price',min =0,max = 800,value=c(0,800),step = 1
                                  ))
                  ),
                  sidebarPanel(
                    width = 9,
                    fluidRow(valueBoxOutput("selected_var2"), valueBoxOutput("selected_price")),
                    br(),
                    fluidRow(plotOutput("paid_best_rating_course"),plotOutput("free_best_rating_course")),
                    br(),
                    fluidRow(plotOutput("paid_best_subscribed_course"),plotOutput("free_best_subscribed_course")),
                    br()
                  )
                  ),
                 mainPanel(title = "Top 5 draft", solidHeader = T,
                           width = 8, collapsible = T,
                           plotOutput("macro_plot2"))
              )
            )
         ),


      tabItem(tabName = "i_appx",
              fluidPage(
                
                titlePanel(h2(strong("Appendix"))),
                sidebarLayout(
                  conditionalPanel(
                    condition = NULL,
                    sidebarPanel(
                      width = 3,
                      helpText("Please select the category and year to see list of Udemy data"),
                      selectInput(
                        'category_id', '1. Category',ud_category),
                      sliderInput(
                        'year_s','2. Year',min = 2010,max = 2020,value=c(2010,2020),step = 1
                      )
                    )),
                  mainPanel(dataTableOutput("newtbl"))
              )
          )
        ),
    
      # Tab Comparison
      tabItem(tabName = "i_compare",
              fluidPage(
                titlePanel(h2(strong("Comparison"))),
                fluidRow(box(
                    column(6, "Course 1",
                           helpText("Please select the category and year to see list of Udemy Courses"),
                           selectInput(
                             'category_idC1', '1. Category',ud_category),
                           selectInput(
                             'year_sC1','2. Year',choices=NULL),
                           selectInput('c1name','3. Course 1 Name',choices=NULL),
                           br(),br(),
                           p("Ratings: "),
                           textOutput("c1ratings"),
                           p("No. of Reviews: "),
                           textOutput("c1reviews"),
                           p("No of Subscribers: "),
                           textOutput("c1subscribers"),
                           p("No. of Lectures: "),
                           textOutput("c1Lectures"),
                           p("Price: "),
                           textOutput("c1price")
                           
                           )),
                    box(column(6, "Course 2",
                           helpText("Please select the category and year to see list of Udemy Courses"),
                           selectInput(
                             'category_idC2', '1. Category',ud_category),
                           selectInput(
                             'year_sC2','2. Year',choices = NULL),
                           selectInput('c2name','3. Course 2 Name',choices=NULL),
                           br(),br(),
                           p("Ratings: "),
                           textOutput("c2ratings"),
                           p("No. of Reviews: "),
                           textOutput("c2reviews"),
                           p("No of Subscribers: "),
                           textOutput("c2subscribers"),
                           p("No. of Lectures: "),
                           textOutput("c2Lectures"),
                           p("Price: "),
                           textOutput("c2price")                           
                           
                           ))
                         
                )
                
                
                
                
              )
            
              
            
        )

    
      
      )
    )
  )

server <- function(input, output,session) { 
  
  range_p<-reactive({input$price_t[1]})
  range_p2<-reactive({input$price_t[2]})
  
  output$paid_best_rating_course <- renderPlot({
    
    course_bestrat <- DataFile %>% select(title, rating, Category, is_paid,price_detail__amount) %>% filter(Category == input$y_category) %>% filter(is_paid == 'TRUE') %>% filter(price_detail__amount >= input$price_t[1]) %>% filter(price_detail__amount <= input$price_t[2])
    course_bestrat_top <- course_bestrat[order(course_bestrat$rating),]
    course_bestrat_top <- tail(course_bestrat_top, 12)
    course_bestrat_top <- head(course_bestrat_top, 10)
    r <- c()
    for(i in 10:1){r <- c(r, i)}
    row.names(course_bestrat_top) <- r
    course_bestrat_top
    options(repr.plot.width=15, repr.plot.height=11)
    plot3 <- ggplot(data = course_bestrat_top, mapping = aes(x = reorder(title, -rating), y = rating)) +
      geom_bar(stat = "identity", mapping = aes(fill = Category, color = Category), alpha = .8, size = 1.5) +
      coord_flip() +
      geom_label(mapping = aes(label = round(rating, 2)), size = 5, fill = "#F5FFFA", fontface = "bold") + ggtitle("Top 10 best rated paid courses")
    plot3
  })
  
  output$free_best_rating_course <- renderPlot({
    validate(
      need(input$y_category != 'Business', "Business does not have any free course. Please select other category for more free courses"),
      need(input$y_category != 'IT_Software', "IT_Software does not have any free course. Please select other category for more free courses.")
    )
    course_bestrat3 <- DataFile %>% select(title, rating, Category, is_paid) %>% filter(Category == input$y_category) %>% filter(is_paid == 'FALSE')
    course_bestrat_top3 <- course_bestrat3[order(course_bestrat3$rating),]
    course_bestrat_top3 <- tail(course_bestrat_top3, 12)
    course_bestrat_top3 <- head(course_bestrat_top3, 10)
    r <- c()
    for(i in 10:1){r <- c(r, i)}
    row.names(course_bestrat_top3) <- r
    course_bestrat_top3
    options(repr.plot.width=10, repr.plot.height=7)
    plot3 <- ggplot(data = course_bestrat_top3, mapping = aes(x = reorder(title, -rating), y = rating)) +
      geom_bar(stat = "identity", mapping = aes(fill = Category, color = Category), alpha = .6, size = 1) +
      coord_flip() +
      geom_label(mapping = aes(label = round(rating, 2)), size = 5, fill = "#F5FFFA", fontface = "bold") + ggtitle("Top 10 best rated free courses")
    plot3
  })
  
  output$paid_best_subscribed_course <- renderPlot({
    
    course_bestrat2 <- DataFile %>% select(title, num_subscribers, Category, is_paid,is_paid,price_detail__amount) %>% filter(Category == input$y_category) %>% filter(is_paid == 'TRUE') %>% filter(price_detail__amount >= input$price_t[1]) %>% filter(price_detail__amount <= input$price_t[2])
    course_bestrat_top2 <- course_bestrat2[order(course_bestrat2$num_subscribers),]
    course_bestrat_top2 <- tail(course_bestrat_top2, 12)
    course_bestrat_top2 <- head(course_bestrat_top2, 10)
    r <- c()
    for(i in 10:1){r <- c(r, i)}
    row.names(course_bestrat_top2) <- r
    course_bestrat_top2
    options(repr.plot.width=10, repr.plot.height=7)
    plot3 <- ggplot(data = course_bestrat_top2, mapping = aes(x = reorder(title, -num_subscribers), y = num_subscribers)) +
      geom_bar(stat = "identity", mapping = aes(fill = Category, color = Category), alpha = .6, size = 1) +
      coord_flip() +
      geom_label(mapping = aes(label = round(num_subscribers, 1)), size = 5, fill = "#F5FFFA", fontface = "bold") + ggtitle("Top 10 best subscribed paid courses")
    plot3
  })
  
 output$free_best_subscribed_course <- renderPlot({
   validate(
      need(input$y_category != 'Business', "Business does not have any free course. Please select other category for more free courses"),
      need(input$y_category != 'IT_Software', "IT_Software does not have any free course. Please select other category for more free courses.")
    )
    course_bestrat4 <- DataFile %>% select(title, num_subscribers, Category, is_paid) %>% filter(Category == input$y_category) %>% filter(is_paid == 'FALSE')
    course_bestrat_top4 <- course_bestrat4[order(course_bestrat4$num_subscribers),]
    course_bestrat_top4 <- tail(course_bestrat_top4, 12)
    course_bestrat_top4 <- head(course_bestrat_top4, 10)
    r <- c()
    for(i in 10:1){r <- c(r, i)}
    row.names(course_bestrat_top4) <- r
    options(repr.plot.width=10, repr.plot.height=7)
    plot3 <- ggplot(data = course_bestrat_top4, mapping = aes(x = reorder(title, -num_subscribers), y = num_subscribers)) +
      geom_bar(stat = "identity", mapping = aes(fill = Category, color = Category), alpha = .6, size = 1) +
      coord_flip() +
      geom_label(mapping = aes(label = round(num_subscribers, 1)), size = 5, fill = "#F5FFFA", fontface = "bold") + ggtitle("Top 10 best subscribed Free courses")
    plot3
  }) 

  # output$word_cloud <- renderPlot ({
  #      if(input$v_category == 'Lifestyle'){
  
  # freq_course_word <- df_unique %>% filter(Category == input$v_category) %>% filter(published_year > 2017)}else{
  # freq_course_word <- df_unique %>% filter(Category == input$v_category)
    
  # }
    
  # text <- freq_course_word$title
  # docs <- Corpus(VectorSource(text))
  # docs <- docs %>%
  # tm_map(removeNumbers) %>%
  # tm_map(removePunctuation) %>%
  # tm_map(stripWhitespace)
  # docs <- tm_map(docs, content_transformer(tolower))
  # docs <- tm_map(docs, removeWords, stopwords("english"))
  # dtm <- TermDocumentMatrix(docs) 
  # matrix <- as.matrix(dtm) 
  # words <- sort(rowSums(matrix),decreasing=TRUE) 
  # df <- data.frame(word = names(words),freq=words)
  # set.seed(1234) # for reproducibility 
  # wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=100, random.order=FALSE,rot.per=0.1 ,colors=brewer.pal(8, "Dark2"))
  # })
  
  output$rating_hist <- renderPlot ({
  
  freq_course_hist <- df_unique %>% filter(Category == input$v_category)
  ggplot(data=freq_course_hist, aes(x=freq_course_hist$rating)) + 
  geom_histogram(breaks=seq(0, 5, by=0.5),
                 fill = "blue") + 
  labs(title="Histogram for Rating", x="Rating", y="Count") + 
  xlim(c(0,5))
  })

  output$price_hist <- renderPlot ({
  
  freq_course_hist2 <- df_unique %>% filter(Category == input$v_category)
  ggplot(data=freq_course_hist2, aes(x=freq_course_hist2$price_detail__amount)) + 
  geom_histogram(breaks=seq(0, 800, by=100),
                 fill = "blue") + 
  labs(title="Histogram for Price", x="Price (MYR)", y="Count") + 
  xlim(c(0,800))
  })

  output$disc_hist <- renderPlot ({
  
  freq_course_hist_d <- df_unique %>% filter(Category == input$v_category)
  ggplot(data= freq_course_hist_d, aes(x= freq_course_hist_d$discount_price__amount)) + 
  geom_histogram(breaks=seq(0, 800, by=100),
                 fill = "blue") + 
  labs(title="Histogram for Discounted Price", x="Price (MYR)", y="Count") + 
  xlim(c(0,800))
  })

   output$price_boxplot <- renderPlot ({
  
   freq_course_boxp <- df_unique %>% filter(Category == input$v_category)
   freqboxp <- freq_course_boxp %>% mutate(bin = cut_width(rating,width=1.0,boundary = 0))
   ggplot(data = freq_course_boxp, aes(x=freqboxp$bin, y=freq_course_boxp$price_detail__amount)) +
   geom_boxplot(fill="#69b3a2") + 
   labs(title="Price-Rating Boxplot", x="Rating", y="Price")
   })

  output$lectures_hist <- renderPlot ({
  
  freq_course_hist3 <- df_unique %>% filter(Category == input$v_category)
  ggplot(data=freq_course_hist3, aes(x=freq_course_hist3$num_published_lectures)) + 
  geom_histogram(breaks=seq(0, 900, by=100),
                 fill = "blue") + 
  labs(title="Histogram for Lectures", x="Lectures", y="Count") + 
  xlim(c(0,900))
  })
  
  range_y<-reactive({input$year_s[1]})
  range_y2<-reactive({input$year_s[2]})
     output$pub_line <- renderPlot({
    
  freq_pubdate <- df_unique %>% filter(Category == input$v_category)
  freqlinepub <- freq_pubdate %>%
            #mutate(published_date = as.date(published_date)) %>%     # update to a datetime variable (if needed)
            group_by(P_Months,    # for each month and year
                     published_year) %>%
            mutate(N = n()) %>% # count number of rows/appearances
            unite(published_date, c(P_Months,published_year),sep = "-", remove = FALSE) %>%
            ungroup() %>%                    # forget the grouping
            select(-P_Months, -published_year, published_date) %>% # Remove help variables
            mutate(published_date = my(published_date)) %>% #Changed it back to Date
            distinct(published_date, .keep_all = TRUE)
    ggplot(data=freqlinepub, aes(x=published_date,y=N)) + geom_line()
    
    
  })
  
  output$create_line <- renderPlot({
    
    freq_credate <- df_unique %>% filter(Category == input$v_category)
    freqlinecre <- freq_credate %>%
              #mutate(created_date = ymd(`created_date`)) %>%     # update to a datetime variable (if needed)
              group_by(C_Months,    # for each month and year
                       created_year) %>%
              mutate(N = n()) %>% # count number of rows/appearances
              unite(created_date, c(C_Months, created_year),sep = "-", remove = FALSE) %>%
              ungroup() %>%                    # forget the grouping
              select(-C_Months, -created_year, created_date) %>% # Remove help variables
              mutate(created_date = my(`created_date`)) %>% #Changed it back to Date
              distinct(created_date, .keep_all = TRUE)
    ggplot(data=freqlinecre, aes(x=created_date,y=N)) + geom_line()
  }) 
  
  
  output$pie <- renderPlot({
    
    freq_course_paid <- DataFile %>% filter(Category == input$v_category) %>% group_by(is_paid) %>%
      summarise(Category_paid = n(), .groups = 'drop')
    
    ggplot( freq_course_paid, aes(x="", y=Category_paid, fill=is_paid)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0)+theme_void() + geom_text(aes(label = paste(round(Category_paid / sum(Category_paid) * 100, 1), "%")),colour = 'white',position = position_stack(vjust = 0.5))+ggtitle("Pie chart of Paid-Free")

  })

  output$newtbl <- renderDataTable({
    newdata <- df_unique %>% filter (Category == input$category_id)  %>% filter (published_year >= input$year_s[1] ) %>% filter (published_year <= input$year_s[2] )
    
    opts <- list(pageLength = 5, lengthChange = TRUE, searching = TRUE,
                 info = FALSE,  pagingType = "full_numbers", scrollX='600px')
    
    datatable(newdata, escape = FALSE, rownames = FALSE, options = opts)
  })
  
  output$selected_var <- renderValueBox({ 

    valueBox(
    input$v_category,"is Selected.",icon = icon("education", lib = "glyphicon"),
    color = "purple"
    )
  })

  output$selected_var2 <- renderValueBox({ 

    valueBox(
    input$y_category,"is Selected.",icon = icon("education", lib = "glyphicon"),
    color = "purple"
    )
  })
  
  output$selected_cou <- renderValueBox({ 
    freq_course %>% filter(Category == input$v_category)
    
    a_course <- freq_course$Category_num[freq_course$Category == input$v_category]
    a_course <- as.numeric(a_course)
    valueBox(
       a_course," courses are available.",
      color = "blue"
    )
  })
  
  output$selected_mean <- renderValueBox({ 
    freq_course_mean %>% filter(Category == input$v_category)
    
    a_course_mean <- freq_course_mean$Category_mean[freq_course_mean$Category == input$v_category]
    a_course_mean <- as.numeric(a_course_mean)
    valueBox(
      round(a_course_mean,2)," Average Rating",icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$selected_price <- renderValueBox({ 
    freq_course_price %>% filter(Category == input$y_category)
    
    a_course_price <- freq_course_price$Category_price[freq_course_price$Category == input$y_category]
    a_course_price <- as.numeric(a_course_price)
    valueBox(
     paste('RM ', round(a_course_price,2))," Average Price",icon = icon("shopping-cart", lib = "glyphicon"),
      color = "green"
    )
  })
  
  # For Comparison Page
  
  #output$text <- renderText({ "HEHE" })
  
  # Update Course 1 Selections
  observeEvent(input$category_idC1,{
    updateSelectInput(session,'year_sC1',
                      choices = unique(sort(df_unique$published_year[df_unique$Category==input$category_idC1])))
  })
  observeEvent(input$year_sC1,{
    updateSelectInput(session,'c1name',
                      choices = unique(sort(df_unique$title[df_unique$Category==input$category_idC1 & df_unique$published_year==input$year_sC1])))
  })

  
  # Update Course 2 Selections
  observeEvent(input$category_idC2,{
    updateSelectInput(session,'year_sC2',
                      choices = unique(sort(df_unique$published_year[df_unique$Category==input$category_idC2])))
  })
  observeEvent(input$year_sC2,{
    updateSelectInput(session,'c2name',
                      choices = unique(sort(df_unique$title[df_unique$Category==input$category_idC2 & df_unique$published_year==input$year_sC2])))
  })
  
  #
  #output$c1 <- renderValueBox({ 
    
   # valueBox(
    #  input$year_sC1,"is Selected.",icon = icon("education", lib = "glyphicon"),
    #  color = "purple"
  #  )
 # })
  
  #Output for Comparison page
  
  output$c1ratings <- renderText({df_unique$rating[df_unique$title==input$c1name]})
  output$c1reviews <- renderText({df_unique$num_reviews[df_unique$title==input$c1name]})
  output$c1subscribers <- renderText({df_unique$num_subscribers[df_unique$title==input$c1name]})
  output$c1Lectures <- renderText({df_unique$num_published_lectures[df_unique$title==input$c1name]})
  output$c1price <- renderText({df_unique$price_detail__amount[df_unique$title==input$c1name]})
  
  
  output$c2ratings <- renderText({df_unique$rating[df_unique$title==input$c2name]})
  output$c2reviews <- renderText({df_unique$num_reviews[df_unique$title==input$c2name]})
  output$c2subscribers <- renderText({df_unique$num_subscribers[df_unique$title==input$c2name]})
  output$c2Lectures <- renderText({df_unique$num_published_lectures[df_unique$title==input$c2name]})
  output$c2price <- renderText({df_unique$price_detail__amount[df_unique$title==input$c2name]})
  
}

shinyApp(ui, server)
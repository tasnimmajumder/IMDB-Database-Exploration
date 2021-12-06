
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(dplyr)

imdb<- read.delim("data/imdb dataset")

# Define UI for application that explore IMDB database
ui <- fluidPage(

    navbarPage(
        theme= shinytheme("flatly"),
        "IMDB Data Exploration",
        #Tab 1      
        tabPanel("Descriptive Statistics",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("Function",label="Choose a Function",choice=c("Mean" = "mean",
                                                                                          "Median" = "median",
                                                                                          "Standard Deviation" = "standard deviation",
                                                                                          "Minimun" = "minimum",
                                                                                          "Maximum" = "maximum",
                                                                                          "Summary Table" = "summary table"), selectize=FALSE),
                            selectInput("variable",label="Choose a variable",choice=c("Title Type" = "Title Type",
                                                                                      "Is Adult" = "Is Adult",
                                                                                      "Genre" = "Genre",
                                                                                      "Start year" = "Start Year",
                                                                                      "Running Time" = "Runtime in Minutes",
                                                                                      "Average Rating" = "Average Rating",
                                                                                      "Number of Votes" = "Number of Votes"), selectize=FALSE)
                            ),
               mainPanel( textOutput("selected_function"),
                          verbatimTextOutput("summ1"))
                        )),
       #Tab 2 
        tabPanel("Visualization",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("y",label="Select Continuous Variable",choice=c("Start Year" = "startYear",
                                                                         "Running Time" = "runtimeMinutes",
                                                                         "Average Rating" = "averageRating",
                                                                         "Number of Votes" = "numVotes"), selectize=FALSE),
                                
                                selectInput("x",label="Select Categorical Variable",choice=c("Title Type" = "titleType",
                                                                         "Is Adult" = "isAdult",
                                                                         "Genre" = "main.genre"), selectize=FALSE),
                                selectInput("z",label="Select Variable",choice=c("Title Type" = "titleType",
                                                                           "Start Year" = "startYear",
                                                                           "Is Adult" = "isAdult",
                                                                           "Genre" = "main.genre",
                                                                           "Running Time" = "runtimeMinutes",
                                                                           "Average Rating" = "averageRating",
                                                                           "Number of Votes" = "numVotes"), selectize=FALSE)
                            ),
                            mainPanel(
                                plotOutput("boxplot")
                            )
                        )),
    # Tab 3   
       navbarMenu("Statistical Analysis",
                          tabPanel("Correlation Test",
                                   sidebarLayout(
                                       sidebarPanel(
                                           selectInput("var1",label="Select Variable",choice=c("Running Time" = "runtimeMinutes",
                                                                                               "Average Rating" = "averageRating",
                                                                                               "Number of Votes" = "numVotes"), selectize=FALSE),
                                           
                                           selectInput("var2",label="Select Variable",choice=c("Running Time" = "runtimeMinutes",
                                                                                               "Average Rating" = "averageRating",
                                                                                               "Number of Votes" = "numVotes"), selectize=FALSE)
                                       ),
                                       mainPanel (verbatimTextOutput("cor"),
                                                  textOutput("result"))
                                   )),
                          tabPanel("ANOVA",
                                   sidebarLayout(
                                       sidebarPanel(
                                           selectInput("var3",label="Select Variable",choice=c("Running Time" = "runtimeMinutes",
                                                                                               "Average Rating" = "averageRating",
                                                                                               "Number of Votes" = "numVotes"), selectize=FALSE),
                                           
                                           selectInput("var4",label="Select Variable",choice=c("Title Type" = "titleType",
                                                                                               "Genre" = "main.genre",
                                                                                               "Is Adult" = "isAdult"), selectize=FALSE)
                                       ),
                                       mainPanel(verbatimTextOutput("anova"),
                                                 textOutput("result1"))
                                   ))
                          )
               )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$selected_function<- renderText({
        paste(input$Function, "of", input$variable)
        })   
    
    output$summ1<- renderPrint({
        if (input$Function == "mean")
        {if(input$variable == "Runtime in Minutes"){mean(imdb$runtimeMinutes)}
            else if (input$variable == "Start Year"){mean(imdb$startYear)}
            else if (input$variable == "Average Rating"){mean(imdb$averageRating)}
            else if (input$variable == "Number of Votes"){mean(imdb$numVotes)}}
        else if (input$Function == "median")
        {if(input$variable == "Runtime in Minutes"){median(imdb$runtimeMinutes)}
            else if (input$variable == "Start Year"){median(imdb$startYear)}
            else if (input$variable == "Average Rating"){median(imdb$averageRating)}
            else if (input$variable == "Number of Votes"){median(imdb$numVotes)}}
        else if (input$Function == "standard deviation")
        {if(input$variable == "Runtime in Minutes"){sd(imdb$runtimeMinutes)}
            else if (input$variable == "Start Year"){sd(imdb$startYear)}
            else if (input$variable == "Average Rating"){sd(imdb$averageRating)}
            else if (input$variable == "Number of Votes"){sd(imdb$numVotes)}}
        else if (input$Function == "minimum")
        {if(input$variable == "Runtime in Minutes"){min(imdb$runtimeMinutes)}
            else if (input$variable == "Start Year"){min(imdb$startYear)}
            else if (input$variable == "Average Rating"){min(imdb$averageRating)}
            else if (input$variable == "Number of Votes"){min(imdb$numVotes)}}
        else if (input$Function == "maximum")
        {if(input$variable == "Runtime in Minutes"){max(imdb$runtimeMinutes)}
            else if (input$variable == "Start Year"){max(imdb$startYear)}
            else if (input$variable == "Average Rating"){max(imdb$averageRating)}
            else if (input$variable == "Number of Votes"){max(imdb$numVotes)}}
        else if (input$Function == "summary table")
        {if(input$variable == "Title Type"){table(imdb$titleType)}
            else if (input$variable == "Is Adult"){table(imdb$isAdult)}
            else if (input$variable == "Genre"){table(imdb$main.genre)}}
    })
    
    
    output$boxplot <- renderPlot({
        if(input$x=="titleType")
        {if(input$y == "startYear"){
            if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = titleType))+
                    geom_boxplot()}
            else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = startYear))+
                    geom_boxplot()}
            else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = isAdult))+
                    geom_boxplot()}
            else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = numVotes))+
                    geom_point()}
            else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = main.genre))+
                    geom_boxplot()}
            else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = averageRating))+
                    geom_point()}
            else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = runtimeMinutes))+
                    geom_point()}}
            else if (input$y == "runtimeMinutes")
            { if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = runtimeMinutes, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = runtimeMinutes))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = runtimeMinutes, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = numVotes, color = titleType))+
                        geom_smooth(method=lm)}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = runtimeMinutes, color = main.genre))+
                        geom_point()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = averageRating, color = titleType))+
                        geom_smooth(method=lm)}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = runtimeMinutes, color = runtimeMinutes))+
                        geom_boxplot()}}
            else if (input$y == "averageRating")
            {if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = averageRating, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = averageRating))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = averageRating, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=numVotes, y = averageRating, color = titleType))+
                        geom_smooth(method=lm)}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = averageRating, color = main.genre))+
                        geom_boxplot()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = averageRating, color = averageRating))+
                        geom_boxplot()}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = averageRating, color = titleType))+
                        geom_smooth(method=lm)}}
            else if (input$y == "numVotes") 
            {if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = numVotes, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = startYear, color = numVotes))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = numVotes, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = numVotes, color = numVotes))+
                        geom_boxplot()}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=titleType, y = numVotes, color = main.genre))+
                        geom_boxplot()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=numVotes, y = averageRating, color = titleType))+
                        geom_smooth(method=lm)}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = numVotes, color = titleType))+
                        geom_smooth(method=lm)}}}
        else if (input$x == "main.genre")
        {if(input$y == "startYear"){
            if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = titleType))+
                    geom_boxplot()}
            else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = startYear))+
                    geom_boxplot()}
            else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = isAdult))+
                    geom_boxplot()}
            else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = numVotes))+
                    geom_point()}
            else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = main.genre))+
                    geom_boxplot()}
            else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = averageRating))+
                    geom_point()}
            else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = runtimeMinutes))+
                    geom_point()}}
            else if (input$y == "runtimeMinutes")
            { if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = runtimeMinutes, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = runtimeMinutes))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = runtimeMinutes, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = numVotes, color = main.genre))+
                        geom_smooth(method=lm)}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = runtimeMinutes, color = main.genre))+
                        geom_boxplot()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = averageRating, color = main.genre))+
                        geom_smooth(method=lm)}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = runtimeMinutes, color = runtimeMinutes))+
                        geom_boxplot()}}
            else if (input$y == "averageRating")
            {if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = averageRating, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = averageRating))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = averageRating, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=numVotes, y = averageRating, color = main.genre))+
                        geom_smooth(method=lm)}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = averageRating, color = main.genre))+
                        geom_boxplot()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = averageRating, color = averageRating))+
                        geom_boxplot()}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = averageRating, color = main.genre))+
                        geom_smooth(method=lm)}}
            else if (input$y == "numVotes") 
            {if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = numVotes, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = startYear, color = numVotes))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = numVotes, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = numVotes, color = numVotes))+
                        geom_boxplot()}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=main.genre, y = numVotes, color = main.genre))+
                        geom_boxplot()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=numVotes, y = averageRating, color = main.genre))+
                        geom_smooth(method=lm)}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = numVotes, color = main.genre))+
                        geom_smooth(method=lm)}}}
        else if (input$x == "isAdult")
        {if(input$y == "startYear"){
            if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = titleType))+
                    geom_boxplot()}
            else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = startYear))+
                    geom_boxplot()}
            else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = isAdult))+
                    geom_boxplot()}
            else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = numVotes))+
                    geom_point()}
            else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = main.genre))+
                    geom_boxplot()}
            else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = averageRating))+
                    geom_point()}
            else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = runtimeMinutes))+
                    geom_point()}}
            else if (input$y == "runtimeMinutes")
            { if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = runtimeMinutes, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = runtimeMinutes))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = runtimeMinutes, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = numVotes, color = isAdult))+
                        geom_smooth(method=lm)}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = runtimeMinutes, color = main.genre))+
                        geom_boxplot()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y =averageRating, color = isAdult ))+
                        geom_smooth(method=lm)}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = runtimeMinutes, color = runtimeMinutes))+
                        geom_boxplot()}}
            else if (input$y == "averageRating")
            {if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = averageRating, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = averageRating))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = averageRating, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=numVotes, y = averageRating, color = isAdult))+
                        geom_smooth(method=lm)}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = averageRating, color = main.genre))+
                        geom_boxplot()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = averageRating, color = averageRating))+
                        geom_boxplot()}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = averageRating, color = isAdult))+
                        geom_smooth(method=lm)}}
            else if (input$y == "numVotes") 
            {if(input$z == "titleType"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = numVotes, color = titleType))+
                    geom_boxplot()}
                else if (input$z == "startYear"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = startYear, color = numVotes))+
                        geom_point()}
                else if (input$z == "isAdult"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = numVotes, color = isAdult))+
                        geom_boxplot()}
                else if (input$z == "numVotes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = numVotes, color = numVotes))+
                        geom_boxplot()}
                else if (input$z == "main.genre"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=isAdult, y = numVotes, color = main.genre))+
                        geom_boxplot()}
                else if (input$z == "averageRating"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=numVotes, y = averageRating, color = isAdult))+
                        geom_smooth(method=lm)}
                else if (input$z == "runtimeMinutes"){ggplot(filter(imdb, runtimeMinutes<300),  aes(x=runtimeMinutes, y = numVotes, color = isAdult))+
                        geom_smooth(method=lm)}}}
    })
    
    output$cor <- renderPrint({
        imdb1<- filter(imdb, runtimeMinutes<300)
        if(input$var1=="runtimeMinutes")
        {if(input$var2 == "numVotes"){cor.test(imdb1$runtimeMinutes, imdb1$numVotes)}
            else if (input$var2 == "averageRating") {cor.test(imdb1$runtimeMinutes, imdb1$averageRating)}}
        else if (input$var1 == "averageRating") 
        {if(input$var2 == "numVotes"){cor.test(imdb1$averageRating, imdb1$numVotes)}
            else if (input$var2 == "runtimeMinutes") {cor.test( imdb1$averageRating, imdb1$runtimeMinutes)}}
        else if (input$var1 == "numVotes") 
        {if(input$var2 == "averageRating"){cor.test(imdb1$numVotes, imdb1$averageRating)}
            else if (input$var2 == "runtimeMinutes") {cor.test(imdb1$numVotes,imdb1$runtimeMinutes)}}
        
    })
    
    output$result<- renderText({
        if(input$var1=="runtimeMinutes")
        {if(input$var2 == "numVotes"){if(cor.test(imdb$runtimeMinutes, imdb$numVotes)$p.value<0.05){"This p-value is less than 0.05. So correlation value between these two variables is statistically significant."} else{"The coreelation value is not statistically significant"}}
            else if (input$var2 == "averageRating"){if(cor.test(imdb$runtimeMinutes, imdb$averageRating)$p.value<0.05){"This p-value is less than 0.05. So correlation value between these two variables is statistically significant."} else{"The coreelation value is not statistically significant"}}}
        else if (input$var1 == "averageRating") 
        {if(input$var2 == "numVotes"){if(cor.test(imdb$averageRating, imdb$numVotes)$p.value<0.05){"This p-value is less than 0.05. So correlation value between these two variables is statistically significant."} else{"The coreelation value is not statistically significant"}}
            else if (input$var2 == "runtimeMinutes"){if(cor.test(imdb$runtimeMinutes, imdb$averageRating)$p.value<0.05){"This p-value is less than 0.05. So correlation value between these two variables is statistically significant."} else{"The coreelation value is not statistically significant"}}}
        else if (input$var1 == "numVotes") 
        {if(input$var2 == "averageRating"){if(cor.test(imdb$averageRating, imdb$numVotes)$p.value<0.05){"This p-value is less than 0.05. So correlation value between these two variables is statistically significant."} else{"The coreelation value is not statistically significant"}}
            else if (input$var2 == "runtimeMinutes"){if(cor.test(imdb$runtimeMinutes, imdb$numVotes)$p.value<0.05){"This p-value is less than 0.05. So correlation value between these two variables is statistically significant."} else{"The coreelation value is not statistically significant"}}}
    })
    
    output$anova <- renderPrint({
        imdb1<- filter(imdb, runtimeMinutes<300)
        if(input$var3=="runtimeMinutes")
        {if(input$var4 == "titleType"){summary(aov(imdb1$runtimeMinutes~imdb1$titleType))}
            else if (input$var4 == "main.genre") {summary(aov(imdb1$runtimeMinutes~imdb1$main.genre))}
            else if (input$var4 == "isAdult"){summary(aov(imdb1$runtimeMinutes~imdb1$isAdult))}}
        else if (input$var3 == "averageRating") 
        {if(input$var4 == "titleType"){summary(aov(imdb1$averageRating~imdb1$titleType))}
            else if (input$var4 == "main.genre") {summary(aov(imdb1$averageRating~imdb1$main.genre))}
            else if (input$var4 == "isAdult"){summary(aov(imdb1$averageRating~imdb1$isAdult))}}
        else if (input$var3 == "numVotes") 
        {if(input$var4 == "titleType"){summary(aov(imdb1$numVotes~imdb1$titleType))}
            else if (input$var4 == "main.genre") {summary(aov(imdb1$numVotes~imdb1$main.genre))}
            else if (input$var4 == "isAdult"){summary(aov(imdb1$numVotes~imdb1$isAdult))}}
        
    })
    
    output$result1<- renderText({
        imdb1<- filter(imdb, runtimeMinutes<300)
        if(input$var3=="runtimeMinutes")
        {if(input$var4 == "titleType"){if(summary(aov(imdb1$runtimeMinutes~imdb1$titleType))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for each tittle type is statistically significant."} else{"The average running time for all title type is equal"}}
            else if (input$var4 == "main.genre"){if(summary(aov(imdb1$runtimeMinutes~imdb1$main.genre))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for each genre is statistically significant."} else{"The average running time for all genre is equal"}}
            else if (input$var4 == "isAdult"){if(summary(aov(imdb1$runtimeMinutes~imdb1$isAdult))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for all categories in isAdult variable is statistically significant."} else{"The average running time for all categories in isAdult variable is equal"}}}
        else if (input$var3 == "averageRating") 
        {if(input$var4 == "titleType"){if(summary(aov(imdb1$averageRating~imdb1$titleType))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for each tittle type is statistically significant."} else{"The average running time for all title type is equal"}}
            else if (input$var4 == "main.genre"){if(summary(aov(imdb1$averageRating~imdb1$main.genre))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for each genre is statistically significant."} else{"The average running time for all genre is equal"}}
            else if (input$var4 == "isAdult"){if(summary(aov(imdb1$averageRating~imdb1$isAdult))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for all categories in isAdult variable is statistically significant."} else{"The average running time for all categories in isAdult variable is equal"}}}
        else if (input$var3 == "numVotes") 
        {if(input$var4 == "titleType"){if(summary(aov(imdb1$numVotes~imdb1$titleType))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for each tittle type is statistically significant."} else{"The average running time for all title type is equal"}}
            else if (input$var4 == "main.genre"){if(summary(aov(imdb1$numVotes~imdb1$main.genre))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for each genre is statistically significant."} else{"The average running time for all genre is equal"}}
            else if (input$var4 == "isAdult"){if(summary(aov(imdb1$numVotes~imdb1$isAdult))[[1]][1,5]<0.05){"This p-value is less than 0.05. So difference in average runiing time for all categories in isAdult variable is statistically significant."} else{"The average running time for all categories in isAdult variable is equal"}}}
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

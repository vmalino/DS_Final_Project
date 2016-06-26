library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Next Word Prediction"),
        sidebarPanel(
                textInput("inp", label = h3("Enter text here:")),
                checkboxGroupInput("corps", label = h3("Select corpus to use:"),
                                   choices = list("Blogs" = 1,
                                                  "News" = 2,
                                                  "Twitter" = 3),
                                   selected = c(1, 2, 3))
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("Result", h2(htmlOutput("pred"))),
                        tabPanel("About", h3("DS Final Project"),
                                 p("author: vmalino"),
                                 p("Enter phrase to get next word prediction"),
                                 p("For more details:"),
                                 a("RPbubs", href = "http://rpubs.com/vmalino/wordpred"))
                )
                
        )
))
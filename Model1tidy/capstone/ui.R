
library(shiny)

shinyUI(fluidPage(
    HTML("<div class='hero-unit'>"),
    fluidRow(
        column(12,
               h2("Coursera Data Science Capstone Project"),
               h6("Anna Witkowiak")
        )
    ),
    HTML("</div>"),
    textInput("sentence", "Please type a sentence:"),
    uiOutput("uiOutputPanel"),
    h6(textOutput("sentenceEntered", container=span)),
    plotOutput("wordPlot")
))

library(shiny)
library(shinysky)
library(shinyBS)
library(ggplot2)
library(grid)
library(reshape2)
library(rJava)
library(xlsx)
options(shiny.maxRequestSize=30*1024^2)
options(java.parameters = "-Xmx8000m")
h1("Mice Data Basic Analysis")

shinyUI(navbarPage(img(src="mouse1.png",width=260,height=60),
                   id = "datatabs",
                   collapsable = T,
                   responsive = T,
                   tabPanel(h4("Basic Analysis"),
                            busyIndicator(wait = 0),
                            uiOutput("basic")),
                   tabPanel(h4("Metabolic Cage"),
                            busyIndicator(wait = 0),
                            uiOutput("metabolic1")),
                   tabPanel(h4("GTT/ITT"),
                            busyIndicator(wait = 0),
                            uiOutput("TT1")),
                   tabPanel(h4("Sample Images"),
                            mainPanel(width=22,
                              h3("1. Basic Analysis:"),
                              list(img(src="ba1.png",width=600,height=250),img(src="ba2.png",width=580,height=250)),
                              h3("2. Metabolic Cage:"),
                              img(src="mc1.png",width=610,height=270),
                              img(src="mc2.png",width=610,height=270),
                              img(src="mc3.png",width=610,height=270),
                              h3("3. GTT/ITT:"),
                              img(src="itt1.png",width=600,height=250),
                              img(src="itt2.png",width=600,height=250)
                              ))
))

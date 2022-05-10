#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
set.seed(23)
samplemeans10 = NULL
samplemeans50 = NULL
samplemeans1000 = NULL
samplesd10 = NULL
samplesd50 = NULL
samplesd1000 = NULL
n1 = 10
n2 = 50
n3 = 1000
truemean = 10
truesd = 3
for (i in 1:50) {
  samplemeans10[i] = mean(rnorm(n1, truemean, truesd))
  samplesd10[i] = sd(rnorm(n1, truemean, truesd))
}
for (i in 1:50) {
  samplemeans50[i] = mean(rnorm(n2, truemean, truesd))
  samplesd50[i] = sd(rnorm(n2, truemean, truesd))
}
for (i in 1:50) {
  samplemeans1000[i] = mean(rnorm(n3, truemean, truesd))
  samplesd1000[i] = sd(rnorm(n3, truemean, truesd))
}
CI <- function(n, samplemean, sd, confidencelevel) {
  lowerbound = samplemean - qnorm(1-(1-confidencelevel)/2)*(sd/sqrt(n))
  upperbound = samplemean + qnorm(1-(1-confidencelevel)/2)*(sd/sqrt(n))
  trial = c(1:50)
  confidence.interval = upperbound - lowerbound
  return(data.frame(cbind(trial, lowerbound, upperbound, confidence.interval)))
}
a = CI(n1, samplemeans10, samplesd10, .95)
b = CI(n2, samplemeans50, samplesd50, .95)
c = CI(n3, samplemeans1000, samplesd1000, .95)
d = CI(n2, samplemeans50, samplesd50, .5)
e = CI(n2, samplemeans50, samplesd50, .9)
f = CI(n2, samplemeans50, samplesd50, .99)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(e, aes(x = samplemeans50, y = trial, group = confidence.interval)) +
      geom_errorbar(aes(xmin= lowerbound, xmax= upperbound), width=.2, position=position_dodge(.9)) +
      geom_dotplot(binaxis='y', stackdir='center') +
      ylab("Trial Number") +
      xlab("Sample Mean and resulting CI") +
      ggtitle("90% Confidence intervals where n=50") +
      geom_vline(xintercept = 10, color = "red") +
      theme(axis.text.x = element_text(angle = 45))+
      scale_x_continuous(limits = c(8, 12))+
      theme(plot.title = element_text(size=8))+
      theme(axis.title.x  = element_text(size=7))+
      theme(axis.title.y  = element_text(size=7))+
      theme(plot.caption = element_text(size=5))+
      labs(caption = "Figure 6")+
      theme(plot.caption = element_text(size=5))+
      theme(plot.caption = element_text(hjust = 0.5))
  })
}
# Run the application
shinyApp(ui = ui, server = server)

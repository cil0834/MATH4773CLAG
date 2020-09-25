# Load libraries needed
library(shiny)
#library(ggplot2)
library(purrr)
library(rootSolve)
source("Rcode.r")
library(scatterplot3d)
data(trees)
library(plotly)
library(plot3D)


spruce.df = read.csv("SPRUCE.csv")

d = spruce.df$BHDiameter



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Spruce Data Set: Piecewise Regression"),
  
  # Sidebar with a slider input for number of bins 
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("xk1",
                  "Choose knot 1:",
                  min = min(d),
                  max = max(d),
                  value = 17.44165,
                  step=0.01),
      
      sliderInput("xk2",
                  "Choose knot 2:",
                  min = min(d),
                  max = max(d),
                  value = 17.44165,
                  step=0.01),
      
      sliderInput("GraphDetail",
                  "3D plot detail:",
                  min = 10,
                  max = 100,
                  value = 10,
                  step=1),
      
      sliderInput("intervalroot",
                  "choose L and U for root interval 1:",
                  min = min(d),
                  max = max(d),
                  value = c(7,10),
                  step=0.01),
      
      sliderInput("intervalroot2",
                  "choose L and U for root interval 2:",
                  min = min(d),
                  max = max(d),
                  value = c(18,20),
                  step=0.01),
      
      sliderInput("Accuracy",
                  "Choose how the sliders increment:",
                  min = 0.0001,
                  max = 1,
                  value = 0.01,
                  step=0.0001)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("regressPlot"),
      plotOutput("R2"),
      tableOutput("root"),
      # table of data
      tableOutput("tab"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$tab <- renderTable(spruce.df)
  
  
  observe({
    maximum <- input$xk2
    updateSliderInput(session, "xk1",
                      max = maximum)
  })
  
  
  
  observe({
    maximum = input$intervalroot2[1]
    updateSliderInput(session, "intervalroot",
                      max = maximum)
  })
  
  observe({
    minimum = input$intervalroot[2]
    updateSliderInput(session, "intervalroot2",
                      min = minimum)
  })
  
  
  observe({
    step_size = input$Accuracy
    updateSliderInput(session, "xk1",
                      step = step_size)
    updateSliderInput(session, "xk2",
                      step = step_size)
    updateSliderInput(session, "intervalroot",
                      step = step_size)
    updateSliderInput(session, "intervalroot2",
                      step = step_size)
    
  })
  
  
  
  output$regressPlot <- renderPlot({
    plot(spruce.df,main="Piecewise regression",pch=21,bg="black")
    
    
    
    
    sp2.df=within(spruce.df, {
      X<-(BHDiameter-input$xk1)*(BHDiameter>input$xk1) 
      X2<-(BHDiameter-input$xk2)*(BHDiameter>input$xk2)
    }
    ) 
    
    lmp = lm(Height ~ BHDiameter + X + X2, data = sp2.df)
    tmp=summary(lmp) # tmp holds the summary info
    
    
    curve(myf2(x,xk=input$xk1,xk2 = input$xk2, coef=coef(lmp)),
          add=TRUE, 
          lwd=2,
          col="Blue")
    
    
    
    points(input$xk1,myf2(input$xk1, input$xk1,input$xk2,coef=coef(lmp)),col="black",pch=21,bg="green",cex=2) 
    
    points(input$xk2,myf2(input$xk2, input$xk1,input$xk2,coef=coef(lmp)),col="black",pch=21,bg="red",cex=2) 
    
    
    text((input$xk2 + input$xk1)/2,16,
         paste("R sq.=",round(tmp$r.squared,4) ))
    
  }) 
  
  
  output$R2 <- renderPlot({
    
    rsq_real = Vectorize(rsq_xk1_xk2_data)
    
    x1 = seq(min(d),max(d), length=input$GraphDetail)
    x2 <- x1
    r_squ <- outer(x1, x2, rsq_real)
    
    
    persp3D(x1,x2,r_squ, axes=TRUE,scale=2, box=TRUE,
            ticktype="detailed", xlab="xk1", ylab="xk2", zlab="r^2", 
            main="R^2 value given 2 x knots")
    inter = matrix_rsquared(x1=input$intervalroot, x2=input$intervalroot2, h=0.1, data=spruce.df)
    text3D(x = inter[1], y = inter[2], 
           z = inter[3], add = TRUE, 
           labels = "x", col = c("black", "red"))
    text3D(x = inter[1], y = inter[2], 
           z = inter[3] + .02, add = TRUE, 
           labels = paste("x1 =", inter[1], ", x2 =", inter[2], ", R sq.=",round(inter[3],4)), col = c("black", "red"))
  })
  
  
  output$root<-renderTable({
    intvs = matrix_rsquared(x1=input$intervalroot, x2=input$intervalroot2, h=0.1, data=spruce.df)
    
    intervals.df = data.frame("x1" = intvs[1], "x2" = intvs[2], "r_squared" = intvs[3])
    intervals.df
      })
}

# Run the application 
shinyApp(ui = ui, server = server)


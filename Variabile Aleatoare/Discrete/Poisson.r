library(shiny)

I_DiscPoissonParams <- function(id) {
  tagList(
    # parametru lambda 
    column(6, sliderInput(paste(id, "_lambda", sep =""), "Lambda:", min = 0.01, max = 100, 50, 0.01))
  )
} 
IO_DiscPoissonProbA <- function(id, input){
  tagList(
    # input a
    column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = 100, 45, 1)),
    # output
    column(6, br(), br(), renderPrint({
      paste("P(X <= ", input$pois_a, ") = ",  ppois(input$pois_a, input$pois_lambda))
    }))
  )
}
IO_DiscPoissonProbB <- function(id, input){
  tagList(
    # input b
    column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = 100, 40, 1)), 
    # output 
    column(6, br(), br(), renderPrint({
      p = 1 - ppois(input$pois_b, input$pois_lambda) + 
        dpois(input$pois_b, input$pois_lambda)
      paste("P(X >= ", input$pois_b, ") = ",  p)
    }))
  )
}
IO_DiscPoissonProbCD <- function(id, input){
  tagList(
    column(6,
           # input c
           column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",33,0,100)),
           # input d
           column(4, numericInput(paste(id, "_d", sep =""),"x <= d:",44,0,100))    
    ),
    # output
    column(6, br(), renderPrint({
        if (input$pois_c <= input$pois_d)
            p = ppois(input$pois_d, input$pois_lambda) - 
                ppois(input$pois_c, input$pois_lambda) +
                dpois(input$pois_c, input$pois_lambda)
        else 
            p = "Input invalid"
        paste("P(", input$pois_c, " <= X <=", input$pois_d, ")= ", p)
    }))
  )
}
O_DiscPoissonFuncMasa <- function(input){
  renderPlot({
    barplot(dpois(0:100, input$pois_lambda),
            names.arg = 0:100,
            col = 'lightseagreen',
            main = "Functia de Masa",
    )
  })
}
O_DiscPoissonFuncRep <- function(input){
  tagList(
    renderPlot({
        # graficul functiei de repartitie
        x = seq(from = 0, to = 100, by = 0.01)
        y = ppois(x, input$pois_lambda)
        plot(x, y,type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 16, bty = "n", cex = 0.5)
    }),
    # Probabilitatile
    renderPlot({
        p = dpois(0:100, input$pois_lambda)
        #P(X <= a)
        colA1 = rep("orange", input$pois_a + 1)
        colA2 = rep("red", 100 - input$pois_a)
        colFinalA = c(colA1, colA2)
        # P(X >= b)
        colB1 = rep("red", input$pois_b)
        colB2 = rep("purple", 100 - input$pois_b + 1)
        colFinalB = c(colB1, colB2)
        # P(c <= X <= d)
        if (input$pois_c <= input$pois_d){
            colCD1 = rep("red", input$pois_c)
            colCD2 = rep("green", input$pois_d - input$pois_c + 1)
            colCD3 = rep("red", 100 - input$pois_d)
            colFinalCD = c(colCD1, colCD2, colCD3)
            
            legendCD =  paste("P(", input$pois_c,  "<= X <=",  input$pois_d, ")")
        } else {
            colFinalCD = rep("red", 101)
            legendCD = "Input invalid"
        }
        # Grafic
        barplot(cbind(p, p, p), beside = T, col = c(colFinalA, colFinalB, colFinalCD), 
                names.arg = c(0:100, 0:100, 0:100),
                main = "Probabilitati")
        legend("topright", c(paste("P( X <= ", input$pois_a, ")"), paste("P( X >=", input$pois_b,  ")"), 
                             legendCD), fill=c("orange","purple","green"))
    })
    
  )
}


DiscPoisson <- function(id_rep, input, output, label = "Discreta Poisson"){
  tagList(
    fluidRow(
      HTML(
        "<h4 style='padding:15px; margin:0;'>Fie X o v.a. repartizata Poisson de parametru lambda</h4>"
      )
    ), hr(),
    fluidRow(
      I_DiscPoissonParams(id_rep)
    ), hr(),
    # CALC PROB P(X <= a)
    fluidRow(
      IO_DiscPoissonProbA(id_rep, input)
    ),
    # CALC PROB P(X >= b)
    fluidRow(
      IO_DiscPoissonProbB(id_rep, input)
    ),
    # CALC PROB P(c <= X <= d)
    fluidRow(
      IO_DiscPoissonProbCD(id_rep, input)
    ),
    # FUNCTIA DE MASA: P(X = x)
    fluidRow(
      O_DiscPoissonFuncMasa(input)
    ),
    # FUNCTIA DE REPARTITIE
    fluidRow(
      O_DiscPoissonFuncRep(input)
    )
  )
}















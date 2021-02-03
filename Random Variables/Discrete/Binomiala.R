library(shiny)

I_BinomialaParams <- function(id) {
      tagList(
            # parametru N
            column(6, sliderInput(paste(id, "_n", sep =""), "n:", min = 0, max = 100, 15, 1)),
            # parametru P
            column(6, sliderInput(paste(id, "_p", sep =""), "p:", min = 0, max = 1, 0.5, 0.01))
      )
}
IO_BinomialaProbA <- function(id, input){
      tagList(
            # input a
             column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = 100, 7, 1)),
            # output
            column(6, br(), br(), renderPrint({
                paste("P(X <= ", input$binom_a, ") = ",  pbinom(input$binom_a, input$binom_n, input$binom_p))
            }))
      )
}
IO_BinomialaProbB <- function(id, input){
      tagList(
            # input b
            column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = 100, 5, 1)), 
            # output 
            column(6, br(), br(), renderPrint({
                  p = 1 - pbinom(input$binom_b, input$binom_n, input$binom_p) + 
                          dbinom(input$binom_b, input$binom_n, input$binom_p)
                  paste("P(X >= ", input$binom_b, ") = ",  p)
            }))
      )
}
IO_BinomialaProbCD <- function(id, input){
      tagList(
            column(6,
                   # input c
                   column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",5,0,100)),
                   # input d
                   column(4, numericInput(paste(id, "_d", sep =""),"x <= d:",8,0,100))    
            ),
            # output
            column(6, br(), renderPrint({
                  p = pbinom(input$binom_d, input$binom_n, input$binom_p) - 
                      pbinom(input$binom_c, input$binom_n, input$binom_p) +
                      dbinom(input$binom_c, input$binom_n, input$binom_p)
                paste("P(", input$binom_c, " <= X <=", input$binom_d, ")= ", p)
            }))
      )
}
O_BinomialaFuncMasa <- function(input){
      renderPlot({
            barplot(dbinom(0:input$binom_n, input$binom_n, input$binom_p),
                    names.arg = 0:input$binom_n,
                    col = 'lightseagreen',
                    main = "Functia de Masa",
            )
      })
}
O_BinomialaFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = 0, to = input$binom_n, by = 0.01)
            y1 = pbinom(x, input$binom_n, input$binom_p)
            plot(x, y1,type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 19,bty = "n",cex = 0.5)
        }),
        # Probabilitatile
        renderPlot({
            p = dbinom(0:input$binom_n, input$binom_n, input$binom_p)
            #P(X <= a)
            colA1 = rep("orange", input$binom_a + 1)
            colA2 = rep("red", input$binom_n - input$binom_a)
            colFinalA = c(colA1, colA2)
            # P(X >= b)
            colB1 = rep("red", input$binom_b)
            colB2 = rep("purple", input$binom_n - input$binom_b + 1)
            colFinalB = c(colB1, colB2)
            # P(c <= X <= d)
            colCD1 = rep("red", input$binom_c)
            colCD2 = rep("green", input$binom_d - input$binom_c + 1)
            colCD3 = rep("red", input$binom_n - input$binom_d)
            colFinalCD = c(colCD1, colCD2, colCD3)
            # Grafic
            barplot(cbind(p, p, p), beside = T, col = c(colFinalA, colFinalB, colFinalCD), 
                    names.arg = c(0:input$binom_n, 0:input$binom_n, 0:input$binom_n),
                    main = "Probabilitati")
            legend("topright", c(paste("P( X <= ", input$binom_a, ")"), paste("P( X >=", input$binom_b,  ")"), 
                                 paste("P(", input$binom_c,  "<= X <=",  input$binom_d, ")")), fill=c("orange","purple","green"))
        })
        
    )
}


Binomiala <- function(id_rep, input, output, label = "Binomiala"){
    tagList(
        fluidRow(
            I_BinomialaParams(id_rep)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_BinomialaProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_BinomialaProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_BinomialaProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_BinomialaFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_BinomialaFuncRep(input)
        )
    )
}















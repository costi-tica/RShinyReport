library(shiny)

I_NegBinomialaParams <- function(id) {
    tagList(
        # parametru R
        column(6, sliderInput(paste(id, "_r", sep =""), "r: (nr realizari) (X ~ nr de esecuri pana la r realizari (asa e default in R))", min = 1, max = 100, 15, 1)),
        # parametru P
        column(6, sliderInput(paste(id, "_p", sep =""), "p:", min = 0.01, max = 1, 0.2, 0.01))
    )
}
IO_NegBinomialaProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = 200, 30, 1)),
        # output
        column(6, br(), br(), renderPrint({
            paste("P(X <= ", input$nbinom_a, ") = ",  pnbinom(input$nbinom_a, input$nbinom_r, input$nbinom_p))
        }))
    )
}
IO_NegBinomialaProbB <- function(id, input){
    tagList(
        # input b
        column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = 200, 60, 1)), 
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - pnbinom(input$nbinom_b, input$nbinom_r, input$nbinom_p) + 
                    dnbinom(input$nbinom_b, input$nbinom_r, input$nbinom_p)
            paste("P(X >= ", input$nbinom_b, ") = ",  p)
        }))
    )
}
IO_NegBinomialaProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",46,0,200)),
               # input d
               column(4, renderUI({
                   numericInput(paste(id, "_d", sep =""),"x <= d:", value = max(input$nbinom_c, input$nbinom_d), min = input$nbinom_c, max = 201)
               }))    
        ),
        # output
        column(6, br(), renderPrint({
            p = pnbinom(input$nbinom_d, input$nbinom_r, input$nbinom_p) - 
                pnbinom(input$nbinom_c, input$nbinom_r, input$nbinom_p) +
                dnbinom(input$nbinom_c, input$nbinom_r, input$nbinom_p)
            paste("P(", input$nbinom_c, " <= X <=", input$nbinom_d, ")= ", p)
        }))
    )
}
O_NegBinomialaFuncMasa <- function(input){
    renderPlot({
        barplot(dnbinom(0:200, input$nbinom_r, input$nbinom_p),
                names.arg = 0:200,
                col = 'lightseagreen',
                main = "Functia de Masa",
        )
    })
}
O_NegBinomialaFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = 0, to = 200, by = 0.1)
            y = pnbinom(x, input$nbinom_r, input$nbinom_p)
            plot(x, y,type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 19,bty = "n",cex = 0.5)
        }),
        # Probabilitatile
        renderPlot({
            p = dnbinom(0:200, input$nbinom_r, input$nbinom_p)
            #P(X <= a)
            colA1 = rep("orange", input$nbinom_a + 1)
            colA2 = rep("red", 200 - input$nbinom_a)
            colFinalA = c(colA1, colA2)
            barplot(p, col = colFinalA, names.arg = c(0:200), main = paste("P( X <= ", input$nbinom_a, ")"))
        }),
        renderPlot({
            p = dnbinom(0:200, input$nbinom_r, input$nbinom_p)
            # P(X >= b)
            colB1 = rep("red", input$nbinom_b)
            colB2 = rep("purple", 200 - input$nbinom_b + 1)
            colFinalB = c(colB1, colB2)
            barplot(p, col = colFinalB, names.arg = c(0:200), main = paste("P( X >=", input$nbinom_b,  ")"))
        }),
        renderPlot({
            p = dnbinom(0:200, input$nbinom_r, input$nbinom_p)
            # P(c <= X <= d)
            colCD1 = rep("red", input$nbinom_c)
            colCD2 = rep("green", input$nbinom_d - input$nbinom_c + 1)
            colCD3 = rep("red", 200 - input$nbinom_d)
            colFinalCD = c(colCD1, colCD2, colCD3)
            barplot(p, col = colFinalCD, names.arg = c(0:200), main = paste("P(", input$nbinom_c,  "<= X <=",  input$nbinom_d, ")"))
        })
    )
}


NegBinomiala <- function(id_rep, input, output, label = "NegBinomiala"){
    tagList(
        fluidRow(
            I_NegBinomialaParams(id_rep)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_NegBinomialaProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_NegBinomialaProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_NegBinomialaProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_NegBinomialaFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_NegBinomialaFuncRep(input)
        )
    )
}















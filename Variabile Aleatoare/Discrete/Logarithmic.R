library(shiny)
library(actuar)

I_DiscLogarithmicParams <- function(id) {
    tagList(
        # parametru P
        column(6, sliderInput(paste(id, "_p", sep =""), "p:", min = 0.01, max = 0.99, 0.8, 0.01))
    )
}
IO_DiscLogarithmicProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = 50, 2, 1)),
        # output
        column(6, br(), br(), renderPrint({
            paste("P(X <= ", input$log_a, ") = ",  plogarithmic(input$log_a, input$log_p))
        }))
    )
}
IO_DiscLogarithmicProbB <- function(id, input){
    tagList(
        # input b
        column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = 50, 4, 1)), 
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - plogarithmic(input$log_b, input$log_p) + 
                    dlogarithmic(input$log_b, input$log_p)
            paste("P(X >= ", input$log_b, ") = ",  p)
        }))
    )
}
IO_DiscLogarithmicProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",12,0,50)),
               # input d
               column(4, renderUI({
                   numericInput(paste(id, "_d", sep =""),"x <= d:", value = max(input$log_c, input$log_d), min = input$log_c, max = 50)
               }))    
        ),
        # output
        column(6, br(), renderPrint({
            p = plogarithmic(input$log_d, input$log_p) - 
                plogarithmic(input$log_c, input$log_p) +
                dlogarithmic(input$log_c, input$log_p)
            paste("P(", input$log_c, " <= X <=", input$log_d, ")= ", p)
        }))
    )
}
O_DiscLogarithmicFuncMasa <- function(input){
    renderPlot({
        barplot(dlogarithmic(0:50, input$log_p),
                names.arg = 0:50,
                col = 'lightseagreen',
                main = "Functia de Masa",
        )
    })
}
O_DiscLogarithmicFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = 0, to = 50, by = 0.01)
            y = plogarithmic(x, input$log_p)
            plot(x, y,type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 19,bty = "n",cex = 0.5)
        }),
        # Probabilitatile
        renderPlot({
            p = dlogarithmic(0:50, input$log_p)
            #P(X <= a)
            colA1 = rep("orange", input$log_a + 1)
            colA2 = rep("red", 50 - input$log_a)
            colFinalA = c(colA1, colA2)
            barplot(p, col = colFinalA, names.arg = c(0:50), main = paste("P( X <= ", input$log_a, ")"))
        }),
        renderPlot({
            p = dlogarithmic(0:50, input$log_p)
            # P(X >= b)
            colB1 = rep("red", input$log_b)
            colB2 = rep("purple", 50 - input$log_b + 1)
            colFinalB = c(colB1, colB2)
            barplot(p, col = colFinalB, names.arg = c(0:50), main = paste("P( X >=", input$log_b,  ")"))
        }),
        renderPlot({
            p = dlogarithmic(0:50, input$log_p)
            # P(c <= X <= d)
            colCD1 = rep("red", input$log_c)
            colCD2 = rep("green", input$log_d - input$log_c + 1)
            colCD3 = rep("red", 50 - input$log_d)
            colFinalCD = c(colCD1, colCD2, colCD3)
            barplot(p, col = colFinalCD, names.arg = c(0:50), main = paste("P(", input$log_c,  "<= X <=",  input$log_d, ")"))
        })
    )
}


DiscLogarithmic <- function(id_rep, input, output, label = "DiscLogarithmic"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>Distributia logaritmica este cazul limitativ al distributiei binomiale 
                negative zero-trunchiate cu parametru de dimensiune egal cu 0. In acest context, 
                parametrul p corespunde în general probabilitatii de esec a negativ binomialei zero-trunchiata.</h4>"
            )
        ), hr(),
        fluidRow(
            I_DiscLogarithmicParams(id_rep)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_DiscLogarithmicProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_DiscLogarithmicProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_DiscLogarithmicProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_DiscLogarithmicFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_DiscLogarithmicFuncRep(input)
        )
    )
}















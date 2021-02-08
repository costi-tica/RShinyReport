library(shiny)

I_DiscBinomialaParams <- function(id) {
      tagList(
            # parametru N
            column(6, sliderInput(paste(id, "_n", sep =""), "n:", min = 0, max = 100, value = 15, 1)),
            # parametru P
            column(6, sliderInput(paste(id, "_p", sep =""), "p:", min = 0, max = 1, 0.5, 0.01))
      )
}
IO_DiscBinomialaProbA <- function(id, input){
      tagList(
            # input a
            column(6, renderUI({
              sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = input$binom_n, 1, 1)
            })),
            # output
            column(6, br(), br(), renderPrint({
                paste("P(X <= ", input$binom_a, ") = ",  pbinom(input$binom_a, input$binom_n, input$binom_p))
            }))
      )
}
IO_DiscBinomialaProbB <- function(id, input){
      tagList(
            # input b
            column(6, renderUI({
              sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = input$binom_n, 5, 1)
            })), 
            # output 
            column(6, br(), br(), renderPrint({
                  p = 1 - pbinom(input$binom_b, input$binom_n, input$binom_p) + 
                          dbinom(input$binom_b, input$binom_n, input$binom_p)
                  paste("P(X >= ", input$binom_b, ") = ",  p)
            }))
      )
}
IO_DiscBinomialaProbCD <- function(id, input){
      tagList(
            column(6,
                   # input c
                   column(4, renderUI({
                       numericInput(paste(id, "_c", sep =""),"x >= c:", min = 0, max = input$binom_n, value = 5)
                   })),
                   # input d
                   column(4, renderUI({
                     numericInput(paste(id, "_d", sep =""),"x <= d:",min = 0, max = input$binom_n, 8)
                    }))    
            ),
            # output
            column(6, br(), renderPrint({
                if (input$binom_c <= input$binom_d)
                    p = pbinom(input$binom_d, input$binom_n, input$binom_p) - 
                        pbinom(input$binom_c, input$binom_n, input$binom_p) +
                        dbinom(input$binom_c, input$binom_n, input$binom_p)
                else 
                    p = "Input invalid"
                paste("P(", input$binom_c, " <= X <=", input$binom_d, ")= ", p)
            }))
      )
}
O_DiscBinomialaFuncMasa <- function(input){
      renderPlot({
            barplot(dbinom(0:input$binom_n, input$binom_n, input$binom_p),
                    names.arg = 0:input$binom_n,
                    col = 'lightseagreen',
                    main = "Functia de Masa",
            )
      })
}
O_DiscBinomialaFuncRep <- function(input){
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
            if (input$binom_c <= input$binom_d){
                colCD1 = rep("red", input$binom_c)
                colCD2 = rep("green", input$binom_d - input$binom_c + 1)
                colCD3 = rep("red", input$binom_n - input$binom_d)
                colFinalCD = c(colCD1, colCD2, colCD3)
                
                legendCD = paste("P(", input$binom_c,  "<= X <=",  input$binom_d, ")")
            } else {
                colFinalCD = rep("red", input$binom_n + 1)
                legendCD = "Input invalid"
            }
            # Grafic
            barplot(cbind(p, p, p), beside = T, col = c(colFinalA, colFinalB, colFinalCD), 
                    names.arg = c(0:input$binom_n, 0:input$binom_n, 0:input$binom_n),
                    main = "Probabilitati")
            legend("topright", c(paste("P( X <= ", input$binom_a, ")"), paste("P( X >=", input$binom_b,  ")"), 
                                 legendCD), fill=c("orange","purple","green"))
        })
        
    )
}


DiscBinomiala <- function(id_rep, input, output, label = "DiscBinomiala"){
    tagList(
        fluidRow(
            HTML(
              "<h4 style='padding:15px; margin:0;'>Avand n experimente aleatoare independente, ne interesam in realizarea sau 
                                                    nerealizarea unui even. A.   P(A) = p si variabila aleatoare X defineste numarul 
                                                    total de realizari ale even. A.</h4>"
            )
        ), hr(),
        fluidRow(
            I_DiscBinomialaParams(id_rep)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_DiscBinomialaProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_DiscBinomialaProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_DiscBinomialaProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_DiscBinomialaFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_DiscBinomialaFuncRep(input)
        )
    )
}
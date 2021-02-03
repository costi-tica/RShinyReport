library(shiny)

I_HiperParams <- function(id) {
    tagList(
        column(4, sliderInput(paste(id, "_m", sep =""), "m: (nr bile negre) cele pe care le dorim", min = 0, max = 100, 30, 1)),
        # parametru n Nr Bile Negre
        column(4, sliderInput(paste(id, "_n", sep =""), "n: (nr bile albe)", min = 1, max = 100, 30, 1)),
        # parametru k Nr Bile Extrase
        column(4, sliderInput(paste(id, "_k", sep =""), "k: (nr bile extrase)", min = 0, max = 200, 20, 1))
    )
}
IO_HiperProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = 100, 7, 1)),
        # output
        column(6, br(), br(), renderPrint({
            paste("P( X <= ", input$hiper_a, ") = ",  phyper(input$hiper_a, input$hiper_m, input$hiper_n, input$hiper_k))
        }))
    )
}
IO_HiperProbB <- function(id, input){
    tagList(
        # input b
        column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = 100, 12, 1)), 
        # output 
        column(6, br(), br(), renderPrint({
                p = 1 - phyper(input$hiper_b, input$hiper_m, input$hiper_n, input$hiper_k) + 
                        dhyper(input$hiper_b, input$hiper_m, input$hiper_n, input$hiper_k)
                paste("P( X >= ", input$hiper_b, ") = ",  p)
        }))
    )
}
IO_HiperProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",10,0,100)),
               # input d
               column(4, numericInput(paste(id, "_d", sep =""),"x <= d:",12,0,100))    
        ),
        # output
        column(6, br(), renderPrint({
               p = phyper(input$hiper_d, input$hiper_m, input$hiper_n, input$hiper_k) - 
                   phyper(input$hiper_c, input$hiper_m, input$hiper_n, input$hiper_k) + 
                   dhyper(input$hiper_c, input$hiper_m, input$hiper_n, input$hiper_k)
               paste("P(", input$hiper_c, " <= X <=", input$hiper_d, ")= ", p)
        }))
    )
}
O_HiperFuncMasa <- function(input){
    renderPlot({
        if (input$hiper_m + input$hiper_n >= input$hiper_k){
            barplot(dhyper(0:input$hiper_m, input$hiper_m, input$hiper_n, input$hiper_k),
                    names.arg = 0:input$hiper_m,
                    col = 'lightseagreen',
                    main = "Functia de Masa",
            )
        }
    })
}
O_HiperFuncRep <- function(input){
    tagList(
        # Functia de repartitie
        renderPlot({
            if (input$hiper_m + input$hiper_n >= input$hiper_k){
                x = seq(from = 0, to = input$hiper_m, by = 0.01)
                y = phyper(x, input$hiper_m, input$hiper_n, input$hiper_k)
                plot(x, y,type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 19,bty = "n",cex = 0.5)
            }
        }),
        # Probabilitatile
        renderPlot({
            p = dhyper(0:input$hiper_m, input$hiper_m, input$hiper_n, input$hiper_k)
            #P(X <= a)
            colA1 = rep("orange", input$hiper_a + 1)
            colA2 = rep("red", input$hiper_m - input$hiper_a)
            colFinalA = c(colA1, colA2)
            # P(X >= b)
            colB1 = rep("red", input$hiper_b)
            colB2 = rep("purple", input$hiper_m - input$hiper_b + 1)
            colFinalB = c(colB1, colB2)
            # P(c <= X <= d)
            colCD1 = rep("red", input$hiper_c)
            colCD2 = rep("green", input$hiper_d - input$hiper_c + 1)
            colCD3 = rep("red", input$hiper_m - input$hiper_d)
            colFinalCD = c(colCD1, colCD2, colCD3)
            # Grafic
            barplot(cbind(p, p, p), beside = T, col = c(colFinalA, colFinalB, colFinalCD), 
                    names.arg = c(0:input$hiper_m, 0:input$hiper_m, 0:input$hiper_m),
                    main = "Probabilitati")
            legend("topright", c(paste("P( X <= ", input$hiper_a, ")"), paste("P( X >=", input$hiper_b,  ")"), 
                                    paste("P(", input$hiper_c,  "<= X <=",  input$hiper_d, ")")), fill=c("orange","purple","green"))
        })
    )
}


Hipergeom <- function(id_rep, input, output, label = "Hipergeom"){
    tagList(
        fluidRow(
            I_HiperParams(id_rep)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_HiperProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_HiperProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_HiperProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_HiperFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_HiperFuncRep(input)
        )
    )
}















library(shiny)
library(tolerance)

I_DiscNegHiperParams <- function(id, input) {
    tagList(
        # parametru n Nr Bile NEGRE
        column(4, sliderInput(paste(id, "_m", sep =""), "m: (nr bile negre)", min = 0, max = 100, 30, 1)),
        # parametru n Nr Total bile
        column(4, renderUI({
            sliderInput(paste(id, "_n", sep =""), "n: (nr total de bile - (negre + albe)", min = input$nhiper_m, max = 200, 60, 1)
        })),
        # parametru k Nr Bile negre dorite
        column(4, renderUI({
            sliderInput(paste(id, "_k", sep =""), "k: (nr bile negre - cate vrem sa extragem)", min = 0, max = input$nhiper_m, value = input$nhiper_m, 1)
        }))
    )
}
IO_DiscNegHiperProbA <- function(id, input){
    tagList(
        # input a
        column(6, renderUI({
            sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = input$nhiper_n, 1, 1)
        })),
        # output
        column(6, br(), br(), renderPrint({
            paste("P( X <= ", input$nhiper_a, ") = ",  pnhyper(input$nhiper_a, input$nhiper_m, input$nhiper_n, input$nhiper_k))
        }))
    )
}
IO_DiscNegHiperProbB <- function(id, input){
    tagList(
        # input b
        column(6, renderUI({
            sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = input$nhiper_n, 1, 1)
        })), 
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - pnhyper(input$nhiper_b, input$nhiper_m, input$nhiper_n, input$nhiper_k) + 
                    dnhyper(input$nhiper_b, input$nhiper_m, input$nhiper_n, input$nhiper_k)
            paste("P( X >= ", input$nhiper_b, ") = ",  p)
        }))
    )
}
IO_DiscNegHiperProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, renderUI({
                   numericInput(paste(id, "_c", sep =""),"x >= c:", min = 0, max = input$nhiper_n, value = 1)
               })),
               # input d
               column(4, renderUI({
                   numericInput(paste(id, "_d", sep =""),"x <= d:",min = 0, max = input$nhiper_n, value = 1)
               }))    
        ),
        # output
        column(6, br(), renderPrint({
            if (input$nhiper_c <= input$nhiper_d)
                p = pnhyper(input$nhiper_d, input$nhiper_m, input$nhiper_n, input$nhiper_k) - 
                    pnhyper(input$nhiper_c, input$nhiper_m, input$nhiper_n, input$nhiper_k) + 
                    dnhyper(input$nhiper_c, input$nhiper_m, input$nhiper_n, input$nhiper_k)
            else 
                p = "Input invalid"
            paste("P(", input$nhiper_c, " <= X <=", input$nhiper_d, ")= ", p)
        }))
    )
}
O_DiscNegHiperFuncMasa <- function(input){
    renderPlot({
        barplot(dnhyper(0:input$nhiper_n, input$nhiper_m, input$nhiper_n, input$nhiper_k),
                names.arg = 0:input$nhiper_n,
                col = 'lightseagreen',
                main = "Functia de Masa",
        )
    })
}
O_DiscNegHiperFuncRep <- function(input){
    tagList(
        # Functia de repartitie
        renderPlot({
            x = seq(from = 0, to = input$nhiper_n, by = 0.01)
            y = pnhyper(x, input$nhiper_m, input$nhiper_n, input$nhiper_k)
            plot(x, y,type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 19,bty = "n",cex = 0.5)
        }),
        # Probabilitatile
        renderPlot({
            p = dnhyper(0:input$nhiper_n, input$nhiper_m, input$nhiper_n, input$nhiper_k)
            #P(X <= a)
            colA1 = rep("orange", input$nhiper_a + 1)
            colA2 = rep("red", input$nhiper_n - input$nhiper_a)
            colFinalA = c(colA1, colA2)
            # P(X >= b)
            colB1 = rep("red", input$nhiper_b)
            colB2 = rep("purple", input$nhiper_n - input$nhiper_b + 1)
            colFinalB = c(colB1, colB2)
            # P(c <= X <= d)
            if (input$nhiper_c <= input$nhiper_d){
                colCD1 = rep("red", input$nhiper_c)
                colCD2 = rep("green", input$nhiper_d - input$nhiper_c + 1)
                colCD3 = rep("red", input$nhiper_n - input$nhiper_d)
                colFinalCD = c(colCD1, colCD2, colCD3)
                
                legendCD = paste("P(", input$nhiper_c,  "<= X <=",  input$nhiper_d, ")")
            } else {
                colFinalCD = rep("red", input$nhiper_n + 1)
                legendCD = "Input invalid"
            }
            # Grafic
            barplot(cbind(p, p, p), beside = T, col = c(colFinalA, colFinalB, colFinalCD), 
                    names.arg = c(0:input$nhiper_n, 0:input$nhiper_n, 0:input$nhiper_n),
                    main = "Probabilitati")
            legend("topright", c(paste("P( X <= ", input$nhiper_a, ")"), paste("P( X >=", input$nhiper_b,  ")"), 
                                 legendCD), fill=c("orange","purple","green"))
        })
    )
}


DiscNegHipergeom <- function(id_rep, input, output, label = "DiscNegHipergeom"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>Fie o urna cu 'm' bile negre, iar nr total de bile este 'n'.
                            Consideram ca v.a X este data de numarul de bile extrase pana cand am reusit sa extragem k bile negre.
                            O distributie negativ hipergeometrica (numita uneori distributie hipergeometrica inversa) modeleaza numarul 
                            total de incercari pana cand apar k succese</h4>"
            )
        ), hr(),
        fluidRow(
            I_DiscNegHiperParams(id_rep, input)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_DiscNegHiperProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_DiscNegHiperProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_DiscNegHiperProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_DiscNegHiperFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_DiscNegHiperFuncRep(input)
        )
    )
}















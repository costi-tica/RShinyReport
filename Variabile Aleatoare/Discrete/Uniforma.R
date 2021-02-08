library(shiny)

I_DiscUniformaParams <- function(id) {
    tagList(
        # parametru N
        column(6, sliderInput(paste(id, "_n_left", sep =""), "marginea inferioara", min = -100, max = 100, 0, 1)),
        # parametru P
        column(6, sliderInput(paste(id, "_n_right", sep =""), "marginea superioara", min = -100, max = 100, 9, 1))
    )
}
IO_DiscUniformaProbA <- function(id, input){
    tagList(
        # input a
        column(6, renderUI({
            sliderInput(paste(id, "_a", sep =""), "P(x <= a):",
                        min = input$unif_n_left, max = input$unif_n_right, value = input$unif_n_left + 1, 1)
        })),
        # output
        column(6, br(), br(), renderPrint({
               p = (input$unif_a - input$unif_n_left + 1) / (input$unif_n_right - input$unif_n_left + 1)
               paste("P(X <= ", input$unif_a, ") = ",  p)
        }))
    )
}
IO_DiscUniformaProbB <- function(id, input){
    tagList(
        # input b
        column(6, renderUI({
            sliderInput(paste(id, "_b", sep =""),"P(x >= b):",min = input$unif_n_left, max = input$unif_n_right, value = input$unif_n_right - 1, 1)
        })), 
        # output 
        column(6, br(), br(), renderPrint({
               p = (input$unif_n_right - input$unif_b + 1) / (input$unif_n_right - input$unif_n_left + 1)
               paste("P(X >= ", input$unif_b, ") = ",  p)
        }))
    )
}
IO_DiscUniformaProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, renderUI({
                   numericInput(paste(id, "_c", sep =""),"x >= c:", min = input$unif_n_left, max = input$unif_n_right, value = input$unif_n_left + 1)
               })),
               # input d
               column(4, renderUI({
                   numericInput(paste(id, "_d", sep =""),"x <= d:", min = input$unif_n_left, max = input$unif_n_right, value = input$unif_n_right - 1)    
               }))
        ),
        # output
        column(6, br(), renderPrint({
               if (input$unif_c <= input$unif_d){
                   p = (input$unif_d - input$unif_c + 1) / (input$unif_n_right - input$unif_n_left + 1)
               } else 
                   p = "Input invalid"
               paste("P(", input$unif_c, " <= X <=", input$unif_d, ")= ", p)
        }))
    )
}
O_DiscUniformaFuncMasa <- function(input){
    renderPlot({
        if (input$unif_n_left <= input$unif_n_right){
            px = 1 / (input$unif_n_right - input$unif_n_left + 1)
            barplot(rep(px, input$unif_n_right - input$unif_n_left + 1),
                    names.arg = input$unif_n_left:input$unif_n_right,
                    col = 'lightseagreen',
                    main = "Functia de Masa",
            )  
        }
    })
}
O_DiscUniformaFuncRep <- function(input){
    tagList(
        # Func de repartitie
        renderPlot({
            if (input$unif_n_left <= input$unif_n_right){
                x = seq(from = input$unif_n_left, to = input$unif_n_right, by = 0.01)
                y_aux = function(x){
                    (as.integer(floor(x)) - input$unif_n_left + 1) / (input$unif_n_right - input$unif_n_left + 1)
                }
                y_aux = Vectorize(y_aux, "x")
                y1 = y_aux(x)
                
                plot(x, y1,type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 19,bty = "n",cex = 0.5)
            }
        }),
        # Probabilitatile
        renderPlot({
                p = rep(1 / (input$unif_n_right - input$unif_n_left + 1), input$unif_n_right - input$unif_n_left + 1)
                #P(X <= a)
                colA1 = rep("orange", input$unif_a - input$unif_n_left + 1)
                colA2 = rep("red", input$unif_n_right - input$unif_a)
                colFinalA = c(colA1, colA2)
                # P(X >= b)
                colB1 = rep("red", input$unif_b - input$unif_n_left)
                colB2 = rep("purple", input$unif_n_right - input$unif_b + 1)
                colFinalB = c(colB1, colB2)
                # P(c <= X <= d)
                if (input$unif_c <= input$unif_d){
                    colCD1 = rep("red", input$unif_c - input$unif_n_left)
                    colCD2 = rep("green", input$unif_d - input$unif_c + 1)
                    colCD3 = rep("red", input$unif_n_right - input$unif_d)
                    colFinalCD = c(colCD1, colCD2, colCD3)
                    
                    legendCD = paste("P(", input$unif_c,  "<= X <=",  input$unif_d, ")")
                } else {
                    colFinalCD = rep("red", input$unif_n_right - input$unif_n_left + 1)
                    legendCD = "Input invalid"
                }
                # Grafic
                barplot(cbind(p, p, p), beside = T, col = c(colFinalA, colFinalB, colFinalCD), 
                        names.arg = c(input$unif_n_left:input$unif_n_right, input$unif_n_left:input$unif_n_right, input$unif_n_left:input$unif_n_right),
                        main = "Probabilitati")
                legend("topright", c(paste("P( X <= ", input$unif_a, ")"), paste("P( X >=", input$unif_b,  ")"), 
                                     legendCD), fill=c("orange","purple","green"))
        })
    )
}


DiscUniforma <- function(id_rep, input, output, label = "DiscUniforma"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>Fie un interval numarabil dat de limita inferioara si limita superioara.
                            Spunem ca v.a. X este repartizata uniform daca pentru orice numar x din interval avem aceeasi prob. P(X = x) = 1/(lungime interval).</h4>"
            )
        ), hr(),
        fluidRow(
            I_DiscUniformaParams(id_rep)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_DiscUniformaProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_DiscUniformaProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_DiscUniformaProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_DiscUniformaFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_DiscUniformaFuncRep(input)
        )
    )
}















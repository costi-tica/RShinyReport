library(shiny)

I_DiscGeometricaParams <- function(id) {
    tagList(
        # parametru P
        column(12, sliderInput(paste(id, "_p", sep =""), "p (prob. de realizare a evenimentului):", min = 0.01, max = 1, 0.1, 0.01))
    )
}
IO_DiscGeometricaProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = 100, 7, 1)),
        # output
        column(6, br(), br(), renderPrint({
            paste("P(X <= ", input$geom_a, ") = ",  pgeom(input$geom_a, input$geom_p))
        }))
    )
}
IO_DiscGeometricaProbB <- function(id, input){
    tagList(
        # input b
        column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = 100, 5, 1)), 
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - pgeom(input$geom_b, input$geom_p) + 
                    dgeom(input$geom_b, input$geom_p)
            paste("P(X >= ", input$geom_b, ") = ",  p)
        }))
    )
}
IO_DiscGeometricaProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",5,0,100)),
               # input d
               column(4, numericInput(paste(id, "_d", sep =""),"x <= d:", value = 10 , 0, 100))
        ),
        # output
        column(6, br(), renderPrint({
            if (input$geom_c <= input$geom_d)
                p = pgeom(input$geom_d, input$geom_p) - 
                    pgeom(input$geom_c, input$geom_p) +
                    dgeom(input$geom_c, input$geom_p)
            else 
                p = "Input invalid"
            paste("P(", input$geom_c, " <= X <=", input$geom_d, ")= ", p)
        }))
    )
}
O_DiscGeometricaFuncMasa <- function(input){
    renderPlot({
        barplot(dgeom(0:100, input$geom_p),
                names.arg = 0:100,
                col = 'lightseagreen',
                main = "Functia de Masa",
        )
    })
}
O_DiscGeometricaFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = 0, to = 100, by = 0.1)
            y = pgeom(x, input$geom_p)
            plot(x, y,type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 19,bty = "n",cex = 0.5)
        }),
        # Probabilitatile
        renderPlot({
            p = dgeom(0:100, input$geom_p)
            #P(X <= a)
            colA1 = rep("orange", input$geom_a + 1)
            colA2 = rep("red", 100 - input$geom_a)
            colFinalA = c(colA1, colA2)
            # P(X >= b)
            colB1 = rep("red", input$geom_b)
            colB2 = rep("purple", 100 - input$geom_b + 1)
            colFinalB = c(colB1, colB2)
            # P(c <= X <= d)
            if (input$geom_c <= input$geom_d){
                colCD1 = rep("red", input$geom_c)
                colCD2 = rep("green", input$geom_d - input$geom_c + 1)
                colCD3 = rep("red", 100 - input$geom_d)
                colFinalCD = c(colCD1, colCD2, colCD3)
                
                legendCD =  paste("P(", input$geom_c,  "<= X <=",  input$geom_d, ")")
            } else {
                colFinalCD = rep("red", 101)
                legendCD = "Input invalid"
            }
            # Grafic
            barplot(cbind(p, p, p), beside = T, col = c(colFinalA, colFinalB, colFinalCD), 
                    names.arg = c(0:100, 0:100, 0:100),
                    main = "Probabilitati")
            
            legend("topright", c(paste("P( X <= ", input$geom_a, ")"), paste("P( X >=", input$geom_b,  ")"), 
                                 legendCD), fill=c("orange","purple","green"))
        })
        
    )
}


DiscGeometrica <- function(id_rep, input, output, label = "DiscGeomertica"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>Aruncam in mod repetat cu o moneda si sansa sa pice cap este p.
                                Fie v.a. X care defineste numarul de esecuri(T) pana la primul succes(H).
                                Aparent 'geom' inplemantata in R este varianta cu numarul de esecuri inaintea primului succes.</h4>"
            )
        ), hr(),
        fluidRow(
            I_DiscGeometricaParams(id_rep)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_DiscGeometricaProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_DiscGeometricaProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_DiscGeometricaProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_DiscGeometricaFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_DiscGeometricaFuncRep(input)
        )
    )
}















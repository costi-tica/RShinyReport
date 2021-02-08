library(shiny)

I_ContUniformaParams <- function(id, input) {
    tagList(
        # parametru limita inferioara
        column(6, sliderInput(paste(id, "_n_left", sep =""), "marginea inferioara:", min = -100, max = 100, value = -15, 1)),
        # parametru limita superioara
        column(6, sliderInput(paste(id, "_n_right", sep =""), "marginea superioara:", min = -100, max = 100, value = 30, 1))
    )
}
IO_ContUniformaProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = -100, max = 100, 0, 1)),
        # output
        column(6, br(), br(), renderPrint({
            p = punif(input$c_unif_a, input$c_unif_n_left, input$c_unif_n_right)
            paste("P(X <= ", input$c_unif_a, ") = ",  p)
        }))
    )
}
IO_ContUniformaProbB <- function(id, input){
    tagList(
        # input b
        column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = -100, max = 100, 0, 1)),
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - punif(input$c_unif_b, input$c_unif_n_left, input$c_unif_n_right)
            paste("P(X >= ", input$c_unif_b, ") = ",  p)
        }))
    )
}
IO_ContUniformaProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:", 20, -100, 100)),
               # input d
               column(4, numericInput(paste(id, "_d", sep =""),"x <= d:", 30, -100 ,100))
        ),
        # output
        column(6, br(), renderPrint({
            if (input$c_unif_c < input$c_unif_d)
                p = punif(input$c_unif_d, input$c_unif_n_left, input$c_unif_n_right) - 
                    punif(input$c_unif_c, input$c_unif_n_left, input$c_unif_n_right)
            else 
                p = "Input invalid"
            paste("P(", input$c_unif_c, " <= X <=", input$c_unif_d, ")= ", p)
        }))
    )
}
O_ContUniformaFuncMasa <- function(input){
    renderPlot({
        x = seq(from = input$c_unif_n_left - 5, to = input$c_unif_n_right + 5, by = 0.01)
        y = dunif(x, input$c_unif_n_left, input$c_unif_n_right)
        plot(x, y, main = "Densitatea", xlab = "x",ylab = "f(x)", type = "p", col = "lightseagreen",bty = "n", pch= 16, cex=0.5)
    })
}
O_ContUniformaFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = input$c_unif_n_left - 5, to = input$c_unif_n_right + 5, by = 0.01)
            y = punif(x, input$c_unif_n_left, input$c_unif_n_right)
            plot(x, y, type = 'l',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black",bty = "n")
        }),
        #P(X <= a)
        renderPlot({
            x = seq(from = input$c_unif_n_left - 5, to = input$c_unif_n_right + 5, by = 0.01)
            y = dunif(x, input$c_unif_n_left, input$c_unif_n_right)
            plot(x, y, main = paste("P( X <= ", input$c_unif_a, ")"), xlab = "x",ylab = "F(x)", type = "p", col = "lightseagreen", pch = 16, cex = 0.2, bty= "n")
            
            if (input$c_unif_a > input$c_unif_n_left){
                x1 = seq(from = input$c_unif_n_left, to = input$c_unif_a, by = 0.01)
                p1 = dunif(x1, input$c_unif_n_left, input$c_unif_n_right)
                xx1 = c(input$c_unif_n_left, x1, input$c_unif_a)
                yy1 = c(0, p1, 0)
                polygon(xx1, yy1, col="purple", density = 10)
            } else {
                legend("top", "Input invalid", fill= "red")
            }
        }),
        #P(X >= b)
        renderPlot({
            x = seq(from = input$c_unif_n_left - 5, to = input$c_unif_n_right + 5, by = 0.01)
            y = dunif(x, input$c_unif_n_left, input$c_unif_n_right)
            plot(x, y, main = paste("P( X >= ", input$c_unif_b, ")"), xlab = "x",ylab = "F(x)", type = "p", col = "lightseagreen",pch = 16, cex = 0.2, bty= "n")

            if (input$c_unif_b < input$c_unif_n_right){
                x2 = seq(from = input$c_unif_b, to = input$c_unif_n_right, by = 0.01)
                p2 = dunif(x2, input$c_unif_n_left, input$c_unif_n_right)
                xx2 = c(input$c_unif_b, x2, input$c_unif_n_right)
                yy2 = c(0, p2, 0)
                polygon(xx2, yy2, col="red", density = 10, angle = 90)
            } else {
                legend("top", "Input invalid", fill= "red")
            }
        }),
        # P(c <= X <= d)
        renderPlot({
            x = seq(from = input$c_unif_n_left - 5, to = input$c_unif_n_right + 5, by = 0.01)
            y = dunif(x, input$c_unif_n_left, input$c_unif_n_right)
            plot(x, y, main = paste("P(", input$c_unif_c, " <= X <= ", input$c_unif_d, ")"), xlab = "x",ylab = "F(x)", type = "p", col = "lightseagreen",pch = 16, cex = 0.2, bty= "n")
                
            if (input$c_unif_c < input$c_unif_d){
                x3 = seq(from = input$c_unif_c, to = input$c_unif_d, by = 0.01)
                p3 = dunif(x3, input$c_unif_n_left, input$c_unif_n_right)
                xx3 = c(input$c_unif_c, x3, input$c_unif_d)
                yy3 = c(0, p3, 0)
                polygon(xx3, yy3, col="orange", density = 10, angle = 135)
            } else {
                legend("top", "Input invalid", fill= "red")
            }
        })
    )
}


ContUniforma <- function(id_rep, input, output, label = "ContUniforma"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>O v.a. este repartizata uniform pe un interval (a,b) daca admite densitatea de repartitie
                                                    f(x) = 1/ (b-a) daca x se afla intre a si b si f(x) = 0 altfel.</h4>"
            )
        ), hr(),
        fluidRow(
            I_ContUniformaParams(id_rep, input)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_ContUniformaProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_ContUniformaProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_ContUniformaProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_ContUniformaFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_ContUniformaFuncRep(input)
        )
    )
}

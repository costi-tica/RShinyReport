library(shiny)

I_ContExponParams <- function(id, input) {
    tagList(
        # parametru lambda
        column(6, sliderInput(paste(id, "_l", sep =""), "lambda:", min = 0, max = 10, value = 0.1, 0.01))
    )
}
IO_ContExponProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = 100, 5, 0.1)),
        # output
        column(6, br(), br(), renderPrint({
            paste("P(X <= ", input$c_exp_a, ") = ",  pexp(input$c_exp_a, input$c_exp_l))
        }))
    )
}
IO_ContExponProbB <- function(id, input){
    tagList(
        # input b
        column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = 100, 10, 0.1)),
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - pexp(input$c_exp_b, input$c_exp_l)
            paste("P(X >= ", input$c_exp_b, ") = ",  p)
        }))
    )
}
IO_ContExponProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",20,0,100)),
               # input d
               column(4,  numericInput(paste(id, "_d", sep =""),"x <= d:", 30, 0, 100))
        ),
        # output
        column(6, br(), renderPrint({
            if (input$c_exp_c < input$c_exp_d)
                p = pexp(input$c_exp_d, input$c_exp_l) - 
                    pexp(input$c_exp_c, input$c_exp_l)
            else 
                p = "Input invalid"
            paste("P(", input$c_exp_c, " <= X <=", input$c_exp_d, ")= ", p)
        }))
    )
}
O_ContExponFuncMasa <- function(input){
    renderPlot({
        x = seq(from = 0, to = 100, by = 0.01)
        y = dexp(x, input$c_exp_l)
        plot(x, y, main = "Densitatea", xlab = "x",ylab = "f(x)", type = "l", col = "lightseagreen",bty = "n", lwd=1)
    })
}
O_ContExponFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = 0, to = 100, by = 0.01)
            y = pexp(x, input$c_exp_l)
            plot(x, y, type = 'l',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", bty = "n", cex = 0.5)
        }),
        #P(X <= a)
        renderPlot({
            x = seq(from = 0, to = 100, by = 0.01)
            y = dexp(x, input$c_exp_l)
            plot(x, y, main = paste("P( X <= ", input$c_exp_a, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            
            x1 = seq(from = 0, to = input$c_exp_a, by = 0.01)
            p1 = dexp(x1, input$c_exp_l)
            xx1 = c(0, x1, input$c_exp_a)
            yy1 = c(0, p1, 0)
            polygon(xx1, yy1, col="purple", density = 10)
        }),
        #P(X >= b)
        renderPlot({
            x = seq(from = 0, to = 100, by = 0.01)
            y = dexp(x, input$c_exp_l)
            plot(x, y, main = paste("P( X >= ", input$c_exp_b, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            x2 = seq(from = input$c_exp_b, to = 100, by = 0.01)
            p2 = dexp(x2, input$c_exp_l)
            xx2 = c(input$c_exp_b, x2, 100)
            yy2 = c(0, p2, 0)
            polygon(xx2, yy2, col="red", density = 10, angle = 90)
        }),
        # P(c <= X <= d)
        renderPlot({
            x = seq(from = 0, to = 100, by = 0.01)
            y = dexp(x, input$c_exp_l)
            plot(x, y, main = paste("P(", input$c_exp_c, " <= X <= ", input$c_exp_d, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
                
            if (input$c_exp_c < input$c_exp_d){
                x3 = seq(from = input$c_exp_c, to = input$c_exp_d, by = 0.01)
                p3 = dexp(x3, input$c_exp_l)
                xx3 = c(input$c_exp_c, x3, input$c_exp_d)
                yy3 = c(0, p3, 0)
                polygon(xx3, yy3, col="orange", density = 10, angle = 135)
            } else {
                legend("top", "Input invalid", fill= "red")
            }
        })
    )
}


ContExpon <- function(id_rep, input, output, label = "ContExpon"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>Varianta continua a repartitiei Geometrice. 
                Modeleaza timpul de asteptare pana la aparitia unui eveniment. Aceasta repatitie primeste ca parametru 'lambda'.</h4>"
            )
        ), hr(),
        fluidRow(
            I_ContExponParams(id_rep, input)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_ContExponProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_ContExponProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_ContExponProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_ContExponFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_ContExponFuncRep(input)
        )
    )
}

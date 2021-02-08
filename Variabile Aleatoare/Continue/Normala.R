library(shiny)

I_ContNormalaParams <- function(id, input) {
    tagList(
        # parametru niu
        column(6, sliderInput(paste(id, "_n", sep =""), "miu:", min = 0, max = 100, value = 10, 1)),
        # parametru sigma
        column(6, sliderInput(paste(id, "_s", sep =""), HTML("sigma<sup>2</sup>:"), min = 0, max = 100, value = 20, 1))
    )
}
IO_ContNormalaProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = -100, max = 100, -10, 0.1)),
        # output
        column(6, br(), br(), renderPrint({
            paste("P(X <= ", input$c_norm_a, ") = ",  pnorm(input$c_norm_a, input$c_norm_n, input$c_norm_s))
        }))
    )
}
IO_ContNormalaProbB <- function(id, input){
    tagList(
        # input b
        column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = -100, max = 100, 40, 0.1)),
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - pnorm(input$c_norm_b, input$c_norm_n, input$c_norm_s)
            paste("P(X >= ", input$c_norm_b, ") = ",  p)
        }))
    )
}
IO_ContNormalaProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",6
                                      ,-100,100)),
               # input d
               column(4,  numericInput(paste(id, "_d", sep =""),"x <= d:", 30, -100, 100))
        ),
        # output
        column(6, br(), renderPrint({
            if (input$c_norm_c < input$c_norm_d) 
                p = pnorm(input$c_norm_d, input$c_norm_n, input$c_norm_s) - 
                    pnorm(input$c_norm_c, input$c_norm_n, input$c_norm_s)
            else 
                p = "Input invalid"
            paste("P(", input$c_norm_c, " <= X <=", input$c_norm_d, ")= ", p)
        }))
    )
}
O_ContNormalaFuncMasa <- function(input){
    renderPlot({
        x = seq(from = -100, to = 100, by = 0.01)
        y = dnorm(x, input$c_norm_n, input$c_norm_s)
        plot(x, y, main = "Densitatea", xlab = "x",ylab = "f(x)", type = "l", col = "lightseagreen",bty = "n", lwd=1)
    })
}
O_ContNormalaFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = -100, to = 100, by = 0.01)
            y = pnorm(x, input$c_norm_n, input$c_norm_s)
            plot(x, y, type = 'l',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", bty = "n", cex = 0.5)
        }),
        #P(X <= a)
        renderPlot({
            x = seq(from = -100, to = 100, by = 0.01)
            y = dnorm(x, input$c_norm_n, input$c_norm_s)
            plot(x, y, main = paste("P( X <= ", input$c_norm_a, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            x1 = seq(from = -100, to = input$c_norm_a, by = 0.01)
            p1 = dnorm(x1, input$c_norm_n, input$c_norm_s)
            xx1 = c(-100, x1, input$c_norm_a)
            yy1 = c(0, p1, 0)
            polygon(xx1, yy1, col="purple", density = 10)
        }),
        #P(X >= b)
        renderPlot({
            x = seq(from = -100, to = 100, by = 0.01)
            y = dnorm(x, input$c_norm_n, input$c_norm_s)
            plot(x, y, main = paste("P( X >= ", input$c_norm_b, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            x2 = seq(from = input$c_norm_b, to = 100, by = 0.01)
            p2 = dnorm(x2, input$c_norm_n, input$c_norm_s)
            xx2 = c(input$c_norm_b, x2, 100)
            yy2 = c(0, p2, 0)
            polygon(xx2, yy2, col="red", density = 10, angle = 90)
        }),
        # P(c <= X <= d)
        renderPlot({
            x = seq(from = -100, to = 100, by = 0.01)
            y = dnorm(x, input$c_norm_n, input$c_norm_s)
            plot(x, y, main = paste("P(", input$c_norm_c, " <= X <= ", input$c_norm_d, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            if (input$c_norm_c < input$c_norm_d) {
                x3 = seq(from = input$c_norm_c, to = input$c_norm_d, by = 0.01)
                p3 = dnorm(x3, input$c_norm_n, input$c_norm_s)
                xx3 = c(input$c_norm_c, x3, input$c_norm_d)
                yy3 = c(0, p3, 0)
                polygon(xx3, yy3, col="orange", density = 10, angle = 135)
            }  else {
                legend("top", "Input invalid", fill= "red")
            }
        })
    )
}


ContNormala <- function(id_rep, input, output, label = "ContNormala"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>V.a. X este repartizata normal (Gaussian) de parametrii 'miu' (media) si 'sigma<sup>2</sup>'(varianta).</h4>"
            )
        ), hr(),
        fluidRow(
            I_ContNormalaParams(id_rep, input)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_ContNormalaProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_ContNormalaProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_ContNormalaProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_ContNormalaFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_ContNormalaFuncRep(input)
        )
    )
}

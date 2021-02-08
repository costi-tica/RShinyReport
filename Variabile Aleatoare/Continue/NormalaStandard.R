library(shiny)

I_ContNormalaStandParams <- function(id, input) {
    tagList(
        # parametru niu
        column(6, numericInput(paste(id, "_n", sep =""), "miu:", min = 0, max = 0, value = 0)),
        # parametru sigma
        column(6, numericInput(paste(id, "_s", sep =""), "sigma:", min = 1, max = 1, value = 1))
    )
}
IO_ContNormalaStandProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = -5, max = 5, 0, 0.1)),
        # output
        column(6, br(), br(), renderPrint({
            paste("P(X <= ", input$c_stnorm_a, ") = ",  pnorm(input$c_stnorm_a, input$c_stnorm_n, input$c_stnorm_s))
        }))
    )
}
IO_ContNormalaStandProbB <- function(id, input){
    tagList(
        # input b
        column(6,  sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = -5, max = 5, 0, 0.1)),
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - pnorm(input$c_stnorm_b, input$c_stnorm_n, input$c_stnorm_s)
            paste("P(X >= ", input$c_stnorm_b, ") = ",  p)
        }))
    )
}
IO_ContNormalaStandProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:",-1 ,-5,5)),
               # input d
               column(4,  numericInput(paste(id, "_d", sep =""),"x <= d:", 0, -5, 5))
        ),
        # output
        column(6, br(), renderPrint({
            if (input$c_stnorm_c < input$c_stnorm_d) 
                p = pnorm(input$c_stnorm_d, input$c_stnorm_n, input$c_stnorm_s) - 
                    pnorm(input$c_stnorm_c, input$c_stnorm_n, input$c_stnorm_s)
            else 
                p = "Input invalid"
            paste("P(", input$c_stnorm_c, " <= X <=", input$c_stnorm_d, ")= ", p)
        }))
    )
}
O_ContNormalaStandFuncMasa <- function(input){
    renderPlot({
        x = seq(from = -5, to = 5, by = 0.01)
        y = dnorm(x, input$c_stnorm_n, input$c_stnorm_s)
        plot(x, y, main = "Densitatea", xlab = "x",ylab = "f(x)", type = "l", col = "lightseagreen",bty = "n", lwd=1)
    })
}
O_ContNormalaStandFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = -5, to = 5, by = 0.01)
            y = pnorm(x, input$c_stnorm_n, input$c_stnorm_s)
            plot(x, y, type = 'l',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", bty = "n", cex = 0.5)
        }),
        #P(X <= a)
        renderPlot({
            x = seq(from = -5, to = 5, by = 0.01)
            y = dnorm(x, input$c_stnorm_n, input$c_stnorm_s)
            plot(x, y, main = paste("P( X <= ", input$c_stnorm_a, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            x1 = seq(from = -5, to = input$c_stnorm_a, by = 0.01)
            p1 = dnorm(x1, input$c_stnorm_n, input$c_stnorm_s)
            xx1 = c(-5, x1, input$c_stnorm_a)
            yy1 = c(0, p1, 0)
            polygon(xx1, yy1, col="purple", density = 10)
        }),
        #P(X >= b)
        renderPlot({
            x = seq(from = -5, to = 5, by = 0.01)
            y = dnorm(x, input$c_stnorm_n, input$c_stnorm_s)
            plot(x, y, main = paste("P( X >= ", input$c_stnorm_b, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            x2 = seq(from = input$c_stnorm_b, to = 5, by = 0.01)
            p2 = dnorm(x2, input$c_stnorm_n, input$c_stnorm_s)
            xx2 = c(input$c_stnorm_b, x2, 5)
            yy2 = c(0, p2, 0)
            polygon(xx2, yy2, col="red", density = 10, angle = 90)
        }),
        # P(c <= X <= d)
        renderPlot({
            x = seq(from = -5, to = 5, by = 0.01)
            y = dnorm(x, input$c_stnorm_n, input$c_stnorm_s)
            plot(x, y, main = paste("P(", input$c_stnorm_c, " <= X <= ", input$c_stnorm_d, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            if (input$c_stnorm_c < input$c_stnorm_d) {
                x3 = seq(from = input$c_stnorm_c, to = input$c_stnorm_d, by = 0.01)
                p3 = dnorm(x3, input$c_stnorm_n, input$c_stnorm_s)
                xx3 = c(input$c_stnorm_c, x3, input$c_stnorm_d)
                yy3 = c(0, p3, 0)
                polygon(xx3, yy3, col="orange", density = 10, angle = 135)
            }  else {
                legend("top", "Input invalid", fill= "red")
            }
        })
    )
}


ContNormalaStand <- function(id_rep, input, output, label = "ContNormalaStand"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>V.a. X este repartizata normal standard (Gaussian) dacaare ca parametrii 'miu' = 0 si 'sigma<sup>2</sup>' = 1.</h4>"
            )
        ), hr(),
        fluidRow(
            I_ContNormalaStandParams(id_rep, input)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_ContNormalaStandProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_ContNormalaStandProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_ContNormalaStandProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_ContNormalaStandFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_ContNormalaStandFuncRep(input)
        )
    )
}

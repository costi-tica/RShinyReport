library(shiny)

I_ContLogNormalaParams <- function(id, input) {
    tagList(
        # parametru niu
        column(6, sliderInput(paste(id, "_n", sep =""), "miu:", min = 0, max = 50, value = 5, 0.1)),
        # parametru sigma
        column(6, sliderInput(paste(id, "_s", sep =""), HTML("sigma<sup>2</sup>:"), min = 0, max = 50, value = 2
                              , 0.1))
    )
}
IO_ContLogNormalaProbA <- function(id, input){
    tagList(
        # input a
        column(6, sliderInput(paste(id, "_a", sep =""), "P(x <= a):", min = 0, max = 100, 10, 0.1)),
        # output
        column(6, br(), br(), renderPrint({
            paste("P(X <= ", input$c_lognorm_a, ") = ",  plnorm(input$c_lognorm_a, input$c_lognorm_n, input$c_lognorm_s))
        }))
    )
}
IO_ContLogNormalaProbB <- function(id, input){
    tagList(
        # input b
        column(6, sliderInput(paste(id, "_b", sep =""),"P(x >= b):", min = 0, max = 100, 40, 0.1)),
        # output 
        column(6, br(), br(), renderPrint({
            p = 1 - plnorm(input$c_lognorm_b, input$c_lognorm_n, input$c_lognorm_s)
            paste("P(X >= ", input$c_lognorm_b, ") = ",  p)
        }))
    )
}
IO_ContLogNormalaProbCD <- function(id, input){
    tagList(
        column(6,
               # input c
               column(4, numericInput(paste(id, "_c", sep =""),"x >= c:", 6, 0, 100)),
               # input d
               column(4,  numericInput(paste(id, "_d", sep =""),"x <= d:", 30, 0, 100))
        ),
        # output
        column(6, br(), renderPrint({
            if (input$c_lognorm_c < input$c_lognorm_d) 
                p = plnorm(input$c_lognorm_d, input$c_lognorm_n, input$c_lognorm_s) - 
                    plnorm(input$c_lognorm_c, input$c_lognorm_n, input$c_lognorm_s)
            else 
                p = "Input invalid"
            paste("P(", input$c_lognorm_c, " <= X <=", input$c_lognorm_d, ")= ", p)
        }))
    )
}
O_ContLogNormalaFuncMasa <- function(input){
    renderPlot({
        x = seq(from = 0, to = 100, by = 0.01)
        y = dlnorm(x, input$c_lognorm_n, input$c_lognorm_s)
        plot(x, y, main = "Densitatea", xlab = "x",ylab = "f(x)", type = "l", col = "lightseagreen",bty = "n", lwd=1)
    })
}
O_ContLogNormalaFuncRep <- function(input){
    tagList(
        renderPlot({
            # graficul functiei de repartitie
            x = seq(from = 0, to = 100, by = 0.01)
            y = plnorm(x, input$c_lognorm_n, input$c_lognorm_s)
            plot(x, y, type = 'l',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", bty = "n", cex = 0.5)
        }),
        #P(X <= a)
        renderPlot({
            x = seq(from = 0, to = 100, by = 0.01)
            y = dlnorm(x, input$c_lognorm_n, input$c_lognorm_s)
            plot(x, y, main = paste("P( X <= ", input$c_lognorm_a, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            x1 = seq(from = 0, to = input$c_lognorm_a, by = 0.01)
            p1 = dlnorm(x1, input$c_lognorm_n, input$c_lognorm_s)
            xx1 = c(0, x1, input$c_lognorm_a)
            yy1 = c(0, p1, 0)
            polygon(xx1, yy1, col="purple", density = 10)
        }),
        #P(X >= b)
        renderPlot({
            x = seq(from = 0, to = 100, by = 0.01)
            y = dlnorm(x, input$c_lognorm_n, input$c_lognorm_s)
            plot(x, y, main = paste("P( X >= ", input$c_lognorm_b, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            x2 = seq(from = input$c_lognorm_b, to = 100, by = 0.01)
            p2 = dlnorm(x2, input$c_lognorm_n, input$c_lognorm_s)
            xx2 = c(input$c_lognorm_b, x2, 100)
            yy2 = c(0, p2, 0)
            polygon(xx2, yy2, col="red", density = 10, angle = 90)
        }),
        # P(c <= X <= d)
        renderPlot({
            x = seq(from = 0, to = 100, by = 0.01)
            y = dlnorm(x, input$c_lognorm_n, input$c_lognorm_s)
            plot(x, y, main = paste("P(", input$c_lognorm_c, " <= X <= ", input$c_lognorm_d, ")"), xlab = "x",ylab = "F(x)", type = "l", col = "lightseagreen", bty= "n")
            
            if (input$c_lognorm_c < input$c_lognorm_d) {
                x3 = seq(from = input$c_lognorm_c, to = input$c_lognorm_d, by = 0.01)
                p3 = dlnorm(x3, input$c_lognorm_n, input$c_lognorm_s)
                xx3 = c(input$c_lognorm_c, x3, input$c_lognorm_d)
                yy3 = c(0, p3, 0)
                polygon(xx3, yy3, col="orange", density = 10, angle = 135)
            }  else {
                legend("top", "Input invalid", fill= "red")
            }
        })
    )
}


ContLogNormala <- function(id_rep, input, output, label = "ContLogNormala"){
    tagList(
        fluidRow(
            HTML(
                "<h4 style='padding:15px; margin:0;'>V.a. X este repartizata Log Normal de parametrii 'miu' (media) si 'sigma<sup>2</sup>'(varianta).</h4>
                <h4 style='padding:15px; margin:0;'> X are densitatea: f(x) = 1/(&radic;(2 &piv;) &sigma; x) e^-((log x - &mu;)^2 / (2 &sigma;^2))</h4>"
            )
        ), hr(),
        fluidRow(
            I_ContLogNormalaParams(id_rep, input)
        ), hr(),
        # CALC PROB P(X <= a)
        fluidRow(
            IO_ContLogNormalaProbA(id_rep, input)
        ),
        # CALC PROB P(X >= b)
        fluidRow(
            IO_ContLogNormalaProbB(id_rep, input)
        ),
        # CALC PROB P(c <= X <= d)
        fluidRow(
            IO_ContLogNormalaProbCD(id_rep, input)
        ),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_ContLogNormalaFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_ContLogNormalaFuncRep(input)
        )
    )
}

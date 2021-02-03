library(shiny)

I_BernoulliParams <- function(id){
    # parametru P
    column(6, sliderInput(paste(id, "_p", sep =""), "p:", min = 0, max = 1, 0.5, 0.01))
}
O_BernoulliFuncMasa <- function(input){
    renderPlot({
        F_mas = function(x){ if (x == 0) 1 - input$bern_p else if (x == 1) input$bern_p else 0 }
        barplot(c(F_mas(0), F_mas(1)),
                names.arg = c(0,1),
                col = 'lightseagreen',
                main = "Functia de Masa",
        )
    })
}
O_BernoulliFuncRep <- function(input){
    tagList(
        renderPlot({
            F_rep = function(x){ if (x < 0) 0 else if (x < 1) 1 - input$bern_p else 1}
            F_rep = Vectorize(F_rep, "x")
            
            x = seq(from = -5, to = 5, by = 0.01)
            y1 = F_rep(x)
            plot(x, y1, type = 'p',main = "Functia de Repartitie",xlab = "x",ylab = "F(x)",col = "black", pch = 19,bty = "n", cex = 0.5)
        })
    )
}


Bernoulli <- function(id_rep, input, output, label = "Bernoulli"){
    tagList(
        fluidRow(
            I_BernoulliParams(id_rep)
        ), hr(),
        # FUNCTIA DE MASA: P(X = x)
        fluidRow(
            O_BernoulliFuncMasa(input)
        ),
        # FUNCTIA DE REPARTITIE
        fluidRow(
            O_BernoulliFuncRep(input)
        )
    )
}

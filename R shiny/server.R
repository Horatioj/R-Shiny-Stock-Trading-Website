library(shiny)
library(plotly)
library(quantmod)

# Server logic
server <- 
  function(input, output, session) {
    stock_df <- shiny::reactiveValues()
    dataInput <- reactive({
      if(input$market == "United States"){
        getSymbols(input$stock1, src = "yahoo",
                   from = input$dates2[1],
                   to = input$dates2[2],
                   auto.assign = FALSE)
      }else{
        getSymbols(input$stock, src = "yahoo", 
                   from = input$dates1[1],
                   to = input$dates1[2],
                   auto.assign = FALSE)
      }
    })
    benchmark <- reactive({
      if(input$market == "United States"){
        getSymbols("^GSPC", src = "yahoo",
                   from = input$dates2[1],
                   to = input$dates2[2],
                   auto.assign = FALSE) %>% 
          na.omit()
      }else{
        getSymbols("000001.ss", src = "yahoo",
                   from = input$dates1[1],
                   to = input$dates1[2],
                   auto.assign = FALSE) %>% 
          na.omit()
      }
    })
    result <- reactive({
      f <- find_function(input$stra1, input$stra2)
      ret(perf(f(dataInput())))
    })
    ben.res <- reactive({
      f <- find_function(input$stra1, input$stra2)
      ret(perf(f(benchmark())))
    })
# 
#     output$his_plot <- renderPlot({
#       stock_df <- dataInput()
#       
#       ggplot() +
#         geom_line(aes(x=index(stock_df), y=Cl(stock_df)), data = fortify(data.frame(stock_df)))+
#       # geom_line(aes(x=index(bands), y=data.frame(bands)[,1], color="down"), data = fortify(data.frame(data.frame(bands)[,1])))+
#       # geom_line(aes(x=index(bands), y=data.frame(bands)[,3], color="up"), data = fortify(data.frame(data.frame(bands)[,3])))+
#         geom_point(aes(x=result()[,1], y=result()[,3], color="buy"), data = result(), size = 3)+
#         geom_point(aes(x=result()[,2], y=result()[,4], color="sell"), data = result(), size = 3)+
#         scale_x_date(labels = date_format("%Y-%m"),date_breaks="6 month", limits = c(index(stock_df)[1], last(index(stock_df))))+
#         xlab("")+
#         ylab("Price")+
#         ggtitle(paste("Stock Performance: ",ifelse(input$market == "China",input$stock,input$stock1)))
#       
#     })
    
    output$his_plot <- plotly::renderPlotly({
      stock_df <- dataInput()
      
      ggplot() +
        geom_line(aes(x=index(stock_df), y=Cl(stock_df)), data = fortify(data.frame(stock_df)))+
        # geom_line(aes(x=index(bands), y=data.frame(bands)[,1], color="down"), data = fortify(data.frame(data.frame(bands)[,1])))+
        # geom_line(aes(x=index(bands), y=data.frame(bands)[,3], color="up"), data = fortify(data.frame(data.frame(bands)[,3])))+
        geom_point(aes(x=result()[,1], y=result()[,3], color="buy"), data = result(), size = 1)+
        geom_point(aes(x=result()[,2], y=result()[,4], color="sell"), data = result(), size = 1)+
        scale_x_date(labels = date_format("%Y-%m"),date_breaks="12 month", limits = c(index(stock_df)[1], last(index(stock_df))))+
        xlab("")+
        ylab("Price")+
        ggtitle(paste("Stock Performance: ",ifelse(input$market == "China",input$stock,input$stock1)))
      
    })
    
    output$regraph <- renderPlot({
      charts.PerformanceSummary(d2xts(result()))
    })
    
    output$retplot <- plotly::renderPlotly({
      #stock_df <- dataInput()
      ggplot() +
        geom_line(aes(x=result()[,1], y=d2xts(result()), color="stock return"), data = result())+
        geom_line(aes(x=ben.res()[,1], y=d2xts(ben.res()), color="benchmark return"), data = ben.res())+
        #scale_x_date(labels = date_format("%Y-%m"),date_breaks="12 month", limits = c(index(stock_df)[1], last(index(stock_df))))+
        xlab("Time")+
        ylab("Return")+
        ggtitle(paste0("Stock Performance against benchmark: ", ifelse(input$market == "China","Shanghai Composite Index", "S&P 500 Index")))
    })
    
    # output$regraph <- plotly::renderPlotly({
    #   charts.PerformanceSummary(d2xts(result()))
    # })
    # 
    output$retable <- renderDataTable({
      cbind(result()[,1:2],apply(result()[,3:4],2,round,4),apply(result()[,5:7],2,round,4))
    })
    
    output$retable2 <- renderDataTable({
        result.ret <- d2xts(result())
        data.frame(alpha = CAPM.alpha(result.ret, Return.calculate(benchmark()[,1], method = "discrete")), 
                   beta = CAPM.beta(result.ret, Return.calculate(benchmark()[,1], method = "discrete")),
                   Sharpe = SharpeRatio(result.ret, FUN = "StdDev"), 
                   Sortino = SortinoRatio(result.ret, MAR = threshold),
                   Omega = Omega(result.ret, 0)[,1],
                   MaxDrawdown = maxDrawdown(result.ret),
                   Volatility = StdDev(result.ret),
                   DownsideDev = DownsideDeviation(result.ret, MAR = threshold),
                   InfoRatio = InformationRatio(result.ret, d2xts(ben.res()))) %>% 
          cbind(stat_table(result()),.) %>%round(4) %>%t()
    })
    
    output$downloadData1 <- downloadHandler(
      filename = function(){
        paste(input$stock,Sys.Date(),".csv",sep = "")
      },
      content = function(file){
        write.csv(dataInput(),file)
      }
    )
    output$downloadData2 <- downloadHandler(
      filename = function(){
        paste(input$stock1,Sys.Date(),".csv",sep = "")
      },
      content = function(file){
        write.csv(dataInput(),file)
      }
    )
    
  }

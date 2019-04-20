library(neuralnet)
library(rio)
library(shiny)
library(stringr)

dataset  <<- NULL;

shinyServer(function(input, output){
  #------------------------------------------------------------------------------>
  #Lê e mostra os dados do Arquivo
  output$tabela <- DT::renderDataTable({
    if(is.null(input$inicio)){
      return(NULL)
    }
    if((input$inicio == 0)){
      return(NULL)
    }
    if(input$inicio > 0){
      withProgress(message="Processando o Arquivo!", value=5, {
        inFile <- input$file1
        
        if(is.null(inFile))
          return( NULL)
       # else(incProgress(0.2, detail ="Analisando os Dados"))
        dataset <<- import(inFile$datapath)
        
        #Retorna a tabela paginada
        return(DT::datatable(dataset))
      })
    }
  }) #Fim output$tbela
  #<------------------------------------------------------------------------------
  
  #------------------------------------------------------------------------------>
  #Possibilita selecionar os atributos (checkboxes) e classe (radiobutton)
  output$atributos <- renderUI({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)

    dataset <<- import(inFile$datapath)
    
    #Retorna checkboxes e radiobutton
    return(
      div(h3("Selecionar Variáveis"),
        div(
          h5("Desmarque o atributo que será a 'Classe' na coluna da esquerda e marque-o na da direita: "),
          style="color:red; font-weight:200; text-align:justify"
        ),
        splitLayout(
          (checkboxGroupInput(
            "atributos",
            label=h5("Atributos"), choices = colnames(dataset), selected = colnames(dataset)
          )),
          (radioButtons(
            "classe", label=h5("Classe"),
            choices = colnames(dataset)
          ))
        )
      )
    );
  }) #Fim output$atributos
  #<-------------------------------------------------------------------------------
  
  #------------------------------------------------------------------------------->
  #Define o botão (e talvez mais coisas) para iniciar
  output$iniciar <- renderUI({
      #Botão para Rodar
      actionButton("inicio", label= "Iniciar!")
  }) #Fim output$inicio
  #<-------------------------------------------------------------------------------
  
  #------------------------------------------------------------------------------->
  #Parte onde é criada, treinada e exibida a rede neural e a tabela de predições
  output$rede_neural <- DT::renderDataTable({
    if(is.null(input$inicio)){
      return(NULL)
    }
    if((input$inicio == 0)){
      return(NULL)
    }
    if(input$inicio > 0){
      withProgress(message="Processando o Arquivo!", value=0.1, {
        dataset <- import(input$file1$datapath)
        
        #Criando a fórmula que pega os nomes dos atributos e concatena cada um com o símbolo '+'
        formula = str_c(input$atributos[1:length(input$atributos)],
                        collapse = "+");
        
        #Barra para exibir o progresso
        incProgress(0.2, detail ="Analisando os Dados");
        
        #Ter o índice dos valores utilizados para treinamento
        index = sample(1:nrow(dataset), round(0.75 * nrow(dataset)))
        #index = sample(seq_len(nrow(dataset)), size = 0.80 * nrow(dataset))
        treino <<- dataset[ index, ];
        teste <<- dataset[ -index, ];
        
        incProgress(0.2, detail ="Criando o Modelo");
          
        NN <<- neuralnet(
                  str_c(input$classe," ~ ", formula), treino, 
                  hidden = 0, stepmax = 10^(5),
                  linear.output=T, lifesign="minimal", threshold = 0.01
                );
        
        #Progresso do Teste
        incProgress(0.5, detail ="Testando os Dados")
        
        #Obtendo Atributos para o teste
        atributosTeste = subset(teste, select = input$atributos);
        
        #Resultados
        previsao = compute(NN, atributosTeste);
        predicao = (round(previsao$net.result, 5));   
        
        #Colocando as predições ao lado da classe na tabela  
        resultado <- teste[str_c("Valor Predito: ")] <<- predicao;
        DT::datatable(resultado);
        
        hr()
        
        #Calculando erro médio quadrático
        output$resultadoMSE <- renderText({
          NN.MSE = sum((teste$input$classe - previsao$net.result)^2)/nrow(teste)
          
          {return(NN.MSE)};
        })
        
        hr()
        
        output$plotLinha <- renderPlot({
          comp <- plot(testeFORA$Temp_mediaAnual, previsaoFORA$net.result,
                       col = 'red', cex = 2, pch =18,
                       xlab = 'Verdadeiro', ylab = 'Predito', xlim = c(1, 30), ylim = c(1, 30))
          abline(0, 1, lwd = 2)
          return(comp);
        })
        
        #Linha horizontal
        hr()
        
        #PLot da Rede Neural
        output$plot <- renderPlot({
          return(plot(NN));
        })
          
        #Possibilita fazer o download dos Resultados
        output$visao <- renderUI({
          return(
            div(
              str_c("Erro: ", round(NN$result.matrix[1], 4)),
                br(),
                "Etapas: ", NN$result.matrix[3],
                br(),
                br(),
                downloadButton('download', 'Download do Resultado')
            )
          );
        })
        return(teste)
      })
    }
  }) #Fim output$rede_neural
  #<-------------------------------------------------------------------------------
  
  #------------------------------------------------------------------------------->
  #Download do .csv com os resultados
  output$download <- downloadHandler(
    filename = function(){
      str_c('redeNeural_', floor(runif(1,1000,9999)), '.csv', sep='')
    },
    content = function(file){
      write.csv(teste, file)
    }
  ) #Fim do output$download
  #<-------------------------------------------------------------------------------
}) #Fim
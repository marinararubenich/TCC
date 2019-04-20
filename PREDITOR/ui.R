library(shiny)
library(shinythemes)

shinyUI(bootstrapPage(theme = shinytheme("sandstone"),
  navbarPage("Preditor de Paleotemperaturas", #helpText("FORAMINÍFEROS"),
    tabPanel("Início", #Mostra um plot da RNA
      tabsetPanel( 
        tabPanel(title="Criar Rede Neural Artificial",
          sidebarPanel(
            h3("Arquivo:"),
              #Selecionar o arquivo necessário
              fileInput('file1', h5('Selecione o Arquivo no seu computador: ')),
              tags$hr(),
                   
            uiOutput(outputId = "atributos"),
            conditionalPanel(
              condition = "output.atributos !== null",
              hr(),
              uiOutput(outputId = "iniciar"), width = 5
            )
          ),
          mainPanel(
            #Será mostrado após ler os dados do arquivo
            conditionalPanel(
              condition = "output.atributos !== null",
              div(h4("Dados lidos: "),
                #Tabela paginada do arquivo                                                                    
                DT::dataTableOutput(outputId = "tabela"),
                hr(),
                h4("Download"), uiOutput(outputId = "visao"),
                hr()
              )
            )
          ) #Fim do mainPanel
        ), #Fim do tabsetPanel
        tabPanel("Ver Estatísticas",
          h4("Erro Médio Quadrático (MSE): "), textOutput(outputId = "resultadoMSE"),
          hr(),
          h4("Plot da Rede Neural"), plotOutput("plot"),
          hr(),
          h4("Plot de Comparação"), plotOutput("plotLinha"),
          hr(),
          hr(),
          h4("Resultados"), DT::dataTableOutput(outputId = "rede_neural")
        )
      ) #Fim do tabsePanel
    ), #Fim do tabPanel
    navbarMenu("Ajuda",
      tabPanel("Sobre"
      ),
      tabPanel("Como Utilizar"
      ),
      tabPanel("Resultados"
      )  
    ),
    tabPanel("Contato",
      verbatimTextOutput("contato")
    )
  ) #Fim do navbarPage
)) #Fim
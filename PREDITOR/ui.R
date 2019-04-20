library(shiny)
library(shinythemes)

shinyUI(bootstrapPage(theme = shinytheme("sandstone"),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  navbarPage("Preditor de Paleotemperaturas", #helpText("FORAMINÍFEROS"),
    tabPanel("Início", #Mostra um plot da RNA
      tabsetPanel( 
        tabPanel(title="Criar Rede Neural Artificial",
          sidebarPanel(
            h3("Arquivo:"),
            #Selecionar o arquivo necessário
            fileInput('file1', h5('Selecione o Arquivo no seu computador: ')),
                   
            uiOutput(outputId = "atributos"),
            conditionalPanel(
              condition = "output.atributos !== null",
              uiOutput(outputId = "inicio")
            )
          ),
          mainPanel(
            #Será mostrado após ler os dados do arquivo
            conditionalPanel(
              condition = "output.atributos !== null",
              div(h4("Dados lidos: "),
                #Tabela paginada do arquivo                                                                    
                DT::dataTableOutput(outputId = "tabela"),
                hr()
              )
            )
          ) #Fim do mainPanel
        ), #Fim do tabsetPanel
        tabPanel("Previsão",
          #conditionalPanel(
            #condition = "output.inicio > 0",
          h4(strong("Resultados (atributos normalizados)"), align = "center"), DT::dataTableOutput(outputId = "rede_neural"),
          br()
        ),
        tabPanel("Plots",
          h4(strong("Plot da Rede Neural"), align = "center"), plotOutput("plot"),
          br(),
          hr(),
          h4(strong("Plot de Comparação"), align = "center"), plotOutput("plotLinha"),
          br()
        ),
        tabPanel("Números e Download",
          h3(strong("Estatísticas: "), align = "center"), uiOutput(outputId = "visao"), renderTable("matrizC"),
          br()          
        )
      ) #Fim do tabsePanel
    ), #Fim do tabPanel
    navbarMenu("Ajuda",
      tabPanel("Sobre",
        uiOutput(outputId = "sobre")
      ),
      tabPanel("Como Utilizar",
        uiOutput(outputId = "manual")    
      ),
      tabPanel("Resultados",
        uiOutput(outputId = "resultados")
      )  
    ),
    tabPanel("Contato",
      uiOutput(outputId = "contato")
    )
  ) #Fim do navbarPage
)) #Fim
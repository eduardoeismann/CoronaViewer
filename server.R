library(shiny)
library(ggplot2)
library(plotly)
library(intrval)
source("data.R")

server <- function(input, output) {
  
# -- VARIAVEIS E DADOS PARA AREA DE VISAO GERAL --------------------------------
    # COMO HÁ DIFERENTES FORMAS DE CHAMAR O MESMO EXAME NOS DIFERENTES HOSPITAIS
    # VI NECESSÁRIO CRIAR UMA VARIÁVEL PARA RECEBER CADA UM DOS TIPOS DE EXAMES.
    # LEMBRANDO QUE AQUI, HÁ EXAMES DE QUATRO HOSPITAIS DIFERENTES E EXAMES COM
    # NOMES SIMILARES.

    exameColetadoNomenclatura01 <- sum(dadosExames$DE_ANALITO == "CoronavirusNL63")
    exameColetadoNomenclatura02 <- sum(dadosExames$DE_ANALITO == "Resultado COVID-19:")
    exameColetadoNomenclatura03 <- sum(dadosExames$DE_ANALITO == "CoronavirusOC43")
    exameColetadoNomenclatura04 <- sum(dadosExames$DE_ANALITO == "Coronavirus229E")
    exameColetadoNomenclatura05 <- sum(dadosExames$DE_ANALITO == "CoronavirusHKU1")
    exameColetadoNomenclatura06 <- sum(dadosExames$DE_ANALITO == "COVID IgG Interp aqui")
    exameColetadoNomenclatura07 <- sum(dadosExames$DE_ANALITO == "Covid 19, Deteccao por PCR")
    exameColetadoNomenclatura08 <- sum(dadosExames$DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia")
    exameColetadoNomenclatura09 <- sum(dadosExames$DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa")
    exameColetadoNomenclatura10 <- sum(dadosExames$DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa")
    exameColetadoNomenclatura11 <- sum(dadosExames$DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor")
    exameColetadoNomenclatura12 <- sum(dadosExames$DE_ANALITO == "Coronavirus 2019-nCoV")
    exameColetadoNomenclatura13 <- sum(dadosExames$DE_ANALITO == "Coronavirus humano NL63 (Cor63)")
    exameColetadoNomenclatura14 <- sum(dadosExames$DE_ANALITO == "Coronavirus humano OC43 (Cor43)")
    exameColetadoNomenclatura15 <- sum(dadosExames$DE_ANALITO == "Coronavirus humano HKU1 (HKU)")
    exameColetadoNomenclatura16 <- sum(dadosExames$DE_ANALITO == "Coronavirus humano 229E (Cor229)")
    exameColetadoNomenclatura17 <- sum(dadosExames$DE_ANALITO == "Coronavirus (2019-nCoV)")
    
    totalExamesCovidRealizados <- (
        exameColetadoNomenclatura01 + exameColetadoNomenclatura02 +
        exameColetadoNomenclatura03 + exameColetadoNomenclatura04 +
        exameColetadoNomenclatura05 + exameColetadoNomenclatura06 +
        exameColetadoNomenclatura07 + exameColetadoNomenclatura08 +
        exameColetadoNomenclatura09 + exameColetadoNomenclatura10 +
        exameColetadoNomenclatura11 + exameColetadoNomenclatura12 +
        exameColetadoNomenclatura13 + exameColetadoNomenclatura14 +
        exameColetadoNomenclatura15 + exameColetadoNomenclatura16 +
        exameColetadoNomenclatura17
    )
    

    resultadosExamesPositivos <- (
        nrow(examesPOSITIVOS)
    )

    
    resultadosExamesNegativos <- (
        nrow(examesNEGATIVOS)
    )
  
    
  
    output$visaoGeralAnoNascimento <- renderPlot({ #Finalizado
        ggplot(
            dadosPacientes,
            aes( x = AA_NASCIMENTO )
        ) + geom_bar( fill = "#3d3d3d", color = "#ffffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Ano de Nascimento", y = "Quantidade" )
    })
    
    output$visaoGeralSexo <- renderPlot({ #Finalizado
        ggplot(
            dadosPacientes,
            aes( x = IC_SEXO )
        ) + geom_bar( fill = "#6070b2", color = "#00ffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Sexo do Paciente", y = "Quantidade" )
    })
    
    output$visaoGeralPacientesPorEstado <- renderPlot({ #Finalizado
        ggplot(
            dadosPacientes,
            aes( x = CD_UF )
        ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "UF dos Pacientes", y = "Quantidade" )
    })
    
    output$visaoGeralPacientesPorCidade <- renderPlot({ #Finalizado
        ggplot(
            dadosPacientes,
            aes( x = CD_MUNICIPIO )
        ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Cidade dos Pacientes", y = "Quantidade" )
    })
    
# ------------------------------------------------------------------------------
    
    output$pacientes_analisados <- renderValueBox({ #Finalizado
        valueBox(
            nrow(dadosPacientes), "Pacientes Analisados", icon = icon("sort-amount-up"),
            color = "blue"
        )
    })
    
    output$quantidade_homens <- renderValueBox({ #Finalizado
        valueBox(
            sum(dadosPacientes$IC_SEXO == "M"), "Quantidade Homens", icon = icon("mars"),
            color = "purple"
        )
    })
    
    output$quantidade_mulheres <- renderValueBox({ #Finalizado
        valueBox(
            sum(dadosPacientes$IC_SEXO == "F"), "Quantidade Mulheres", icon = icon("venus"),
            color = "purple"
        )
    })
    
    output$exames_coletados <- renderValueBox({ #Finalizado
        valueBox(
            totalExamesCovidRealizados,
            "Exames Válidos Coletados",
            icon = icon("clipboard-list"),
            color = "aqua"
        )
    })
    
    output$quantidade_positivos <- renderValueBox({ #Finalizado
        valueBox(
            resultadosExamesPositivos,
            "Quantidade Positivos",
            icon = icon("biohazard"),
            color = "red"
        )
    })
    
    output$quantidade_negativos <- renderValueBox({ #Finalizado
        valueBox(
            resultadosExamesNegativos,
            "Quantidade Negativos",
            icon = icon("book-medical"),
            color = "green"
        )
    })
    
    output$quantidade_altas <- renderValueBox({ #Finalizado
        alta1 <- sum(dadosDesfecho$DE_DESFECHO == "Alta Administrativa")
        alta2 <- sum(dadosDesfecho$DE_DESFECHO == "Alta medica melhorado")
        alta3 <- sum(dadosDesfecho$DE_DESFECHO == "Alta por abandono")
        alta4 <- sum(dadosDesfecho$DE_DESFECHO == "Alta medica Inalterado")
        alta5 <- sum(dadosDesfecho$DE_DESFECHO == "Alta a pedido")
        alta6 <- sum(dadosDesfecho$DE_DESFECHO == "Alta medica curado")

        valueBox(
            total <- (alta1 + alta2 + alta3 + alta4 + alta5 + alta6),
            "Quantidade Altas",
            icon = icon("hand-holding-medical"),
            color = "green"
        )
    })
    
    output$quantidade_falecidos <- renderValueBox({ #Finalizado
        valueBox(
            obitos <- sum(dadosDesfecho$DE_DESFECHO == "Obito apos 48hs de internacao sem necropsia"),
            "Quantidade Falecidos",
            icon = icon("chart-bar"),
            color = "red"
        )
    })
    
    output$desistencia_tratamento <- renderValueBox({ #Finalizado
        valueBox(
            desistencia <- sum(dadosDesfecho$DE_DESFECHO == "Desistencia do atendimento"),
            "Desistência de Atendimento",
            icon = icon("chart-bar"),
            color = "yellow"
        )
    })
    
    
    
# ------------------------------------------------------------------------------
# -- PACIENTES COM EXAMES ------------------------------------------------------
# ------------------------------------------------------------------------------

    # PACIENTES COM EXAMES POR SEXO
    output$pacienteComExamePorSexo <- renderPlot({
        novo <- subset(
            dadosSelecionados,
            (
                if( input$sexoMasculino == TRUE && input$sexoFeminino == FALSE ) {
                    IC_SEXO == "M"
                } else if( input$sexoMasculino == FALSE && input$sexoFeminino == TRUE ) {
                    IC_SEXO == "F"
                } else {
                    ( IC_SEXO == "M" | IC_SEXO == "F")
                }
            )
            & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames[2])
            & (
                DE_ANALITO == "CoronavirusNL63" | DE_ANALITO == "Resultado COVID-19:" | DE_ANALITO == "CoronavirusOC43" | 
                DE_ANALITO == "Coronavirus229E" | DE_ANALITO == "CoronavirusHKU1" | DE_ANALITO == "COVID IgG Interp aqui" | 
                DE_ANALITO == "Covid 19, Deteccao por PCR" | DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" | DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" | 
                DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" | DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" | DE_ANALITO == "Coronavirus 2019-nCoV" | 
                DE_ANALITO == "Coronavirus humano NL63 (Cor63)" | DE_ANALITO == "Coronavirus humano OC43 (Cor43)" | DE_ANALITO == "Coronavirus humano HKU1 (HKU)" | 
                DE_ANALITO == "Coronavirus humano 229E (Cor229)" | DE_ANALITO == "Coronavirus (2019-nCoV)"
            )
            &( CD_UF == input$buscaEstadoPacientesComExames )
            &( CD_MUNICIPIO == input$buscaCidadePacientesComExames )
        )
      
        ggplot(
            novo,
            aes( x = IC_SEXO )
        ) + geom_bar( fill = "#6070b2", color = "#00ffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Sexo", y = "Quantidade" )
    })
    
    # PACIENTES COM EXAMES POR ESTADO
    output$pacienteComExamePorEstado <- renderPlot({
        novo <- subset(
            dadosSelecionados,
            (CD_UF == input$buscaEstadoPacientesComExames) & 
            (
                if( input$sexoMasculino == TRUE && input$sexoFeminino == FALSE ) {
                    IC_SEXO == "M"
                } else if( input$sexoMasculino == FALSE && input$sexoFeminino == TRUE ) {
                    IC_SEXO == "F"
                } else {
                    ( IC_SEXO == "M" | IC_SEXO == "F")
                }
            )
            & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames[2])
            & (
                DE_ANALITO == "CoronavirusNL63" | DE_ANALITO == "Resultado COVID-19:" | DE_ANALITO == "CoronavirusOC43" | 
                DE_ANALITO == "Coronavirus229E" | DE_ANALITO == "CoronavirusHKU1" | DE_ANALITO == "COVID IgG Interp aqui" | 
                DE_ANALITO == "Covid 19, Deteccao por PCR" | DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" | DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" | 
                DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" | DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" | DE_ANALITO == "Coronavirus 2019-nCoV" | 
                DE_ANALITO == "Coronavirus humano NL63 (Cor63)" | DE_ANALITO == "Coronavirus humano OC43 (Cor43)" | DE_ANALITO == "Coronavirus humano HKU1 (HKU)" | 
                DE_ANALITO == "Coronavirus humano 229E (Cor229)" | DE_ANALITO == "Coronavirus (2019-nCoV)"
            )
            &( CD_UF == input$buscaEstadoPacientesComExames )
            &( CD_MUNICIPIO == input$buscaCidadePacientesComExames )
        )
      
        ggplot(
            novo,
            aes( x = CD_UF )
        ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Unidade Federativa (UF)", y = "Quantidade" )
    })
    
    # PACIENTES COM EXAMES POR CIDADE
    output$pacienteComExamePorCidade <- renderPlot({
        novo <- subset(
            dadosSelecionados,
            (
                if( input$sexoMasculino == TRUE && input$sexoFeminino == FALSE ) {
                    IC_SEXO == "M"
                } else if( input$sexoMasculino == FALSE && input$sexoFeminino == TRUE ) {
                    IC_SEXO == "F"
                } else {
                    ( IC_SEXO == "M" | IC_SEXO == "F")
                }
            )
            & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames[2])
            & (
                DE_ANALITO == "CoronavirusNL63" | DE_ANALITO == "Resultado COVID-19:" | DE_ANALITO == "CoronavirusOC43" | 
                DE_ANALITO == "Coronavirus229E" | DE_ANALITO == "CoronavirusHKU1" | DE_ANALITO == "COVID IgG Interp aqui" | 
                DE_ANALITO == "Covid 19, Deteccao por PCR" | DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" | DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" | 
                DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" | DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" | DE_ANALITO == "Coronavirus 2019-nCoV" | 
                DE_ANALITO == "Coronavirus humano NL63 (Cor63)" | DE_ANALITO == "Coronavirus humano OC43 (Cor43)" | DE_ANALITO == "Coronavirus humano HKU1 (HKU)" | 
                DE_ANALITO == "Coronavirus humano 229E (Cor229)" | DE_ANALITO == "Coronavirus (2019-nCoV)"
            )
            &( CD_UF == input$buscaEstadoPacientesComExames )
            &( CD_MUNICIPIO == input$buscaCidadePacientesComExames )
        )
        
        ggplot(
            novo,
            aes( x = CD_MUNICIPIO )
        ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Cidade Brasileira", y = "Quantidade" )
    })
    
    # PACIENTES COM EXAMES POR ANO DE NASCIMENTO
    output$pacienteComExamePorRangeAnoNascimento <- renderPlot({
        novo <- subset(
            dadosSelecionados,
            (
                if( input$sexoMasculino == TRUE && input$sexoFeminino == FALSE ) {
                    IC_SEXO == "M"
                } else if( input$sexoMasculino == FALSE && input$sexoFeminino == TRUE ) {
                    IC_SEXO == "F"
                } else {
                    ( IC_SEXO == "M" | IC_SEXO == "F")
                }
            )
            & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames[2])
            &(
                DE_ANALITO == "CoronavirusNL63" | DE_ANALITO == "Resultado COVID-19:" | DE_ANALITO == "CoronavirusOC43" | 
                DE_ANALITO == "Coronavirus229E" | DE_ANALITO == "CoronavirusHKU1" | DE_ANALITO == "COVID IgG Interp aqui" | 
                DE_ANALITO == "Covid 19, Deteccao por PCR" | DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" | DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" | 
                DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" | DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" | DE_ANALITO == "Coronavirus 2019-nCoV" | 
                DE_ANALITO == "Coronavirus humano NL63 (Cor63)" | DE_ANALITO == "Coronavirus humano OC43 (Cor43)" | DE_ANALITO == "Coronavirus humano HKU1 (HKU)" | 
                DE_ANALITO == "Coronavirus humano 229E (Cor229)" | DE_ANALITO == "Coronavirus (2019-nCoV)"
            )
            &( CD_UF == input$buscaEstadoPacientesComExames )
            &( CD_MUNICIPIO == input$buscaCidadePacientesComExames )
        )

        ggplot(
            novo,
            aes( x = AA_NASCIMENTO )
        ) + geom_bar( fill = "#3d3d3d", color = "#ffffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Ano Nascimento", y = "Quantidade" )
    })
    
# ------------------------------------------------------------------------------
    
    
    
# ------------------------------------------------------------------------------
# -- NEGATIVO -------------------------------------------------------
# ------------------------------------------------------------------------------
    
    # POSITIVO E NEGATIVO POR SEXO
    output$pacienteComExamePorSexo_teste5 <- renderPlot({
      novo <- subset(
        dadosSelecionados,
          (
            if( input$sexoMasculino_teste5 == TRUE && input$sexoFeminino_teste5 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste5 == FALSE && input$sexoFeminino_teste5 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & ( AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste5[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste5[2] )
          & ( CD_UF == input$buscaEstadoPacientesComExames_teste5 )
          & ( CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste5 )
          & (
              ( DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") )
          )
        
      )
      
      ggplot(
        novo,
        aes( x = IC_SEXO )
      ) + geom_bar( fill = "#6070b2", color = "#00ffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Sexo", y = "Quantidade" )
    })
    
    # POSITIVO E NEGATIVO POR ESTADO
    output$pacienteComExamePorEstado_teste5 <- renderPlot({
      novo <- subset(
        dadosSelecionados,
          (
            if( input$sexoMasculino_teste5 == TRUE && input$sexoFeminino_teste5 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste5 == FALSE && input$sexoFeminino_teste5 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (CD_UF == input$buscaEstadoPacientesComExames_teste5)
          & ( CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste5 )
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste5[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste5[2])
          & (
            ( DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") )
          )
        
      )
      
      ggplot(
        novo,
        aes( x = CD_UF )
      ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Unidade Federativa (UF)", y = "Quantidade" )
    })
    
    # POSITIVO E NEGATIVO POR CIDADE
    output$pacienteComExamePorCidade_teste5 <- renderPlot({
      novo <- subset(
        dadosSelecionados,
          (
            if( input$sexoMasculino_teste5 == TRUE && input$sexoFeminino_teste5 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste5 == FALSE && input$sexoFeminino_teste5 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste5)
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste5[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste5[2])
          & (CD_UF == input$buscaEstadoPacientesComExames_teste5)
          & (
            ( DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
              ( DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") )
          )
        
      )
      
      ggplot(
        novo,
        aes( x = CD_MUNICIPIO )
      ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Cidade Brasileira", y = "Quantidade" )
    })
    
    # POSITIVO E NEGATIVO POR ANO DE NASCIMENTO
    output$pacienteComExamePorRangeAnoNascimento_teste5 <- renderPlot({
      novo <- subset(
        dadosSelecionados,
        (
          if( input$sexoMasculino_teste5 == TRUE && input$sexoFeminino_teste5 == FALSE ) {
            IC_SEXO == "M"
          } else if( input$sexoMasculino_teste5 == FALSE && input$sexoFeminino_teste5 == TRUE ) {
            IC_SEXO == "F"
          } else {
            ( IC_SEXO == "M" | IC_SEXO == "F")
          }
        )
        & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste5[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste5[2])
        & (CD_UF == input$buscaEstadoPacientesComExames_teste5)
        & ( CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste5 )
        & (
          ( DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") ) |
            ( DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)") )
        )
        
      )
      
      ggplot(
        novo,
        aes( x = AA_NASCIMENTO )
      ) + geom_bar( fill = "#3d3d3d", color = "#ffffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Ano Nascimento", y = "Quantidade" )
    })
    
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# -- POSITIVO -------------------------------------------------------
# ------------------------------------------------------------------------------
    
    # POSITIVO E NEGATIVO POR SEXO
    output$pacienteComExamePorSexo_teste52 <- renderPlot({
      novo <- subset(
        dadosSelecionados,
        (
          if( input$sexoMasculino_teste52 == TRUE && input$sexoFeminino_teste52 == FALSE ) {
            IC_SEXO == "M"
          } else if( input$sexoMasculino_teste52 == FALSE && input$sexoFeminino_teste52 == TRUE ) {
            IC_SEXO == "F"
          } else {
            ( IC_SEXO == "M" | IC_SEXO == "F")
          }
        )
        & ( AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste52[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste52[2] )
        & ( CD_UF == input$buscaEstadoPacientesComExames_teste52 )
        & ( CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste52 )
        & (
            ( DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
        )
        
      )
      
      ggplot(
        novo,
        aes( x = IC_SEXO )
      ) + geom_bar( fill = "#6070b2", color = "#00ffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Sexo", y = "Quantidade" )
    })
    
    # POSITIVO E NEGATIVO POR ESTADO
    output$pacienteComExamePorEstado_teste52 <- renderPlot({
      novo <- subset(
        dadosSelecionados,
        (
          if( input$sexoMasculino_teste52 == TRUE && input$sexoFeminino_teste52 == FALSE ) {
            IC_SEXO == "M"
          } else if( input$sexoMasculino_teste52 == FALSE && input$sexoFeminino_teste52 == TRUE ) {
            IC_SEXO == "F"
          } else {
            ( IC_SEXO == "M" | IC_SEXO == "F")
          }
        )
        & (CD_UF == input$buscaEstadoPacientesComExames_teste52)
        & ( CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste52 )
        & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste52[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste52[2])
        & (
          ( DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
        )
        
      )
      
      ggplot(
        novo,
        aes( x = CD_UF )
      ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Unidade Federativa (UF)", y = "Quantidade" )
    })
    
    # POSITIVO E NEGATIVO POR CIDADE
    output$pacienteComExamePorCidade_teste52 <- renderPlot({
      novo <- subset(
        dadosSelecionados,
        (
          if( input$sexoMasculino_teste52 == TRUE && input$sexoFeminino_teste52 == FALSE ) {
            IC_SEXO == "M"
          } else if( input$sexoMasculino_teste52 == FALSE && input$sexoFeminino_teste52 == TRUE ) {
            IC_SEXO == "F"
          } else {
            ( IC_SEXO == "M" | IC_SEXO == "F")
          }
        )
        & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste52)
        & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste52[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste52[2])
        & (CD_UF == input$buscaEstadoPacientesComExames_teste52)
        & (
          ( DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
        )
        
      )
      
      ggplot(
        novo,
        aes( x = CD_MUNICIPIO )
      ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Cidade Brasileira", y = "Quantidade" )
    })
    
    # POSITIVO E NEGATIVO POR ANO DE NASCIMENTO
    output$pacienteComExamePorRangeAnoNascimento_teste52 <- renderPlot({
      novo <- subset(
        dadosSelecionados,
        (
          if( input$sexoMasculino_teste52 == TRUE && input$sexoFeminino_teste52 == FALSE ) {
            IC_SEXO == "M"
          } else if( input$sexoMasculino_teste52 == FALSE && input$sexoFeminino_teste52 == TRUE ) {
            IC_SEXO == "F"
          } else {
            ( IC_SEXO == "M" | IC_SEXO == "F")
          }
        )
        & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste52[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste52[2])
        & (CD_UF == input$buscaEstadoPacientesComExames_teste52)
        & ( CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste52 )
        & (
          ( DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))|
            ( DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
        )
        
      )
      
      ggplot(
        novo,
        aes( x = AA_NASCIMENTO )
      ) + geom_bar( fill = "#3d3d3d", color = "#ffffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Ano Nascimento", y = "Quantidade" )
    })
    
    # ------------------------------------------------------------------------------
    
    
    
# ------------------------------------------------------------------------------
# -- OBITOS ------------------------------------------------------
# ------------------------------------------------------------------------------
    
    # OBITOS E RECUPERADOS POR SEXO
    output$pacienteComExamePorSexo_teste8 <- renderPlot({
      novo <- subset(
          dadosObitosRecuperados,
          (
            if( input$sexoMasculino_teste8 == TRUE && input$sexoFeminino_teste8 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste8 == FALSE && input$sexoFeminino_teste8 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste8[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste8[2])
          & (CD_UF == input$buscaEstadoPacientesComExames_teste8)
          & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste8)
          & (DE_DESFECHO == "Obito apos 48hs de internacao sem necropsia")
      )
      
      ggplot(
        novo,
        aes( x = IC_SEXO )
      ) + geom_bar( fill = "#6070b2", color = "#00ffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Sexo", y = "Quantidade" )
    })
    
    # OBITOS E RECUPERADOS POR ESTADO
    output$pacienteComExamePorEstado_teste8 <- renderPlot({
      novo <- subset(
        dadosObitosRecuperados,
          (
            if( input$sexoMasculino_teste8 == TRUE && input$sexoFeminino_teste8 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste8 == FALSE && input$sexoFeminino_teste8 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (CD_UF == input$buscaEstadoPacientesComExames_teste8)
          & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste8)
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste8[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste8[2])
          & (DE_DESFECHO == "Obito apos 48hs de internacao sem necropsia")
      )
      
      ggplot(
        novo,
        aes( x = CD_UF )
      ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Unidade Federativa (UF)", y = "Quantidade" )
    })
    
    # OBITOS E RECUPERADOS POR CIDADE
    output$pacienteComExamePorCidade_teste8 <- renderPlot({
      novo <- subset(
        dadosObitosRecuperados,
          (
            if( input$sexoMasculino_teste8 == TRUE && input$sexoFeminino_teste8 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste8 == FALSE && input$sexoFeminino_teste8 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste8)
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste8[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste8[2])
          & (CD_UF == input$buscaEstadoPacientesComExames_teste8)
          & (DE_DESFECHO == "Obito apos 48hs de internacao sem necropsia")
      )
      
      ggplot(
        novo,
        aes( x = CD_MUNICIPIO )
      ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Cidade Brasileira", y = "Quantidade" )
    })
    
    # OBITOS E RECUPERADOS POR ANO DE NASCIMENTO
    output$pacienteComExamePorRangeAnoNascimento_teste8 <- renderPlot({
      novo <- subset(
          dadosObitosRecuperados,
          (
            if( input$sexoMasculino_teste8 == TRUE && input$sexoFeminino_teste8 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste8 == FALSE && input$sexoFeminino_teste8 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste8[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste8[2])
          & (CD_UF == input$buscaEstadoPacientesComExames_teste8)
          & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste8)
          & (DE_DESFECHO == "Obito apos 48hs de internacao sem necropsia")
      )
      
      ggplot(
        novo,
        aes( x = AA_NASCIMENTO )
      ) + geom_bar( fill = "#3d3d3d", color = "#ffffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Ano Nascimento", y = "Quantidade" )
    })
    
# ------------------------------------------------------------------------------
    
    
    
# ------------------------------------------------------------------------------
# -- RECUPERADOS ------------------------------------------------------
# ------------------------------------------------------------------------------
    
    # OBITOS E RECUPERADOS POR SEXO
    output$pacienteComExamePorSexo_teste82 <- renderPlot({
      novo <- subset(
          dadosObitosRecuperados,
          (
            if( input$sexoMasculino_teste82 == TRUE && input$sexoFeminino_teste82 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste82 == FALSE && input$sexoFeminino_teste82 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste82[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste82[2])
          & (CD_UF == input$buscaEstadoPacientesComExames_teste82)
          & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste82)
          & ( DE_DESFECHO == "Alta Administrativa" 
             | DE_DESFECHO == "Alta medica melhorado" 
             | DE_DESFECHO == "Alta por abandono" 
             | DE_DESFECHO == "Alta medica Inalterado" 
             | DE_DESFECHO == "Alta a pedido" 
             | DE_DESFECHO == "Alta medica curado" )
      )
      
      ggplot(
        novo,
        aes( x = IC_SEXO )
      ) + geom_bar( fill = "#6070b2", color = "#00ffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Sexo", y = "Quantidade" )
    })
    
    # OBITOS E RECUPERADOS POR ESTADO
    output$pacienteComExamePorEstado_teste82 <- renderPlot({
      novo <- subset(
        dadosObitosRecuperados,
            (
              if( input$sexoMasculino_teste82 == TRUE && input$sexoFeminino_teste82 == FALSE ) {
                IC_SEXO == "M"
              } else if( input$sexoMasculino_teste82 == FALSE && input$sexoFeminino_teste82 == TRUE ) {
                IC_SEXO == "F"
              } else {
                ( IC_SEXO == "M" | IC_SEXO == "F")
              }
            )
            & (CD_UF == input$buscaEstadoPacientesComExames_teste82)
            & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste82)
            & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste82[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste82[2])
            & ( DE_DESFECHO == "Alta Administrativa" 
                | DE_DESFECHO == "Alta medica melhorado" 
                | DE_DESFECHO == "Alta por abandono" 
                | DE_DESFECHO == "Alta medica Inalterado" 
                | DE_DESFECHO == "Alta a pedido" 
                | DE_DESFECHO == "Alta medica curado" )
      )
      
      ggplot(
        novo,
        aes( x = CD_UF )
      ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Unidade Federativa (UF)", y = "Quantidade" )
    })
    
    # OBITOS E RECUPERADOS POR CIDADE
    output$pacienteComExamePorCidade_teste82 <- renderPlot({
      novo <- subset(
        dadosObitosRecuperados,
          (
            if( input$sexoMasculino_teste82 == TRUE && input$sexoFeminino_teste82 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste82 == FALSE && input$sexoFeminino_teste82 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste82)
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste82[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste82[2])
          & (CD_UF == input$buscaEstadoPacientesComExames_teste82)
          & ( DE_DESFECHO == "Alta Administrativa" 
              | DE_DESFECHO == "Alta medica melhorado" 
              | DE_DESFECHO == "Alta por abandono" 
              | DE_DESFECHO == "Alta medica Inalterado" 
              | DE_DESFECHO == "Alta a pedido" 
              | DE_DESFECHO == "Alta medica curado" )
      )
      
      ggplot(
        novo,
        aes( x = CD_MUNICIPIO )
      ) + geom_bar( fill = "#126e36", color = "#c9c900", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Cidade Brasileira", y = "Quantidade" )
    })
    
    # OBITOS E RECUPERADOS POR ANO DE NASCIMENTO
    output$pacienteComExamePorRangeAnoNascimento_teste82 <- renderPlot({
      novo <- subset(
          dadosObitosRecuperados,
          (
            if( input$sexoMasculino_teste82 == TRUE && input$sexoFeminino_teste82 == FALSE ) {
              IC_SEXO == "M"
            } else if( input$sexoMasculino_teste82 == FALSE && input$sexoFeminino_teste82 == TRUE ) {
              IC_SEXO == "F"
            } else {
              ( IC_SEXO == "M" | IC_SEXO == "F")
            }
          )
          & (AA_NASCIMENTO >= input$rangeAnoNascimentoPacientesComExames_teste82[1] & AA_NASCIMENTO <= input$rangeAnoNascimentoPacientesComExames_teste82[2])
          & (CD_UF == input$buscaEstadoPacientesComExames_teste82)
          & (CD_MUNICIPIO == input$buscaCidadePacientesComExames_teste82)
          & ( DE_DESFECHO == "Alta Administrativa" 
              | DE_DESFECHO == "Alta medica melhorado" 
              | DE_DESFECHO == "Alta por abandono" 
              | DE_DESFECHO == "Alta medica Inalterado" 
              | DE_DESFECHO == "Alta a pedido" 
              | DE_DESFECHO == "Alta medica curado" )
      )
      
      ggplot(
        novo,
        aes( x = AA_NASCIMENTO )
      ) + geom_bar( fill = "#3d3d3d", color = "#ffffff", alpha = 0.7, show.legend = TRUE ) +
        labs( x = "Ano Nascimento", y = "Quantidade" )
    })
    
# ------------------------------------------------------------------------------
    
}

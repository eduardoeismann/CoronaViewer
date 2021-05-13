library(shinydashboard)
library(ggplot2)
library(plotly)
source("data.R")

ui <- dashboardPage(
  
    dashboardHeader(
        title = "Corona Viewer"
    ),
  
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Visão Geral",
                tabName = "visaoGeral",
                icon = icon("chart-bar")
            ),
            menuItem(
                "Pacientes com Exames",
                tabName = "pacientesComExames",
                icon = icon("chart-bar")
            ),
            menuItem(
                "Resultados Negativos",
                tabName = "positivosNegativos",
                icon = icon("chart-bar")
            ),
            menuItem(
                "Resultados Positivos",
                tabName = "positivosNegativosPOSITIVOS",
                icon = icon("chart-bar")
            ),
            menuItem(
                "Óbitos",
                tabName = "obitosRecuperacoes",
                icon = icon("chart-bar")
            ),
            menuItem(
                "Recuperações",
                tabName = "obitosRecuperacoesRECUPERACOES",
                icon = icon("chart-bar")
            ),
            menuItem(
                "Sobre",
                tabName = "sobre",
                icon = icon("chart-bar")
            )
        )
    ),

    dashboardBody(
        tabItems(
            tabItem(
                # --------------------------------------------------------------
                # -- VISAO GERAL -----------------------------------------------
                # --------------------------------------------------------------
                tabName = "visaoGeral",
                fluidRow(
                    valueBoxOutput(width = 2, outputId = "pacientes_analisados"),
                    valueBoxOutput(width = 2, outputId = ""),
                    valueBoxOutput(width = 2, outputId = ""),
                    valueBoxOutput(width = 2, outputId = "exames_coletados"),
                    valueBoxOutput(width = 2, outputId = "quantidade_negativos"),
                    valueBoxOutput(width = 2, outputId = "quantidade_positivos")
                ),
                fluidRow(
                    valueBoxOutput(width = 2, outputId = "quantidade_homens"),
                    valueBoxOutput(width = 2, outputId = "quantidade_mulheres"),
                    valueBoxOutput(width = 2, outputId = ""),
                    valueBoxOutput(width = 2, outputId = "desistencia_tratamento"),
                    valueBoxOutput(width = 2, outputId = "quantidade_altas"),
                    valueBoxOutput(width = 2, outputId = "quantidade_falecidos")
                ),
                fluidRow(
                    box(
                        title = "Pacientes por faixa de Ano de Nascimento",
                        plotOutput( outputId = "visaoGeralAnoNascimento" ) # Finalizado
                    ),
                    box(
                        title = "Pacientes por Sexo",
                        plotOutput( outputId = "visaoGeralSexo" ) #Finalizado
                    ),
                    box(
                        title = "Pacientes por Estado",
                        width = 12,
                        plotOutput( outputId = "visaoGeralPacientesPorEstado" ) #Finalizado
                    ),
                    box(
                        title = "Pacientes por Cidade",
                        width = 12,
                        plotOutput( outputId = "visaoGeralPacientesPorCidade" ) #Finalizado
                    )
                )
            ),
            tabItem(
                # --------------------------------------------------------------
                # -- PACIENTES COM EXAMES --------------------------------------
                # --------------------------------------------------------------
                tabName = "pacientesComExames",
                fluidRow(
                    box(
                        width = 3,
                        height = 140,
                        sliderInput(inputId = "rangeAnoNascimentoPacientesComExames", strong("Ano Nascimento"),
                                    min = 1930, max = 2020,
                                    value = c( 1950, 2000 )
                        )
                    ),
                    box(
                        width = 3,
                        height = 140,
                        title = strong("Sexo"),
                        checkboxInput(inputId = "sexoMasculino",
                                      strong("M - Masculino"),
                                      value = TRUE),
                        checkboxInput(inputId = "sexoFeminino",
                                      strong("F - Feminino"),
                                      value = TRUE)
                    ),
                    box(
                        width = 3,
                        height = 140,
                        selectInput(
                            inputId = "buscaEstadoPacientesComExames",
                            label = "Estado",
                            choices = unique( dadosPacientes$CD_UF ),
                            selected = 1
                        )
                    ),
                    box(
                        width = 3,
                        height = 140,
                        selectInput(
                            inputId = "buscaCidadePacientesComExames",
                            label = "Cidade",
                            choices = unique( dadosPacientes$CD_MUNICIPIO ),
                            selected = 1
                        )
                    )
                ),
                fluidRow(
                    box(
                        title = strong("Pacientes com Exames por faixa de Ano de Nascimento"),
                        plotOutput( outputId = "pacienteComExamePorRangeAnoNascimento" )
                    ),
                    box(
                        title = strong("Pacientes com Exames por Sexo"),
                        plotOutput( outputId = "pacienteComExamePorSexo" )
                    ),
                    box(
                        title = strong("Pacientes com Exames por Estado"),
                        plotOutput( outputId = "pacienteComExamePorEstado" )
                    ),
                    box(
                        title = strong("Pacientes com Exames por Cidade"),
                        plotOutput( outputId = "pacienteComExamePorCidade" )
                    )
                )
            ),
            tabItem(
                # --------------------------------------------------------------
                # ------------- NEGATIVO ---------------------------------------
                # --------------------------------------------------------------
                tabName = "positivosNegativos",
                fluidRow(
                    box(
                        width = 3,
                        height = 140,
                        sliderInput(inputId = "rangeAnoNascimentoPacientesComExames_teste5", strong("Ano Nascimento"),
                                    min = 1930, max = 2020,
                                    value = c( 1950, 2000 )
                        )
                    ),
                    box(
                        width = 3,
                        height = 140,
                        title = strong("Sexo"),
                        checkboxInput(inputId = "sexoMasculino_teste5",
                                      strong("M - Masculino"),
                                      value = TRUE),
                        checkboxInput(inputId = "sexoFeminino_teste5",
                                      strong("F - Feminino"),
                                      value = TRUE)
                    ),
                    box(
                        width = 3,
                        height = 140,
                        selectInput(
                            inputId = "buscaEstadoPacientesComExames_teste5",
                            label = "Estado",
                            choices = unique( dadosPacientes$CD_UF ),
                            selected = 1
                        )
                    ),
                    box(
                        width = 3,
                        height = 140,
                        selectInput(
                            inputId = "buscaCidadePacientesComExames_teste5",
                            label = "Cidade",
                            choices = unique( dadosPacientes$CD_MUNICIPIO ),
                            selected = 1
                        )
                    )
                ),
                fluidRow(
                    box(
                        title = strong("Pacientes Negativos para COVID-19 por Ano de Nascimento"),
                        plotOutput( outputId = "pacienteComExamePorRangeAnoNascimento_teste5" )
                    ),
                    box(
                        title = strong("Pacientes Negativos para COVID-19 por Sexo"),
                        plotOutput( outputId = "pacienteComExamePorSexo_teste5" )
                    ),
                    box(
                        title = strong("Pacientes Negativos para COVID-19 por Estado"),
                        plotOutput( outputId = "pacienteComExamePorEstado_teste5" )
                    ),
                    box(
                        title = strong("Pacientes Negativos para COVID-19 por Cidade"),
                        plotOutput( outputId = "pacienteComExamePorCidade_teste5" )
                    )
                )
            ),
            tabItem(
              # --------------------------------------------------------------
              # -- POSITIVO --------------------------------------------------positivosNegativosPOSITIVOS
              # --------------------------------------------------------------
              tabName = "positivosNegativosPOSITIVOS",
              fluidRow(
                box(
                  width = 3,
                  height = 140,
                  sliderInput(inputId = "rangeAnoNascimentoPacientesComExames_teste52", strong("Ano Nascimento"),
                              min = 1930, max = 2020,
                              value = c( 1950, 2000 )
                  )
                ),
                box(
                  width = 3,
                  height = 140,
                  title = strong("Sexo"),
                  checkboxInput(inputId = "sexoMasculino_teste52",
                                strong("M - Masculino"),
                                value = TRUE),
                  checkboxInput(inputId = "sexoFeminino_teste52",
                                strong("F - Feminino"),
                                value = TRUE)
                ),
                box(
                  width = 3,
                  height = 140,
                  selectInput(
                    inputId = "buscaEstadoPacientesComExames_teste52",
                    label = "Estado",
                    choices = unique( dadosPacientes$CD_UF ),
                    selected = 1
                  )
                ),
                box(
                  width = 3,
                  height = 140,
                  selectInput(
                    inputId = "buscaCidadePacientesComExames_teste52",
                    label = "Cidade",
                    choices = unique( dadosPacientes$CD_MUNICIPIO ),
                    selected = 1
                  )
                )
              ),
              fluidRow(
                box(
                  title = strong("Pacientes Positivos para COVID-19 por faixa de Ano de Nascimento"),
                  plotOutput( outputId = "pacienteComExamePorRangeAnoNascimento_teste52" )
                ),
                box(
                  title = strong("Pacientes Positivos para COVID-19 por Sexo"),
                  plotOutput( outputId = "pacienteComExamePorSexo_teste52" )
                ),
                box(
                  title = strong("Pacientes Positivos para COVID-19 por Estado"),
                  plotOutput( outputId = "pacienteComExamePorEstado_teste52" )
                ),
                box(
                  title = strong("Pacientes Positivos para COVID-19 por Cidade"),
                  plotOutput( outputId = "pacienteComExamePorCidade_teste52" )
                )
              )
            ),
            tabItem(
                # --------------------------------------------------------------
                # -- OBITOS -------------------------------------
                # --------------------------------------------------------------
                tabName = "obitosRecuperacoes",
                fluidRow(
                    box(
                        width = 3,
                        height = 140,
                        sliderInput(inputId = "rangeAnoNascimentoPacientesComExames_teste8", strong("Ano Nascimento"),
                                    min = 1930, max = 2020,
                                    value = c( 1950, 2000 )
                        )
                    ),
                    box(
                        width = 3,
                        height = 140,
                        title = strong("Sexo"),
                        checkboxInput(inputId = "sexoMasculino_teste8",
                                      strong("M - Masculino"),
                                      value = TRUE),
                        checkboxInput(inputId = "sexoFeminino_teste8",
                                      strong("F - Feminino"),
                                      value = TRUE)
                    ),
                    box(
                        width = 3,
                        height = 140,
                        selectInput(
                            inputId = "buscaEstadoPacientesComExames_teste8",
                            label = "Estado",
                            choices = unique( dadosPacientes$CD_UF ),
                            selected = 1
                        )
                    ),
                    box(
                        width = 3,
                        height = 140,
                        selectInput(
                            inputId = "buscaCidadePacientesComExames_teste8",
                            label = "Cidade",
                            choices = unique( dadosPacientes$CD_MUNICIPIO ),
                            selected = 1
                        )
                    )
                ),
                fluidRow(
                    box(
                        title = strong("Obitos por faixa de Ano de Nascimento"),
                        plotOutput( outputId = "pacienteComExamePorRangeAnoNascimento_teste8" )
                    ),
                    box(
                        title = strong("Obitos por Sexo"),
                        plotOutput( outputId = "pacienteComExamePorSexo_teste8" )
                    ),
                    box(
                        title = strong("Obitos por Estado"),
                        plotOutput( outputId = "pacienteComExamePorEstado_teste8" )
                    ),
                    box(
                        title = strong("Obitos por Cidade"),
                        plotOutput( outputId = "pacienteComExamePorCidade_teste8" )
                    )
                )
            ),
            tabItem(
              # --------------------------------------------------------------
              # -- RECUPERAÇÕES -------------------------------------
              # --------------------------------------------------------------
              tabName = "obitosRecuperacoesRECUPERACOES",
              fluidRow(
                box(
                  width = 3,
                  height = 140,
                  sliderInput(inputId = "rangeAnoNascimentoPacientesComExames_teste82", strong("Ano Nascimento"),
                              min = 1930, max = 2020,
                              value = c( 1950, 2000 )
                  )
                ),
                box(
                  width = 3,
                  height = 140,
                  title = strong("Sexo"),
                  checkboxInput(inputId = "sexoMasculino_teste82",
                                strong("M - Masculino"),
                                value = TRUE),
                  checkboxInput(inputId = "sexoFeminino_teste82",
                                strong("F - Feminino"),
                                value = TRUE)
                ),
                box(
                  width = 3,
                  height = 140,
                  selectInput(
                    inputId = "buscaEstadoPacientesComExames_teste82",
                    label = "Estado",
                    choices = unique( dadosPacientes$CD_UF ),
                    selected = 1
                  )
                ),
                box(
                  width = 3,
                  height = 140,
                  selectInput(
                    inputId = "buscaCidadePacientesComExames_teste82",
                    label = "Cidade",
                    choices = unique( dadosPacientes$CD_MUNICIPIO ),
                    selected = 1
                  )
                )
              ),
              fluidRow(
                box(
                  title = strong("Pacientes Recuperados por faixa de Ano de Nascimento"),
                  plotOutput( outputId = "pacienteComExamePorRangeAnoNascimento_teste82" )
                ),
                box(
                  title = strong("Pacientes Recuperados por Sexo"),
                  plotOutput( outputId = "pacienteComExamePorSexo_teste82" )
                ),
                box(
                  title = strong("Pacientes Recuperados por Estado"),
                  plotOutput( outputId = "pacienteComExamePorEstado_teste82" )
                ),
                box(
                  title = strong("Pacientes Recuperados por Cidade"),
                  plotOutput( outputId = "pacienteComExamePorCidade_teste82" )
                )
              )
            ),
            tabItem(
                tabName = "sobre",
                # --------------------------------------------------------------
                # -- SOBRE -----------------------------------------------------
                # --------------------------------------------------------------
                box(
                    width = 12,
                    HTML("
                        <html>
                            <h3>Este trabalho utilizou dados disponibilizados por (FAPESP, 2020)</h3>
                            <h4><strong>Bases de Dados utilizadas:</strong></h4>
                            <h4> &#8226 Hospital das Clinicas da Faculdade de Medicina da Universidade de Sao Paulo</h4>
                            <h4> &#8226 Grupo Fleury</h4>
                            <h4> &#8226 Hospital Israelita Albert Einstein</h4>
                            <h4> &#8226 Hospital Sírio-Libanês</h4>
                            <hr>
                            <h4><strong>Nome Autor: </strong>Eduardo Eismann</h4>
                            <h4><strong>Orientadora: </strong>Dra. Marta Rosecler Bez</h4>
                            <hr>
                            <h4>Contato:</h4>
                            <h4><strong>E-mail: </strong>eduardo.eismann@gmail.com</h4>
                            <h4><strong>Instagram: </strong><a href=\"https://www.instagram.com/eduardoeismann/\">@eduardoeismann</a></h4>
                            <h4><strong>Twitter: </strong><a href=\"https://twitter.com/EduardoEismann/\">@eduardoeismann</a></h4>
                            <hr>
                            <h4>FAPESP. FAPESP COVID-19 Data Sharing/BR, Available from https://repositoriodatasharingfapesp.uspdigital.usp.br/. Accessed on July 2nd 2020</h4>
                        </html>
                    ")
                )
            )
        )
    )

)

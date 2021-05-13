# -- RM COLUMN ID_ATENDIMENTO --------------------------------------------------
excluirColunaIdAtendimento <- c("ID_ATENDIMENTO")
# ------------------------------------------------------------------------------


# -- CONCATENATE DATA PACIENTES ------------------------------------------------
dadosPacientesHCliSP <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Clinicas SP\\HC_PACIENTES_1.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)
dadosPacientesAlbEin <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Albert Einstein\\einstein_full_dataset_paciente.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)
dadosPacientesGruFle <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Grupo Fleury\\Grupo_Fleury_Dataset_Covid19_Pacientes.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)
dadosPacientesSirLib <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Sirio Libanes\\hsl_patient_1.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)

dadosPacientes <- rbind( dadosPacientesHCliSP, dadosPacientesAlbEin, dadosPacientesGruFle, dadosPacientesSirLib )
# ------------------------------------------------------------------------------


# -- CONCATENATE DATA EXAMES ---------------------------------------------------
dadosExamesHCliSP <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Clinicas SP\\HC_EXAMES_1.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)
dadosExamesAlbEin <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Albert Einstein\\einstein_full_dataset_exames.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)
dadosExamesGruFle <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Grupo Fleury\\Grupo_Fleury_Dataset_Covid19_Resultados_Exames.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)
dadosExamesSirLib <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Sirio Libanes\\hsl_lab_result_1.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)

dadosExamesSirLib <- dadosExamesSirLib[,!(names(dadosExamesSirLib) %in% excluirColunaIdAtendimento)]
dadosExamesHCliSP <- dadosExamesHCliSP[,!(names(dadosExamesHCliSP) %in% excluirColunaIdAtendimento)]
head(dadosExamesSirLib)
head(dadosExamesHCliSP)

dadosExames <- rbind( dadosExamesHCliSP, dadosExamesAlbEin, dadosExamesGruFle, dadosExamesSirLib)
# ------------------------------------------------------------------------------


# -- DADOS DE DESFECHO DE PACIENTES HOSP SIR LIB -------------------------------
dadosDesfecho <- read.csv("C:\\Users\\Eduardo\\Desktop\\Bases de Dados Covid\\Hosp Sirio Libanes\\hsl_desfecho_1.csv", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE)
# ------------------------------------------------------------------------------

dadosSelecionados <- merge(dadosPacientes, dadosExames, by = "ID_PACIENTE")

dadosObitosRecuperados <- merge(dadosPacientes, dadosDesfecho, by = "ID_PACIENTE")

# RESULTADOS DOS EXAMES (POSITIVO) POR EXAME
exameColetadoNomenclatura01_POS <- subset(dadosExames, DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura02_POS <- subset(dadosExames, DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura03_POS <- subset(dadosExames, DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura04_POS <- subset(dadosExames, DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura05_POS <- subset(dadosExames, DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura06_POS <- subset(dadosExames, DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura07_POS <- subset(dadosExames, DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura08_POS <- subset(dadosExames, DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura09_POS <- subset(dadosExames, DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura10_POS <- subset(dadosExames, DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura11_POS <- subset(dadosExames, DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura12_POS <- subset(dadosExames, DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura13_POS <- subset(dadosExames, DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura14_POS <- subset(dadosExames, DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura15_POS <- subset(dadosExames, DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura16_POS <- subset(dadosExames, DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))
exameColetadoNomenclatura17_POS <- subset(dadosExames, DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Detectado" | DE_RESULTADO == "DETECTADO" | DE_RESULTADO == "DETECTADO (POSITIVO)" | DE_RESULTADO == "Reagente" | DE_RESULTADO == "REAGENTE" | DE_RESULTADO == "REAGENTE (POSITIVO)"))

# CONCATENANDO RESULTADOS DOS EXAMES (POSITIVO)
examesPOSITIVOS <- rbind(
    exameColetadoNomenclatura01_POS, exameColetadoNomenclatura02_POS,
    exameColetadoNomenclatura03_POS, exameColetadoNomenclatura04_POS,
    exameColetadoNomenclatura05_POS, exameColetadoNomenclatura06_POS,
    exameColetadoNomenclatura07_POS, exameColetadoNomenclatura08_POS,
    exameColetadoNomenclatura09_POS, exameColetadoNomenclatura10_POS,
    exameColetadoNomenclatura11_POS, exameColetadoNomenclatura12_POS,
    exameColetadoNomenclatura13_POS, exameColetadoNomenclatura14_POS,
    exameColetadoNomenclatura15_POS, exameColetadoNomenclatura16_POS,
    exameColetadoNomenclatura17_POS
)



# RESULTADOS DOS EXAMES (NEGATIVO) POR EXAME
exameColetadoNomenclatura01_NEG <- subset(dadosExames, DE_ANALITO == "CoronavirusNL63" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura02_NEG <- subset(dadosExames, DE_ANALITO == "Resultado COVID-19:" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura03_NEG <- subset(dadosExames, DE_ANALITO == "CoronavirusOC43" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura04_NEG <- subset(dadosExames, DE_ANALITO == "Coronavirus229E" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura05_NEG <- subset(dadosExames, DE_ANALITO == "CoronavirusHKU1" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura06_NEG <- subset(dadosExames, DE_ANALITO == "COVID IgG Interp aqui" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura07_NEG <- subset(dadosExames, DE_ANALITO == "Covid 19, Deteccao por PCR" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura08_NEG <- subset(dadosExames, DE_ANALITO == "Covid 19, Anticorpos IgM, Quimioluminescencia" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura09_NEG <- subset(dadosExames, DE_ANALITO == "Covid 19, Anticorpos IgA, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura10_NEG <- subset(dadosExames, DE_ANALITO == "Covid 19, Anticorpos IgG, Elisa" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura11_NEG <- subset(dadosExames, DE_ANALITO == "Teste Rapido para SARS-CoV-2- Pesquisa de anticor" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura12_NEG <- subset(dadosExames, DE_ANALITO == "Coronavirus 2019-nCoV" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura13_NEG <- subset(dadosExames, DE_ANALITO == "Coronavirus humano NL63 (Cor63)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura14_NEG <- subset(dadosExames, DE_ANALITO == "Coronavirus humano OC43 (Cor43)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura15_NEG <- subset(dadosExames, DE_ANALITO == "Coronavirus humano HKU1 (HKU)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura16_NEG <- subset(dadosExames, DE_ANALITO == "Coronavirus humano 229E (Cor229)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))
exameColetadoNomenclatura17_NEG <- subset(dadosExames, DE_ANALITO == "Coronavirus (2019-nCoV)" & (DE_RESULTADO == "Nao Reagente" | DE_RESULTADO == "Nao reagente" | DE_RESULTADO == "NaO REAGENTE" | DE_RESULTADO == "NAO REAGENTE" | DE_RESULTADO == "NaO REAGENTE (NEGATIVO)" | DE_RESULTADO == "Nao Detectado" | DE_RESULTADO == "Nao detectado" | DE_RESULTADO == "NaO DETECTADO" | DE_RESULTADO == "NAO DETECTADO" | DE_RESULTADO == "NaO DETECTADO (NEGATIVO)"))

# CONCATENANDO RESULTADOS DOS EXAMES (NEGATIVO)
examesNEGATIVOS <- rbind(
  exameColetadoNomenclatura01_NEG, exameColetadoNomenclatura02_NEG,
  exameColetadoNomenclatura03_NEG, exameColetadoNomenclatura04_NEG,
  exameColetadoNomenclatura05_NEG, exameColetadoNomenclatura06_NEG,
  exameColetadoNomenclatura07_NEG, exameColetadoNomenclatura08_NEG,
  exameColetadoNomenclatura09_NEG, exameColetadoNomenclatura10_NEG,
  exameColetadoNomenclatura11_NEG, exameColetadoNomenclatura12_NEG,
  exameColetadoNomenclatura13_NEG, exameColetadoNomenclatura14_NEG,
  exameColetadoNomenclatura15_NEG, exameColetadoNomenclatura16_NEG,
  exameColetadoNomenclatura17_NEG
)

resultadosExamesGerais <- rbind(examesNEGATIVOS, examesPOSITIVOS)

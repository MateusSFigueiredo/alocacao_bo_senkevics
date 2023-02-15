# ==============================================================================
# Arquivo: analysis_03_roda_funcoes_preenche.R
# Roda modelo de preenchimento de vagas de Aygun e Bó (2021) e Bó e Senkevics (2023)

# Todos os candidatos podem preencher vagas da AC
# cotistas podem preencher quaisquer modalidade que eles tenham direito
# Gera análise de notas e de preenchimento de vagas ao final
# AC ao final

# Modificado em 2023-02-15.
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Preparação

# Carrega dados

setwd("C:/Users/Mateus/Desktop/R/alocacao_bo_senkevics")
# getwd()
# list.files()

# carregar dados de MEDICINA e PEDAGOGIA, e colocar em ordem decrescente de nota
d_med <- read.csv("dados_medicina_2019.csv")
dados_MEDICINA_2019 <- d_med[order(d_med$nota,decreasing=TRUE),];rm(d_med)

d_ped <- read.csv("dados_pedagogia_2019.csv")
dados_PEDAGOGIA_2019 <- d_ped[order(d_ped$nota,decreasing=TRUE),]#;rm(d_ped)



# ------------------------------------------------------------------------------

# Carregar lista_todos
# lista_todos # lista de todos os candidatos para determinado curso e ano

# # dados observados
# # carregado a partir de data_04_carregar_dados_UFV.R

### MUDAR CURSO AQUI ###
# lista_todos <- dados_MEDICINA_2019
lista_todos <- dados_PEDAGOGIA_2019

lista_todos %>% slice_head(n=30)

# ------------------------------------------------------------------------------
# É preciso existir nvagas

# sequencia: A0, L01, L02, L05, L06, L09, L10, L13, L14

# cria nvagas a partir do Curso em lista_todos
if (lista_todos$Curso[1]=="MEDICINA") {nvagas <- c(25,5,6,4,6,1,1,1,1)}
if (lista_todos$Curso[1]=="PEDAGOGIA"){nvagas <- c(30,5,8,5,8,1,1,1,1)}

# cria nvagas arbitrariamente
# nvagas <- c(25,5,6,4,6,1,1,1,1) # Medicina UFV
# nvagas <- c(30,5,8,5,8,1,1,1,1) # Pedagogia UFV

nvagas %>% length == 9

# é preciso existir nvagas_A0 até nvagas_L14
{nvagas[1]->nvagas_A0
  nvagas[2]->nvagas_L01
  nvagas[3]->nvagas_L02
  nvagas[4]->nvagas_L05
  nvagas[5]->nvagas_L06
  nvagas[6]->nvagas_L09
  nvagas[7]->nvagas_L10
  nvagas[8]->nvagas_L13
  nvagas[9]->nvagas_L14}

# inscritos deve ser maior ou igual que vagas em todas as modalidades
prod(
  lista_todos %>% subset(mod_ins=="A0")  %>% nrow() >= nvagas_A0   ,
  lista_todos %>% subset(mod_ins=="L01") %>% nrow() >= nvagas_L01  ,
  lista_todos %>% subset(mod_ins=="L02") %>% nrow() >= nvagas_L02  ,
  lista_todos %>% subset(mod_ins=="L05") %>% nrow() >= nvagas_L05  ,
  lista_todos %>% subset(mod_ins=="L06") %>% nrow() >= nvagas_L06  ,
  lista_todos %>% subset(mod_ins=="L09") %>% nrow() >= nvagas_L09  ,
  lista_todos %>% subset(mod_ins=="L10") %>% nrow() >= nvagas_L10  ,
  lista_todos %>% subset(mod_ins=="L13") %>% nrow() >= nvagas_L13  ,
  lista_todos %>% subset(mod_ins=="L14") %>% nrow() >= nvagas_L14) == 1
# se TRUE, então ok

# ------------------------------------------------------------------------------

# quantos candidatos tem? print texto
paste(lista_todos %>% nrow(), "candidatos em",
      lista_todos$Curso[1], "no", lista_todos$Processo_Seletivo[1])

# ==============================================================================
# Rodar apenas uma ordem

# # ordem_1a <- function(){   
#   lista_todos_original <- lista_todos # armazena lista_todos   
#   rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
# preenche_A0();
# preenche_L05();
# preenche_L01();
# preenche_L06();
# preenche_L02();
# preenche_L13();
# preenche_L09();
# preenche_L14();
# preenche_L10();
# convocados_1a  <<- do.call("rbind", list(aprovados_A0,
#                                          aprovados_L01,aprovados_L02,
#                                          aprovados_L05,aprovados_L06,
#                                          aprovados_L09,aprovados_L10,
#                                          aprovados_L13,aprovados_L14)); 
# lista_todos <<- lista_todos_original # recupera lista_todos
# # } # fim da ordem_1a


# ==============================================================================

# Roda funções ordem_1a até ordem_12b
{ # roda todas as ordens
rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_1a(); convocados_1a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_2a(); convocados_2a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_3a(); convocados_3a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_4a(); convocados_4a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_5a(); convocados_5a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_6a(); convocados_6a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_7a(); convocados_7a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_8a(); convocados_8a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_9a(); convocados_9a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_10a(); convocados_10a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_11a(); convocados_11a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_12a(); convocados_12a$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_1b(); convocados_1b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_2b(); convocados_2b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_3b(); convocados_3b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_4b(); convocados_4b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_5b(); convocados_5b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_6b(); convocados_6b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_7b(); convocados_7b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_8b(); convocados_8b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_9b(); convocados_9b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_10b(); convocados_10b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_11b(); convocados_11b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
ordem_12b(); convocados_12b$nota %>% mean() # cria convocados

rm(list=ls(pattern="^aprovados")) # apaga aprovados
} # fim de todar todas as ordens

# ==============================================================================
# Análises

# Criar analise_vagas

mod<-c("A0","L01","L02","L05","L06","L09","L10","L13","L14")
          #"total","pub","bxa","ppi","pcd")

codigo_das_ordens<-c(
                    paste(1:12,"a",sep=""),
                    paste(1:12,"b",sep=""))

nomes_das_ordens<-c(
  paste("ordem_",1:12,"a",sep=""),
  paste("ordem_",1:12,"b",sep=""))


analise_vagas <-data.frame(matrix(ncol = length(mod),
                                  nrow = length(nomes_das_ordens)),
                           row.names=nomes_das_ordens)

colnames(analise_vagas) <- mod

# Preencher analise_vagas

# preencher apenas uma linha
# for (i in 1:9) {
#   analise_vagas["ordem_1a", mod[i]] <-
#     convocados_1a %>% filter(mod_ins == mod[i]) %>% nrow()
# }



# ------------------------------------------------------------------------------
# Preencher atual
analise_vagas["atual",1:9] <- nvagas
# analise_vagas["atual","total"] <- sum(nvagas)
# analise_vagas["atual","pub"] <- sum(nvagas[c(2:9)])
# analise_vagas["atual","bxa"] <- sum(nvagas[c(2,3,6,7)])
# analise_vagas["atual","ppi"] <- sum(nvagas[c(3,5,7,9)])
# analise_vagas["atual","pcd"] <- sum(nvagas[c(6:9)])

# ------------------------------------------------------------------------------
# Preencher ordens 1a até 12b

# Create an empty data frame with the appropriate column names and row names
analise_vagas <- data.frame(matrix(nrow=24, ncol=9))
colnames(analise_vagas) <- mod
row.names(analise_vagas)<-c(paste("ordem_",1:12,"a",sep=""),
                    paste("ordem_",1:12,"b",sep=""))

# preencher 1a até 12a

# Loop through each value of k and modify the appropriate values in analise_vagas
for (k in 1:12) {
  a_value <- paste0(k, "a")
  convocados <- get(paste0("convocados_", a_value))
  for (i in 1:9) {
    analise_vagas[paste0("ordem_", a_value), mod[i]] <- convocados %>% filter(mod_ins == mod[i]) %>% nrow()
  }
}

# preencher 1b até 12b

# Loop through each value of k and modify the appropriate values in analise_vagas
for (k in 1:12) {
  b_value <- paste0(k, "b")
  convocados <- get(paste0("convocados_", b_value))
  for (i in 1:9) {
    analise_vagas[paste0("ordem_", b_value), mod[i]] <- convocados %>% filter(mod_ins == mod[i]) %>% nrow()
  }
}

# PARECE QUE ATÉ AQUI ESTÁ FUNCIONANDO CORRETAMENTE

# Até aqui tá ok

# there be monsters ahead
# 

# ------------------------------------------------------------------------------

# preencher para todas as linhas:
# total de vagas preenchidas por grupo social

for (i in 1:24){
analise_vagas$tot[i] <- sum(analise_vagas[i,1:9])
analise_vagas$pub[i] <- sum(analise_vagas[i,2:9])
analise_vagas$bxa[i] <- sum(analise_vagas$L01[i], 
                            analise_vagas$L02[i],
                            analise_vagas$L09[i],
                            analise_vagas$L10[i])
analise_vagas$ppi[i] <- sum(analise_vagas$L02[i], 
                            analise_vagas$L06[i],
                            analise_vagas$L10[i], 
                            analise_vagas$L14[i])
analise_vagas$pcd[i] <- sum(analise_vagas$L09[i],
                            analise_vagas$L10[i],
                            analise_vagas$L13[i],
                            analise_vagas$L14[i])
}

analise_vagas$nota <- 0

# ------------------------------------------------------------------------------
# preencher nota média

# para uma ordem
convocados_1a$nota %>% mean() -> analise_vagas["ordem_1a","nota"]

# para ordens 1a até 12a

for (i in 1:12) {
  ordem_value <- paste0(i, "a")
  convocados <- get(paste0("convocados_", ordem_value))
  analise_vagas[paste0("ordem_", ordem_value), "nota"] <- convocados$nota %>% mean()
}

# para ordens 1b até 12b

for (i in 1:12) {
  ordem_value <- paste0(i, "b")
  convocados <- get(paste0("convocados_", ordem_value))
  analise_vagas[paste0("ordem_", ordem_value), "nota"] <- convocados$nota %>% mean()
}

# ==============================================================================

# Exportar tabelas de analise

# criar de curso arbitrário
# analise_PEDAGOGIA <- analise_vagas

# criar analise_CURSO a partir do curso que estiver em lista_todos
eval(parse(text=(paste("analise_",lista_todos$Curso[1],"<- analise_vagas",
                       sep=""))))

# ==============================================================================
# Arquivo: analysis_04_preenche_listas_mult.R
# Roda modelo de listas múltiplas de preenchimento de vagas
# (também chamado concorrência separada)

# Cada candidato só preenche vaga da modalidade em que ele se inscreveu

# Modificado em 2023-02-13.
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Preparação

library(dplyr)

# Inputs necessários. Devem ser TRUE:

exists("dados_MEDICINA_2019")
exists("dados_PEDAGOGIA_2019")
exists("nvagas")

# ==============================================================================

# Preenche concorrência separada

# Criar funções preenche_ mod _ c1 (c1 = concorrencia simples, modelo c1)

# ------------------------------------------------------------------------------
{ # começa a criar todas as funcion preenche mod c1
  preenche_A0_c1<-function(){
    
    # Cria aprovados_A0
    aprovados_A0 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_A0))
    colnames(aprovados_A0) <<- colnames(lista_todos)
    
    # Preenche aprovados_A0
    lista_todos %>% subset(mod_ins=="A0") %>% slice_head(n=nvagas_A0) ->> aprovados_A0
    aprovados_A0$mod_con <<- "A0"
    
    # Atualiza lista_todos removendo presentes em aprovados_A0
    subset(lista_todos, !id %in% aprovados_A0$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L01_c1<-function(){
    
    # Cria aprovados_L01
    aprovados_L01 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L01))
    colnames(aprovados_L01) <<- colnames(lista_todos)
    
    # Preenche aprovados_L01
    lista_todos %>% subset(mod_ins=="L01") %>%slice_head(n=nvagas_L01) ->> aprovados_L01
    aprovados_L01$mod_con <<- "L01"
    
    # Atualiza lista_todos removendo presentes em aprovados_L01
    subset(lista_todos, !id %in% aprovados_L01$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L02_c1<-function(){
    
    # Cria aprovados_L02
    aprovados_L02 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L02))
    colnames(aprovados_L02) <<- colnames(lista_todos)
    
    # Preenche aprovados_L02
    lista_todos %>% subset(mod_ins=="L02") %>% slice_head(n=nvagas_L02) ->> aprovados_L02
    aprovados_L02$mod_con <<- "L02"
    
    # Atualiza lista_todos removendo presentes em aprovados_L02
    subset(lista_todos, !id %in% aprovados_L02$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L05_c1<-function(){
    
    # Cria aprovados_L05
    aprovados_L05 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L05))
    colnames(aprovados_L05) <<- colnames(lista_todos)
    
    # Preenche aprovados_L05
    lista_todos %>% subset(mod_ins=="L05") %>%slice_head(n=nvagas_L05) ->> aprovados_L05
    aprovados_L05$mod_con <<- "L05"
    
    # Atualiza lista_todos removendo presentes em aprovados_L05
    subset(lista_todos, !id %in% aprovados_L05$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L06_c1<-function(){
    
    # Cria aprovados_L06
    aprovados_L06 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L06))
    colnames(aprovados_L06) <<- colnames(lista_todos)
    
    # Preenche aprovados_L06
    lista_todos %>% subset(mod_ins=="L06") %>%slice_head(n=nvagas_L06) ->> aprovados_L06
    aprovados_L06$mod_con <<- "L06"
    
    # Atualiza lista_todos removendo presentes em aprovados_L06
    subset(lista_todos, !id %in% aprovados_L06$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L09_c1<-function(){
    
    # Cria aprovados_L09
    aprovados_L09 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L09))
    colnames(aprovados_L09) <<- colnames(lista_todos)
    
    # Preenche aprovados_L09
    lista_todos %>% subset(mod_ins=="L09") %>%slice_head(n=nvagas_L09) ->> aprovados_L09
    aprovados_L09$mod_con <<- "L09"
    
    # Atualiza lista_todos removendo presentes em aprovados_L09
    subset(lista_todos, !id %in% aprovados_L09$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L10_c1<-function(){
    
    # Cria aprovados_L10
    aprovados_L10 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L10))
    colnames(aprovados_L10) <<- colnames(lista_todos)
    
    # Preenche aprovados_L10
    lista_todos %>% subset(mod_ins=="L10") %>%slice_head(n=nvagas_L10) ->> aprovados_L10
    aprovados_L10$mod_con <<- "L10"
    
    # Atualiza lista_todos removendo presentes em aprovados_L10
    subset(lista_todos, !id %in% aprovados_L10$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L13_c1<-function(){
    
    # Cria aprovados_L13
    aprovados_L13 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L13))
    colnames(aprovados_L13) <<- colnames(lista_todos)
    
    # Preenche aprovados_L13
    lista_todos %>% subset(mod_ins=="L13") %>%slice_head(n=nvagas_L13) ->> aprovados_L13
    aprovados_L13$mod_con <<- "L13"
    
    # Atualiza lista_todos removendo presentes em aprovados_L13
    subset(lista_todos, !id %in% aprovados_L13$id) ->> lista_todos
    
  }# fim da function
  
  # ------------------------------------------------------------------------------
  
  preenche_L14_c1<-function(){
    
    # Cria aprovados_L14
    aprovados_L14 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L14))
    colnames(aprovados_L14) <<- colnames(lista_todos)
    
    # Preenche aprovados_L14
    lista_todos %>% subset(mod_ins=="L14") %>%slice_head(n=nvagas_L14) ->> aprovados_L14
    aprovados_L14$mod_con <<- "L14"
    
    # Atualiza lista_todos removendo presentes em aprovados_L14
    subset(lista_todos, !id %in% aprovados_L14$id) ->> lista_todos
    
  }# fim da function
} # fim de criar todas as function preenche_ mod _c1

# ==============================================================================
# remove objetos aprovados_, caso já existam
rm(list=ls(pattern="^aprovados_"))

# Rodar funções preenche_ mod _ c1 (c1 = concorrencia simples, modelo c1)

if(isFALSE(exists("aprovados_A0")))  {preenche_A0_c1()}
if(isFALSE(exists("aprovados_L01"))) {preenche_L01_c1()}
if(isFALSE(exists("aprovados_L02"))) {preenche_L02_c1()}
if(isFALSE(exists("aprovados_L05"))) {preenche_L05_c1()}
if(isFALSE(exists("aprovados_L06"))) {preenche_L06_c1()}
if(isFALSE(exists("aprovados_L09"))) {preenche_L09_c1()}
if(isFALSE(exists("aprovados_L10"))) {preenche_L10_c1()}
if(isFALSE(exists("aprovados_L13"))) {preenche_L13_c1()}
if(isFALSE(exists("aprovados_L14"))) {preenche_L14_c1()}

# cria objeto com todos os aprovados
aprovados <- do.call("rbind", list(aprovados_A0,
                                   aprovados_L01,aprovados_L02,
                                   aprovados_L05,aprovados_L06,
                                   aprovados_L09,aprovados_L10,
                                   aprovados_L13,aprovados_L14))

# ==============================================================================
# Análises

# Criar analise_vagas

mod<-c("A0","L01","L02","L05","L06","L09","L10","L13","L14")
          #"total","pub","bxa","ppi","pcd")

analise_vagas <-data.frame(matrix(ncol = length(mod),
                                  nrow = 1),
                           row.names="sisu_atual")

colnames(analise_vagas) <- mod

# Preencher analise_vagas 1:9

convocados_c1 <- aprovados

# preencher apenas uma linha
for (i in 1:9) {
  analise_vagas["sisu_atual", mod[i]] <-
    convocados_c1 %>% filter(mod_ins == mod[i]) %>% nrow()
}

# preencher total de vagas preenchidas por grupo social

for (i in 1){
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

# preencher nota média
# para uma linha
convocados_c1$nota %>% mean() -> analise_vagas[1,"nota"]



# ==============================================================================

# Atualizar tabelas de analise

if (lista_todos$Curso[1] == "MEDICINA"){
analise_MEDICINA <- rbind(analise_MEDICINA, analise_vagas)
}

if (lista_todos$Curso[1] == "PEDAGOGIA"){
  analise_PEDAGOGIA <- rbind(analise_PEDAGOGIA, analise_vagas)
}

# ------------------------------------------------------------------------------

# Exportar

# write.csv2(analise_PEDAGOGIA, "analise_PEDAGOGIA.csv")
# write.csv2(analise_MEDICINA, "analise_MEDICINA.csv")


# ==============================================================================
# Arquivo: analysis_02_funcoes_preenche.R
# Roda modelo de preenchimento de vagas de Aygun e Bó (2021) e Bó e Senkevics (2023)

# Todos os candidatos podem preencher vagas da AC
# cotistas podem preencher quaisquer modalidade que eles tenham direito
# Gera análise de notas e de preenchimento de vagas ao final
# AC ao final

# Cria funções preenche_A0 até L14, que preenche as vagas dessas modalidades
# Cria funções ordem_1a até 12b, que cria convocados_1a até 12b

# Semelhante a analysis_014_conc_bo.R
# Modificado em 2023-02-15.
# Autor: Mateus Silva Figueiredo

# ==============================================================================

# Cria funções preenche_ A0 até L13

# Cada função preenche_ faz:
# cria aprovados_ A0 até L13
# preenche aprovados_ A0 até L13
# Atualiza lista_todos para remover os aprovados

# ------------------------------------------------------------------------------

# Criar funções preenche_ mod _ c4 (c4 = concorrencia segundo bo e senkevics)

{ # criar todas as funções preenche_ A0 até L13
print("criando funções preenche_ A0 até L13")

# ------------------------------------------------------------------------------

preenche_A0 <-function(){
  if(isFALSE(exists("aprovados_A0"))){ # só roda se não houver aprovados_A0
# Cria aprovados_A0
aprovados_A0 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_A0))
colnames(aprovados_A0) <<- colnames(lista_todos)

# Preenche aprovados_A0
lista_todos %>% slice_head(n=nvagas_A0) ->> aprovados_A0 # ignora mod_ins
aprovados_A0$mod_con <<- "A0"

# Atualiza lista_todos removendo presentes em aprovados_A0
subset(lista_todos, !id %in% aprovados_A0$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

preenche_L01 <-function(){
  if(isFALSE(exists("aprovados_L01"))){ # só roda se não houver aprovados_L01
# Cria aprovados_L01
aprovados_L01 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L01))
colnames(aprovados_L01) <<- colnames(lista_todos)

# Preenche aprovados_L01 # apenas pub e bxa
lista_todos %>% subset(pub) %>% subset(bxa) %>% slice_head(n=nvagas_L01) ->> aprovados_L01
aprovados_L01$mod_con <<- "L01"

# Atualiza lista_todos removendo presentes em aprovados_L01
subset(lista_todos, !id %in% aprovados_L01$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

preenche_L02 <-function(){
  if(isFALSE(exists("aprovados_L02"))){ # só roda se não houver aprovados_L02
# Cria aprovados_L02
aprovados_L02 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L02))
colnames(aprovados_L02) <<- colnames(lista_todos)

# Preenche aprovados_L02# apenas pub bxa ppi
lista_todos %>% subset(pub) %>% subset(bxa) %>% subset(ppi) %>%
  slice_head(n=nvagas_L02) ->> aprovados_L02
aprovados_L02$mod_con <<- "L02"

# Atualiza lista_todos removendo presentes em aprovados_L02
subset(lista_todos, !id %in% aprovados_L02$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

preenche_L05 <-function(){
  if(isFALSE(exists("aprovados_L05"))){ # só roda se não houver aprovados_L05
# Cria aprovados_L05
aprovados_L05 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L05))
colnames(aprovados_L05) <<- colnames(lista_todos)

# Preenche aprovados_L05# apenas pub
lista_todos %>% subset(pub) %>% 
  slice_head(n=nvagas_L05) ->> aprovados_L05
aprovados_L05$mod_con <<- "L05"

# Atualiza lista_todos removendo presentes em aprovados_L02
subset(lista_todos, !id %in% aprovados_L05$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

preenche_L06 <-function(){
  if(isFALSE(exists("aprovados_L06"))){ # só roda se não houver aprovados_L06
# Cria aprovados_L06
aprovados_L06 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L06))
colnames(aprovados_L06) <<- colnames(lista_todos)

# Preenche aprovados_L06# apenas pub ppi
lista_todos %>% subset(pub) %>% subset(ppi) %>%
  slice_head(n=nvagas_L06) ->> aprovados_L06
aprovados_L06$mod_con <<- "L06"

# Atualiza lista_todos removendo presentes em aprovados_L06
subset(lista_todos, !id %in% aprovados_L06$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

preenche_L09 <-function(){
  if(isFALSE(exists("aprovados_L09"))){ # só roda se não houver aprovados_L09
# Cria aprovados_L09
aprovados_L09 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L09))
colnames(aprovados_L09) <<- colnames(lista_todos)

# Preenche aprovados_L09 # apenas pub bxa pcd
lista_todos %>% subset(pub) %>% subset(bxa) %>% subset(pcd) %>%
  slice_head(n=nvagas_L09) ->> aprovados_L09
aprovados_L09$mod_con <<- "L09"

# Atualiza lista_todos removendo presentes em aprovados_L09
subset(lista_todos, !id %in% aprovados_L09$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

preenche_L10 <-function(){
  if(isFALSE(exists("aprovados_L10"))){ # só roda se não houver aprovados_L10
# Cria aprovados_L10
aprovados_L10 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L10))
colnames(aprovados_L10) <<- colnames(lista_todos)

# Preenche aprovados_L10 # apenas pub bxa ppi pcd
lista_todos %>% subset(pub) %>% subset(bxa) %>% subset(ppi) %>% subset(pcd) %>%
  slice_head(n=nvagas_L10) ->> aprovados_L10
aprovados_L10$mod_con <<- "L10"

# Atualiza lista_todos removendo presentes em aprovados_L10
subset(lista_todos, !id %in% aprovados_L10$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

preenche_L13 <-function(){
  if(isFALSE(exists("aprovados_L13"))){ # só roda se não houver aprovados_L13
# Cria aprovados_L13
aprovados_L13 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L13))
colnames(aprovados_L13) <<- colnames(lista_todos)

# Preenche aprovados_L13 # apenas pub  pcd
lista_todos %>% subset(pub) %>%  subset(pcd) %>%
  slice_head(n=nvagas_L13) ->> aprovados_L13
aprovados_L13$mod_con <<- "L13"

# Atualiza lista_todos removendo presentes em aprovados_L10
subset(lista_todos, !id %in% aprovados_L13$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

preenche_L14 <-function(){
  if(isFALSE(exists("aprovados_L14"))){ # só roda se não houver aprovados_L14
# Cria aprovados_L14
aprovados_L14 <<- data.frame(matrix(ncol = ncol(lista_todos), nrow = nvagas_L14))
colnames(aprovados_L14) <<- colnames(lista_todos)

# Preenche aprovados_L14 # apenas pub ppi pcd
lista_todos %>% subset(pub) %>% subset(ppi) %>% subset(pcd) %>%
  slice_head(n=nvagas_L14) ->> aprovados_L14
aprovados_L14$mod_con <<- "L14"

# Atualiza lista_todos removendo presentes em aprovados_L10
subset(lista_todos, !id %in% aprovados_L14$id) ->> lista_todos
} # fim do if(isFALSE
}# fim da function

# ------------------------------------------------------------------------------

} # fim da criação de todas as funções preenche_ A0 até L13


# ==============================================================================


# Cria funções ordem
{ # começa a criar funções ordem
 
ordem_1a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L01();
  preenche_L06();
  preenche_L02();
  preenche_L13();
  preenche_L09();
  preenche_L14();
  preenche_L10();
  convocados_1a  <<- do.call("rbind", list(aprovados_A0,
                                     aprovados_L01,aprovados_L02,
                                     aprovados_L05,aprovados_L06,
                                     aprovados_L09,aprovados_L10,
                                     aprovados_L13,aprovados_L14)); 
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_2a <- function(){
  lista_todos_original <- lista_todos # armazena lista_todos
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L01();
  preenche_L13();
  preenche_L09();
  preenche_L06();
  preenche_L02();
  preenche_L14();
  preenche_L10();
  convocados_2a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_3a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L06();
  preenche_L01();
  preenche_L02();
  preenche_L13();
  preenche_L14();
  preenche_L09();
  preenche_L10();
  convocados_3a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_4a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L13();
  preenche_L01();
  preenche_L09();
  preenche_L06();
  preenche_L14();
  preenche_L02();
  preenche_L10();
  convocados_4a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_5a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L06();
  preenche_L13();
  preenche_L14();
  preenche_L01();
  preenche_L02();
  preenche_L09();
  preenche_L10();
  convocados_5a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_6a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos  
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L13();
  preenche_L06();
  preenche_L14();
  preenche_L01();
  preenche_L09();
  preenche_L02();
  preenche_L10();
  convocados_6a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_7a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L01();
  preenche_L06();
  preenche_L13();
  preenche_L02();
  preenche_L09();
  preenche_L14();
  preenche_L10();
  convocados_7a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_8a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L01();
  preenche_L13();
  preenche_L06();
  preenche_L09();
  preenche_L02();
  preenche_L14();
  preenche_L10();
  convocados_8a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_9a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L06();
  preenche_L01();
  preenche_L13();
  preenche_L02();
  preenche_L14();
  preenche_L09();
  preenche_L10();
  convocados_9a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_10a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L13();
  preenche_L01();
  preenche_L06();
  preenche_L09();
  preenche_L14();
  preenche_L02();
  preenche_L10();
  convocados_10a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_11a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L06();
  preenche_L13();
  preenche_L01();
  preenche_L14();
  preenche_L02();
  preenche_L09();
  preenche_L10();
  convocados_11a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_12a <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_A0();
  preenche_L05();
  preenche_L13();
  preenche_L06();
  preenche_L01();
  preenche_L14();
  preenche_L09();
  preenche_L02();
  preenche_L10();
  convocados_12a  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_1b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L14();
  preenche_L09();
  preenche_L13();
  preenche_L02();
  preenche_L06();
  preenche_L01();
  preenche_L05();
  preenche_A0();
  convocados_1b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_2b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L14();
  preenche_L02();
  preenche_L06();
  preenche_L09();
  preenche_L13();
  preenche_L01();
  preenche_L05();
  preenche_A0();
  convocados_2b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_3b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L09();
  preenche_L14();
  preenche_L13();
  preenche_L02();
  preenche_L01();
  preenche_L06();
  preenche_L05();
  preenche_A0();
  convocados_3b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_4b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L02();
  preenche_L14();
  preenche_L06();
  preenche_L09();
  preenche_L01();
  preenche_L13();
  preenche_L05();
  preenche_A0();
  convocados_4b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_5b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L09();
  preenche_L02();
  preenche_L01();
  preenche_L14();
  preenche_L13();
  preenche_L06();
  preenche_L05();
  preenche_A0();
  convocados_5b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_6b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L02();
  preenche_L09();
  preenche_L01();
  preenche_L14();
  preenche_L06();
  preenche_L13();
  preenche_L05();
  preenche_A0();
  convocados_6b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_7b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L14();
  preenche_L09();
  preenche_L02();
  preenche_L13();
  preenche_L06();
  preenche_L01();
  preenche_L05();
  preenche_A0();
  convocados_7b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_8b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L14();
  preenche_L02();
  preenche_L09();
  preenche_L06();
  preenche_L13();
  preenche_L01();
  preenche_L05();
  preenche_A0();
  convocados_8b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))  
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_9b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L09();
  preenche_L14();
  preenche_L02();
  preenche_L13();
  preenche_L01();
  preenche_L06();
  preenche_L05();
  preenche_A0();
  convocados_9b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_10b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L02();
  preenche_L14();
  preenche_L09();
  preenche_L06();
  preenche_L01();
  preenche_L13();
  preenche_L05();
  preenche_A0();
  convocados_10b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_11b <- function(){   
  lista_todos_original <- lista_todos # armazena lista_todos   
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L09();
  preenche_L02();
  preenche_L14();
  preenche_L01();
  preenche_L13();
  preenche_L06();
  preenche_L05();
  preenche_A0();
  convocados_11b  <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))   
  lista_todos <<- lista_todos_original # recupera lista_todos
}

ordem_12b <- function(){
  lista_todos_original <- lista_todos # armazena lista_todos
  rm(list=ls(pattern="^aprovados")) # apaga aprovados, caso existam
  preenche_L10();
  preenche_L02();
  preenche_L09();
  preenche_L14();
  preenche_L01();
  preenche_L06();
  preenche_L13();
  preenche_L05();
  preenche_A0();
  convocados_12b <<- do.call("rbind", list(aprovados_A0,
                                         aprovados_L01,aprovados_L02,
                                         aprovados_L05,aprovados_L06,
                                         aprovados_L09,aprovados_L10,
                                         aprovados_L13,aprovados_L14))
  lista_todos <<- lista_todos_original # recupera lista_todos
}
} # termina de criar funções ordem

# fim do código
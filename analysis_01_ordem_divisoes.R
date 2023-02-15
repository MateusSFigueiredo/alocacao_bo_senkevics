# ==============================================================================
# Arquivo: analysis_01_ordem_divisoes
# Ordem das divisões

# Modificado em 2023-02-15.
# Autor: Mateus Silva Figueiredo

# ==============================================================================

# De acordo com Bó e Senkevics, 2023, existem nove divisões a serem feitas
# Elas podem ser feitas em qualquer ordem
# Assim sendo, é possível fazer as divisões em 9! = 362880 ordens diferentes
# No entanto, nem todas as ordens fazem sentido.
# As que fazem sentido seguem a ordem A0 - L5 - ... - L10
# Apresentamos uma proposta de atribuir peso às categorias (bxa, ppi, pcd)
# Elas podem ter peso 1, 2 ou 4.
# Assim, há seis ordens que fazem sentido.
# Este código calcula elas, de acordo com o peso atribuído a cada categoria.
# Teoricamente, as ordens podem ser seguidas do A0 até L10, ou o oposto.

# ==============================================================================
# carregar biblioteca
library(dplyr)

# ==============================================================================
# carregar função de permutações

getPermutations <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(res)
  }
}

# ==============================================================================
# Peso 1, 2, 4

# obter permutações
permutations<-getPermutations(c(1,2,4))

# Montar todas_as_ordens

todas_as_ordens <- data.frame(matrix(ncol=12+9,nrow=6))
colnames(todas_as_ordens) <- c("bxa","ppi","pcd",
                               "A0",
                               "L01","L02","L05","L06",
                               "L09","L10","L13","L14",
                               "1o","2o","3o","4o","5o","6o","7o","8o","9o")

# ------------------------------------------------------------------------------

# abre for loop
# i<-1

for (i in 1:6){

pub <- 0.1 # para que L05 fique perto de A0
bxa <- todas_as_ordens[i,1] <- permutations[i,1]
ppi <- todas_as_ordens[i,2] <- permutations[i,2]
pcd <- todas_as_ordens[i,3] <- permutations[i,3]

todas_as_ordens[i,4]  <- 0                       #A0
todas_as_ordens[i,5]  <- pub + bxa               #L01
todas_as_ordens[i,6]  <- pub + bxa + ppi         #L02
todas_as_ordens[i,7]  <- pub                     #L05
todas_as_ordens[i,8]  <- pub +     + ppi         #L06
todas_as_ordens[i,9]  <- pub + bxa       + pcd   #L09
todas_as_ordens[i,10] <- pub + bxa + ppi + pcd   #L10
todas_as_ordens[i,11] <- pub +           + pcd   #L13
todas_as_ordens[i,12] <- pub       + ppi + pcd   #L14

}


# ------------------------------------------------------------------------------
# obter as ordens

i<-1
for (i in 1:6){
todas_as_ordens[i,4:12][order(todas_as_ordens[i,4:12])] %>% colnames() %>% paste() ->
  todas_as_ordens[i,13:21]
}

todas_as_ordens_2 <- todas_as_ordens[c(1:3,13:21)]

# ==============================================================================
# Pesos 2, 3, 4

# obter permutações
permutations<-getPermutations(c(2,3,4))

# Montar todas_as_ordens

todas_as_ordens <- data.frame(matrix(ncol=12+9,nrow=6))
colnames(todas_as_ordens) <- c("bxa","ppi","pcd",
                               "A0",
                               "L01","L02","L05","L06",
                               "L09","L10","L13","L14",
                               "1o","2o","3o","4o","5o","6o","7o","8o","9o")

# ------------------------------------------------------------------------------

# abre for loop
# i<-1

for (i in 1:6){
  
  pub <- 0.1 # para que L05 fique perto de A0
  bxa <- todas_as_ordens[i,1] <- permutations[i,1]
  ppi <- todas_as_ordens[i,2] <- permutations[i,2]
  pcd <- todas_as_ordens[i,3] <- permutations[i,3]
  
  todas_as_ordens[i,4]  <- 0                       #A0
  todas_as_ordens[i,5]  <- pub + bxa               #L01
  todas_as_ordens[i,6]  <- pub + bxa + ppi         #L02
  todas_as_ordens[i,7]  <- pub                     #L05
  todas_as_ordens[i,8]  <- pub +     + ppi         #L06
  todas_as_ordens[i,9]  <- pub + bxa       + pcd   #L09
  todas_as_ordens[i,10] <- pub + bxa + ppi + pcd   #L10
  todas_as_ordens[i,11] <- pub +           + pcd   #L13
  todas_as_ordens[i,12] <- pub       + ppi + pcd   #L14
  
}


# ------------------------------------------------------------------------------
# obter as ordens

i<-1
for (i in 1:6){
  todas_as_ordens[i,4:12][order(todas_as_ordens[i,4:12])] %>% colnames() %>% paste() ->
    todas_as_ordens[i,13:21]
}

todas_as_ordens_3 <- todas_as_ordens[c(1:3,13:21)]


# ------------------------------------------------------------------------------
todas_as_ordens <- rbind(todas_as_ordens_2,todas_as_ordens_3)


# ==============================================================================
# Peso -1, -2, -4
# sequência reversa da ordem 1, 2, 4

# obter permutações
permutations<-getPermutations(c(-1,-2,-4))

# Montar todas_as_ordens

todas_as_ordens <- data.frame(matrix(ncol=12+9,nrow=6))
colnames(todas_as_ordens) <- c("bxa","ppi","pcd",
                               "A0",
                               "L01","L02","L05","L06",
                               "L09","L10","L13","L14",
                               "1o","2o","3o","4o","5o","6o","7o","8o","9o")

# ------------------------------------------------------------------------------

# abre for loop
# i<-1

for (i in 1:6){
  
  pub <- -0.1 # para que L05 fique perto de A0
  bxa <- todas_as_ordens[i,1] <- permutations[i,1]
  ppi <- todas_as_ordens[i,2] <- permutations[i,2]
  pcd <- todas_as_ordens[i,3] <- permutations[i,3]
  
  todas_as_ordens[i,4]  <- 0                       #A0
  todas_as_ordens[i,5]  <- pub + bxa               #L01
  todas_as_ordens[i,6]  <- pub + bxa + ppi         #L02
  todas_as_ordens[i,7]  <- pub                     #L05
  todas_as_ordens[i,8]  <- pub +     + ppi         #L06
  todas_as_ordens[i,9]  <- pub + bxa       + pcd   #L09
  todas_as_ordens[i,10] <- pub + bxa + ppi + pcd   #L10
  todas_as_ordens[i,11] <- pub +           + pcd   #L13
  todas_as_ordens[i,12] <- pub       + ppi + pcd   #L14
  
}


# ------------------------------------------------------------------------------
# obter as ordens

i<-1
for (i in 1:6){
  todas_as_ordens[i,4:12][order(todas_as_ordens[i,4:12])] %>% colnames() %>% paste() ->
    todas_as_ordens[i,13:21]
}

todas_as_ordens_m_2 <- todas_as_ordens[c(1:3,13:21)]

# ==============================================================================
# Pesos -2, -3, -4
# Sequência reversa da ordem 2, 3, 4

# obter permutações
permutations<-getPermutations(c(-2,-3,-4))

# Montar todas_as_ordens

todas_as_ordens <- data.frame(matrix(ncol=12+9,nrow=6))
colnames(todas_as_ordens) <- c("bxa","ppi","pcd",
                               "A0",
                               "L01","L02","L05","L06",
                               "L09","L10","L13","L14",
                               "1o","2o","3o","4o","5o","6o","7o","8o","9o")

# ------------------------------------------------------------------------------

# abre for loop
# i<-1

for (i in 1:6){
  
  pub <- -0.1 # para que L05 fique perto de A0
  bxa <- todas_as_ordens[i,1] <- permutations[i,1]
  ppi <- todas_as_ordens[i,2] <- permutations[i,2]
  pcd <- todas_as_ordens[i,3] <- permutations[i,3]
  
  todas_as_ordens[i,4]  <- 0                       #A0
  todas_as_ordens[i,5]  <- pub + bxa               #L01
  todas_as_ordens[i,6]  <- pub + bxa + ppi         #L02
  todas_as_ordens[i,7]  <- pub                     #L05
  todas_as_ordens[i,8]  <- pub +     + ppi         #L06
  todas_as_ordens[i,9]  <- pub + bxa       + pcd   #L09
  todas_as_ordens[i,10] <- pub + bxa + ppi + pcd   #L10
  todas_as_ordens[i,11] <- pub +           + pcd   #L13
  todas_as_ordens[i,12] <- pub       + ppi + pcd   #L14
  
}


# ------------------------------------------------------------------------------
# obter as ordens

i<-1
for (i in 1:6){
  todas_as_ordens[i,4:12][order(todas_as_ordens[i,4:12])] %>% colnames() %>% paste() ->
    todas_as_ordens[i,13:21]
}

todas_as_ordens_m_3 <- todas_as_ordens[c(1:3,13:21)]


# ------------------------------------------------------------------------------
# Unir todas_as_ordens

todas_as_ordens <- rbind(todas_as_ordens_2,todas_as_ordens_3,
                         todas_as_ordens_m_2,todas_as_ordens_m_3)

# Limpeza
rm(todas_as_ordens_2, todas_as_ordens_3,
  todas_as_ordens_m_2,todas_as_ordens_m_3,
   permutations)

# ==============================================================================
# Gambiarra
# Transformar todas_as_ordens em código a ser corrido

ordem_para_codigo <- todas_as_ordens
ordem_para_codigo <- ordem_para_codigo[,4:12]

nomes_das_ordens<-c(paste("ordem_",1:12,"a <- function(){",sep=""),
                    paste("ordem_",1:12,"b <- function(){",sep=""))

rownames(ordem_para_codigo) <- nomes_das_ordens

ordem_para_codigo$fim<-"}"


ordem_para_codigo[ordem_para_codigo=="A0"] <- "preenche_A0();"
ordem_para_codigo[ordem_para_codigo=="L01"] <- "preenche_L01();"
ordem_para_codigo[ordem_para_codigo=="L02"] <- "preenche_L02();"
ordem_para_codigo[ordem_para_codigo=="L05"] <- "preenche_L05();"
ordem_para_codigo[ordem_para_codigo=="L06"] <- "preenche_L06();"
ordem_para_codigo[ordem_para_codigo=="L09"] <- "preenche_L09();"
ordem_para_codigo[ordem_para_codigo=="L10"] <- "preenche_L10();"
ordem_para_codigo[ordem_para_codigo=="L13"] <- "preenche_L13();"
ordem_para_codigo[ordem_para_codigo=="L14"] <- "preenche_L14();"


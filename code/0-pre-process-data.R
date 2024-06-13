library(readxl)
library(mirt)
library(dplyr)

library(genderBR)


#### functions ####

sum2 <- function(x) {
  if (all(is.na(x))) NA else sum(x, na.rm = T)
}


toASCII <- function(x) {
  library(stringi)
  library(stringr)
  trimws(
    toupper(
      str_replace_all(
        stri_trans_general(x, id = "Latin-ASCII"), "[^0-9a-zA-Z]+", " "
      )
    )
  )
}

toResponse <- function(x) {
  if (is.na(x)) NA
  else if ("NR001" == toASCII(x)) NA
  else if ("NR002" == toASCII(x)) NA
  else if ("V002" == toASCII(x)) 1
  else if ("V003" == toASCII(x)) 1
  else if (!is.na(as.numeric(x)) && 1 == as.numeric(x)) as.numeric(x)
  else 0
}

#### load data ####

dat.EF02MA06 <- read_excel("pre-data/Parametros com base na TRI para os itens das competencias EF02MA06 e EF03MA07.xlsx",
                           sheet = "EF02MA06-ID")
dat.EF03MA07 <- read_excel("pre-data/Parametros com base na TRI para os itens das competencias EF02MA06 e EF03MA07.xlsx",
                           sheet = "EF03MA07-ID")

dat.EF02MA06[dat.EF02MA06 == -1] <- NA
dat.EF03MA07[dat.EF03MA07 == -1] <- NA

colnames(dat.EF02MA06) <- paste0(colnames(dat.EF02MA06),"_EF02MA06")
colnames(dat.EF03MA07) <- paste0(colnames(dat.EF03MA07),"_EF03MA07")

params <- read_excel("pre-data/Parametros com base na TRI para os itens das competencias EF02MA06 e EF03MA07.xlsx",
                     sheet = "Seleçao de Itens Pre e Pos-test")
colnames(params) <- c("item","a1","d","g","u","loc","remover")
params$item <- stringr::str_replace_all(params$item,"\\.","_")

dat <- read_excel("pre-data/Classificação dos Pré e Pós Testes.xlsx",
                  sheet = "Classificação (Corrigido)")
dat$Gender <- get_gender(dat$Estudante)

dat <- select(dat, -c("Estudante","NOME (na lista fornecida pela escola)"))
dat$ID <- paste0("P", as.integer(dat$ID))

for (cname in c(colnames(dat)[startsWith(colnames(dat), "Item")])) {
  dat[[cname]] = sapply(c(dat[[cname]]), FUN = toResponse)
}



#### Calculating Pre- and Post-scores ####

subset1 = c("Item07_EF02MA06","Item11_EF03MA07","Item11_EF02MA06","Item04_EF03MA07")
subset2 = c("Item06_EF02MA06","Item13_EF03MA07","Item01_EF02MA06","Item05_EF03MA07")

idx_ss1to2 = which(dat$Cod.Tutor %in% c("PS1","PS6"))  # index of students in which subset1 is pre-test and subset2 is post-test
idx_ss2to1 = which(dat$Cod.Tutor %in% c(
  "PS2","PS3","PS4","PS5",
  "PL1","PL2","PL3","PL4","PL5","PL8","PL10","PL11","PL12","PL13","PL14","PL15","PL16")) # index of students in which subset2 is pre-test and subset1 is post-test

dat[["pre.score"]] <- rep(NA, nrow(dat))
dat[["pre.score"]][idx_ss1to2] <- apply(dat[idx_ss1to2,c(subset1)], 1 , sum2)
dat[["pre.score"]][idx_ss2to1] <- apply(dat[idx_ss2to1,c(subset2)], 1 , sum2)

dat[["pos.score"]] <- rep(NA, nrow(dat))
dat[["pos.score"]][idx_ss1to2] <- apply(dat[idx_ss1to2,c(subset2)], 1 , sum2)
dat[["pos.score"]][idx_ss2to1] <- apply(dat[idx_ss2to1,c(subset1)], 1 , sum2)

#dat <- dat[which(!is.na(dat$pre.score) & !is.na(dat$pos.score)),]

#### IRT-Based Concurrent Calibration Using Pre-Calculated Parameters ####

##### estimate theta for the 1st subset of item params #####

pdat <- dat[,c(subset1)]
for (cname in subset2) pdat[[cname]] <- NA
pdat <- rbind(0, pdat)
pdat <- rbind(1, pdat)

sv <- mirt(pdat, itemtype= '3PL', pars = 'values')

for (cname in colnames(pdat)) {
  sv$value[sv$name == 'a1' & sv$item == cname] <- params$a1[params$item == cname]
  sv$value[sv$name == 'd' & sv$item == cname] <- params$d[params$item == cname]
  sv$value[sv$name == 'g' & sv$item == cname] <- params$g[params$item == cname]
  sv$value[sv$name == 'u' & sv$item == cname] <- params$u[params$item == cname]
}
sv$est <- FALSE

md <- mirt(pdat, itemtype = '3PL', pars = sv)

coef(md, simplify = TRUE, IRTpars = TRUE)

coef(md, simplify = TRUE, IRTpars = FALSE)

fscores.ss1 <- tail(c(fscores(md, method = "EAP")),-2)

##### estimate theta for the 2nd subset of item params #####

pdat <- dat[,c(subset2)]
for (cname in subset1) pdat[[cname]] <- NA
pdat <- rbind(0, pdat)
pdat <- rbind(1, pdat)

sv <- mirt(pdat, itemtype= '3PL', pars = 'values')

for (cname in colnames(pdat)) {
  sv$value[sv$name == 'a1' & sv$item == cname] <- params$a1[params$item == cname]
  sv$value[sv$name == 'd' & sv$item == cname] <- params$d[params$item == cname]
  sv$value[sv$name == 'g' & sv$item == cname] <- params$g[params$item == cname]
  sv$value[sv$name == 'u' & sv$item == cname] <- params$u[params$item == cname]
}
sv$est <- FALSE

md <- mirt(pdat, itemtype = '3PL', pars = sv)

coef(md, simplify = TRUE, IRTpars = TRUE)

coef(md, simplify = TRUE, IRTpars = FALSE)

fscores.ss2 <- tail(c(fscores(md, method = "EAP")),-2)


dat[["pre.irt.cc"]] <- rep(NA, nrow(dat))
dat[["pre.irt.cc"]][idx_ss1to2] <- fscores.ss1[idx_ss1to2]
dat[["pre.irt.cc"]][idx_ss2to1] <- fscores.ss2[idx_ss2to1]

dat[["pos.irt.cc"]] <- rep(NA, nrow(dat))
dat[["pos.irt.cc"]][idx_ss1to2] <- fscores.ss2[idx_ss1to2]
dat[["pos.irt.cc"]][idx_ss2to1] <- fscores.ss1[idx_ss2to1]



#### IRT-Based Scoring Using Equation Mode (including previous data) ####

library(plyr)

##### estimate theta for the 1st subset of params #####

pdat <- dat[,c(subset1)]

pdat <- rbind(0, pdat)
pdat <- rbind(1, pdat)

pdat <- rbind.fill(pdat, dat.EF02MA06[,colnames(dat.EF02MA06) %in% c(subset1)])
pdat <- rbind.fill(pdat, dat.EF03MA07[,colnames(dat.EF03MA07) %in% c(subset1)])

md <- mirt(pdat, itemtype = '3PL')

coef(md, simplify = TRUE, IRTpars = TRUE)

coef(md, simplify = TRUE, IRTpars = FALSE)

fscores.ss1 <- c(fscores(md, method = "EAP"))

fscores.ss1.max <- fscores.ss1[1]
fscores.ss1.min <- fscores.ss1[2]

fscores.ss1 <- tail(fscores.ss1, -2)

##### estimate theta for the 2nd subset of params #####

pdat <- dat[,c(subset2)]

pdat <- rbind(0, pdat)
pdat <- rbind(1, pdat)

pdat <- rbind.fill(pdat, dat.EF02MA06[,colnames(dat.EF02MA06) %in% c(subset2)])
pdat <- rbind.fill(pdat, dat.EF03MA07[,colnames(dat.EF03MA07) %in% c(subset2)])

md <- mirt(pdat, itemtype = '3PL')

coef(md, simplify = TRUE, IRTpars = TRUE)

coef(md, simplify = TRUE, IRTpars = FALSE)

fscores.ss2 <- c(fscores(md, method = "EAP"))

fscores.ss2.max <- fscores.ss2[1]
fscores.ss2.min <- fscores.ss2[2]

fscores.ss2 <- tail(fscores.ss2, -2)


dat[["pre.irt2"]] <- rep(NA, nrow(dat))
dat[["pre.irt2"]][idx_ss1to2] <- fscores.ss1[idx_ss1to2]
dat[["pre.irt2"]][idx_ss2to1] <- fscores.ss2[idx_ss2to1]

dat[["pos.irt2"]] <- rep(NA, nrow(dat))
dat[["pos.irt2"]][idx_ss1to2] <- fscores.ss2[idx_ss1to2]
dat[["pos.irt2"]][idx_ss2to1] <- fscores.ss1[idx_ss2to1]


##### equate estimated abilities in 1st and 2nd subset #####

fscores.ss1.norm = 0 + (((fscores.ss1 - fscores.ss1.min) * 100)/(fscores.ss1.max - fscores.ss1.min))
fscores.ss2.norm = 0 + (((fscores.ss2 - fscores.ss2.min) * 100)/(fscores.ss2.max - fscores.ss2.min))


dat[["pre.irt.em.norm"]] <- rep(NA, nrow(dat))
dat[["pre.irt.em.norm"]][idx_ss1to2] <- fscores.ss1.norm[idx_ss1to2]
dat[["pre.irt.em.norm"]][idx_ss2to1] <- fscores.ss2.norm[idx_ss2to1]

dat[["pos.irt.em.norm"]] <- rep(NA, nrow(dat))
dat[["pos.irt.em.norm"]][idx_ss1to2] <- fscores.ss2.norm[idx_ss1to2]
dat[["pos.irt.em.norm"]][idx_ss2to1] <- fscores.ss1.norm[idx_ss2to1]




#### Calculating Quintiles in Summary ####


for (coln in c("score","irt.cc","irt2","irt.em.norm")) {
  max.qtl <- max(dat[[paste0('pre.',coln)]], na.rm = T)
  qtl <- quantile(dat[[paste0('pre.',coln)]], probs = seq(0, 1, 1/5), na.rm = T)

  dat[[paste0('qtl.',coln)]] <- sapply(dat[[paste0('pre.',coln)]], function(v) {
    if (!is.na(v) && is.numeric(v)) {
      if (v < qtl[2]) "1st"
      else if (v < qtl[3]) "2nd"
      else if (v <= qtl[4]) "3rd"
      else if (v <= qtl[5]) "4th"
      else "5th"
    } else NA })
}



#### Setting Groups ####


dat$Group <- rep(NA, nrow(dat))
dat$Group[dat$Cod.Tutor %in% c("PS4","PS5","PS6")] <- "Ctr.B"
dat$Group[dat$Cod.Tutor %in% c("PS1","PS2","PS3")] <- "Exp"

dat$Group[dat$Cod.Tutor %in% c("PL1","PL2","PL3")] <- "Ctr.A"
dat$Group[dat$Cod.Tutor %in% c("PL11","PL12","PL13","PL14","PL15","PL16")] <- "Ctr.B"
dat$Group[dat$Cod.Tutor %in% c("PL4","PL5","PL6","PL7","PL8","PL9","PL10")] <- "Exp"


#### Saving final data ####

dat$Ano <- paste0(dat$Ano,"a")

colnames(dat)[c(3,4,5)] <- c("Town","School","Degree")

df.legend = data.frame(
  column = colnames(dat),
  description = c(
    "Identifier of participant", "Cod.Tutor", "Town", "School", "Degree",
    colnames(dat)[startsWith(colnames(dat),"Item")],
    "Gender of participants",
    "Number of correct answers in the pre-test",
    "Number of correct answers in the post-test",
    "Estimated ability in the pre-test using concurrent calibration",
    "Estimated ability in the post-test using concurrent calibration",

    "Estimated ability in the pre-test using new and previous data",
    "Estimated ability in the post-test using new and previous data",
    "Estimated ability in the pre-test using equation mode to normalize pre.irt2",
    "Estimated ability in the post-test using equation mode to normalize pre.irt2",
    "Quintile for the score",
    "Quintile for the pre.irt.cc",
    "Quintile for the pre.irt2",
    "Quintile for the pre.irt.em.norm",
    "Groups"
  )
)




values = c("score","irt.cc","irt2","irt.em.norm")
lvalues = as.list(values); names(lvalues) = values

for (x in names(lvalues))
  dat[[paste0("dif.",x)]] <- dat[[paste0("pos.",x)]] - dat[[paste0("pre.",x)]]


writexl::write_xlsx(list(data = dat, legend = as.data.frame(df.legend)), "data/data.xlsx")






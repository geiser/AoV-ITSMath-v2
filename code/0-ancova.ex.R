wants <- c('ggplot2','ggpubr','templates','PerformanceAnalytics','utils','randomcoloR')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(knitr)
library(templates)
library(markdown)

for (ofile in c("H1-H3","H4")) {
  dat.filter = ""
  if (ofile == "H4")
    dat.filter = paste0("gdat <- gdat[which(gdat$dif.irt.em.norm != 0 & gdat$pre.irt.em.norm != 100),]")

  ofile = paste0("code/",ofile,"-irt.em.norm.Rmd")

  tfile = "templates/non-param-aov.Rmd"
  params = list(
    title = "for assess H4(null) hypothesis",
    dv = "irt.em.norm", dv.dif = "dif.irt.em.norm",
    dv.pre = "pre.irt.em.norm", dv.pos = "pos.irt.em.norm",
    fatores = c("Gender","Town","Degree","qtl.irt.em.norm"),
    dat.filter = dat.filter
  )
  txt <- do.call(tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))


  tfile =  "templates/non-param-aov-one.Rmd"
  params = c(params, list(
    title = paste0("One-way factor analysis for: *irt.em.norm ~ Group*"),
    iv = "Group", pivot.key = "time", pivot.value = "irt.em.norm",
    fig.width = 8, fig.height = 8,
    pfig.width = 12, pfig.height = 8
  ))
  txt <- paste0(
    txt, "\n",
    do.call(tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params)))

  for (iv2 in c("Gender","Town","Degree","qtl.irt.em.norm")) {
    tfile =  "templates/non-param-aov-two.Rmd"
    params = c(params, list(
      iv1 = "Group", iv2 = iv2,
      title = paste0("Two-way factor analysis for: *irt.em.norm ~ Group:",iv2,"*")
    ))
    txt <- paste0(txt, "\n", do.call(
      tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params)))
  }

  if (!file.exists(ofile)) {
    writeLines(txt, ofile, useBytes=T)
    rmarkdown::render(
      ofile, output_dir = './results', clean = T,
      output_format = c("github_document","word_document","html_document"))
  }

}








    writeLines(txt, file.input, useBytes=T)
  }
}



generate_wg <- function(
    factors = c("Sexo","Zona","Cor.Raca","Serie"),
    info = list(color = "#008000", grupo = "WG.Grupo",
                prefix=paste0(getwd(),'/code/aov-wordgen')),
    only.dvs = c(),
    other.factors = c(), dat.filter = "", suffix = "", fig.size = list()) {

  dvs.flow = list(
    list(ylab="flow (debat)",
         dv.pre = "dfs.media.debat", dv.pos = "fss.media.debat", dv = "flow.debat"),
    list(ylab="flow (textual prod)",
         dv.pre = "dfs.media.text", dv.pos = "fss.media.text", dv = "flow.text"),
    list(ylab="flow (reading)",
         dv.pre = "dfs.media.read", dv.pos = "fss.media.read", dv = "flow.read"),
    list(ylab="flow (math)",
         dv.pre = "dfs.media.math", dv.pos = "fss.media.math", dv = "flow.math")
  )


  dvs = c(list(
    list(ylab = "Vocabulary",dv.pre = "vocab.pre", dv.pos = "vocab.pos", dv = "vocab"),
    list(ylab = "Vocabulary taught",dv.pre = "vocab.teach.pre", dv.pos = "vocab.teach.pos", dv = "vocab.teach"),
    list(ylab = "Vocabulary not taught",dv.pre = "vocab.non.teach.pre", dv.pos = "vocab.non.teach.pos", dv = "vocab.non.teach"),
    list(ylab = "Writing (TDE)",dv.pre = "score.tde.pre", dv.pos = "score.tde.pos", dv = "score.tde")
  ), dvs.flow)

  for (dv in dvs) {
    if (length(only.dvs) > 0 && !(dv$dv %in% only.dvs)) next
    generate_md(info, dv, factors, other.factors, dat.filter, suffix, fig.size)
  }
}


generate_stari <- function(
    factors = c("Sexo","Zona","Cor.Raca","Serie"),
    info = list(color = "#fd7f6f", grupo = "Stari.Grupo",
                prefix=paste0(getwd(),'/code/aov-stari')),
    only.dvs = c(),
    other.factors = c(), dat.filter = "", suffix = "", fig.size = list()) {

  dvs.flow = list(
    list(ylab="flow (debat)",
         dv.pre = "dfs.media.debat", dv.pos = "fss.media.debat", dv = "flow.debat"),
    list(ylab="flow (textual prod)",
         dv.pre = "dfs.media.text", dv.pos = "fss.media.text", dv = "flow.text"),
    list(ylab="flow (reading)",
         dv.pre = "dfs.media.read", dv.pos = "fss.media.read", dv = "flow.read"),
    list(ylab="flow (math)",
         dv.pre = "dfs.media.math", dv.pos = "fss.media.math", dv = "flow.math")
  )

  dvs = c(list(
    list(ylab = "Vocabulary",dv.pre = "vocab.pre", dv.pos = "vocab.pos", dv = "vocab"),
    list(ylab = "Vocabulary taught",dv.pre = "vocab.teach.pre", dv.pos = "vocab.teach.pos", dv = "vocab.teach"),
    list(ylab = "Vocabulary not taught",dv.pre = "vocab.non.teach.pre", dv.pos = "vocab.non.teach.pos", dv = "vocab.non.teach"),
    list(ylab = "Writing (TDE)",dv.pre = "score.tde.pre", dv.pos = "score.tde.pos", dv = "score.tde"),

    list(ylab = "Reading Words (1 Min)",
         dv.pre = "TFL.lidas.per.min.pre", dv.pos = "TFL.lidas.per.min.pos", dv = "TFL.lidas.per.min"),
    list(ylab = "Reading Correct Words (1 Min)",
         dv.pre = "TFL.corretas.per.min.pre", dv.pos = "TFL.corretas.per.min.pos", dv = "TFL.corretas.per.min"),
    list(ylab = "Reading Comprehension",
         dv.pre = "leitura.compreensao.pre", dv.pos = "leitura.compreensao.pos", dv = "leitura.compreensao")
    ), dvs.flow)


  for (dv in dvs) {
    if (length(only.dvs) > 0 && !(dv$dv %in% only.dvs)) next
    generate_md(info, dv, factors, other.factors, dat.filter, suffix, fig.size)
  }
}



other.factors = list(
  vocab = "vocab.quintile",
  vocab.teach = "vocab.teach.quintile",
  vocab.non.teach = "vocab.non.teach.quintile",
  score.tde = "score.tde.quintile",
  TFL.lidas.per.min = "TFL.lidas.per.min.quintile",
  TFL.corretas.per.min = "TFL.corretas.per.min.quintile",
  leitura.compreensao = "leitura.compreensao.quintile"
)

fig.size = list(
  "grupo:vocab.quintile" = list(fig.width.pbar = 21, fig.height.pbar = 6),
  "grupo:score.tde.quintile" = list(fig.width.pbar = 21, fig.height.pbar = 6),
  "grupo:TFL.lidas.per.min.quintile" = list(fig.width.pbar = 21, fig.height.pbar = 6),
  "grupo:TFL.corretas.per.min.quintile" = list(fig.width.pbar = 21, fig.height.pbar = 6),
  "grupo:leitura.compreensao.quintile" = list(fig.width.pbar = 21, fig.height.pbar = 6)
)





library(readxl)
data <- read_excel("data/data.xlsx")


## ... generate wordgen

generate_wg(other.factors = other.factors, fig.size = fig.size)

## ... generate stari

generate_stari(other.factors = other.factors, fig.size = fig.size)




for (filter.val in unique(data$Serie)[!is.na(unique(data$Serie))]){
  if (filter.val %in% c("8 ano", "6 ano","9 ano","7 ano")) next
  suffix = paste0('-Serie-',filter.val)
  dat.filter = paste0('gdat <- gdat[which(gdat$Serie == "',filter.val,'"),]')
  generate_wg(dat.filter = dat.filter, suffix = suffix,
              factors = c("Sexo","Zona","Cor.Raca"),
              other.factors = other.factors, fig.size = fig.size)
}


for(dv in names(other.factors)) {
  f = other.factors[[dv]]
  for (val in unique(data[[f]][which(!is.na(data[[paste0(dv,".pos")]]))])) {
    # 4th quintile 1st quintile 3th quintile 2nd quinti
    if (is.na(val)) next
    suffix = paste0('-',val)
    dat.filter = paste0('gdat <- gdat[which(gdat[["',f,'"]] == "',val,'"),]')
    generate_wg(dat.filter = dat.filter, suffix = suffix, only.dvs = dv)
  }
}




for (filter.val in unique(data$Serie)[!is.na(unique(data$Serie))]){
  if (filter.val %in% c("8 ano","6 ano", "9 ano", "7 ano")) next
  suffix = paste0('-Serie-',filter.val)
  dat.filter = paste0('gdat <- gdat[which(gdat$Serie == "',filter.val,'"),]')
  generate_stari(dat.filter = dat.filter, suffix = suffix,
                 factors = c("Sexo","Zona","Cor.Raca"),
                 other.factors = other.factors, fig.size = fig.size)
}


for(dv in names(other.factors)) {
  f = other.factors[[dv]]
  for (val in unique(data[[f]][which(!is.na(data[[paste0(dv,".pos")]]))])) {
    if (is.na(val)) next
    suffix = paste0('-',val)
    dat.filter = paste0('gdat <- gdat[which(gdat[["',f,'"]] == "',val,'"),]')
    generate_stari(dat.filter = dat.filter, suffix = suffix, only.dvs = dv)
  }
}



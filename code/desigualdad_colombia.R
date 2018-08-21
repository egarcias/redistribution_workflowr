###############################################################################
#
# Estadísticos descriptivos proyecto percepciones de la desigualdad, ideologías y redistribución
# en Colombia
# Escrito por: Efraín Garcia-Sanchez.
# e-mail: egarcias@correo.ugr.es or egarsa@gmail.com
# Por favor contactarme si encuentra algún error o tiene alguna pregunta
# última actualización: 27/06/2018
#
###############################################################################
setwd("C:/Users/Efrain/Dropbox/PhD_UGR/Estancia COES Chile/Desigualdad_Colombia")
load("C:/Users/Efrain/Dropbox/PhD_UGR/Estancia COES Chile/Desigualdad_Colombia/desigualdad_colombia.R.RData")

##### 0. Cargar paquetes#####
library(pacman)
pacman::p_load(plyr, foreign, car, ggplot2, reshape, lme4, interplot,
                devtools,reghelper,stargazer, psych, interplot, lavaan,
                dplyr, haven, data.table, gridExtra,reghelper, labelled, lmeresampler,
                sjstats, sjPlot, sjlabelled,sjmisc,memisc, knitr, GPArotation)




##### 1. Estudio 1. #####

##### 1.1 cargar datos Estudio 1#####

data1_raw <- read_sav("SDO_2_UV.sav")
data1_raw$sexo[data1_raw$sexo==99] <- NA        #Setting 99 as missing values

data1 <- data1_raw[,c(1,20:70,91:100)] # Selecciono variables de interés

##### 1.2 Libro de códigos #####

##### 1.2.1 libro de códigoso para informe #####

library(codebook)
cdbk_data1 <- codebook_table(data1)
cdbk_data1 <- subset(cdbk_data1, select = c("name",
                                            "label",
                                            "data_type",
                                            "value_labels",
                                            "missing",
                                            "complete",
                                            "n"))

knitr::kable(cdbk_data1)

show_html(cdbk_data1)

#### 1.2.2 Libro de códigos para exxportar (con descritivos)#####

library(memisc)
data1a <- as.data.set(spss.portable.file("SDO_2_UV.por"))
data1a <- subset(data1a, select= c(1,20:70,91:100))
cdbk_data1a <- memisc::codebook(data1a)
show_html(cdbk_data1a)

memisc::description(data1a)


##### 1.3 Estadístico descriptivos por item #####

library(sjmisc)
descrip_data1 <- descr(data1[,c(2:59)])
knitr::kable(descrip_data1)

##### 1.4 Análisis escala de Redistribución #####
##### 1.4.1 Descriptivos Redistribución #####
redis_data1 <- data1[,c("Q1_red1","Q1_red2","Q1_red3","Q1_red4","Q1_red5",
                        "Q1_red6","Q1_red7","Q1_red8","Q1_red9","Q1_red10")]

get_labels(redis_data1, include.non.labelled = TRUE) #se puede borrar

redis_data1 <- set_labels(redis_data1, labels = c("Totalmente en Desacuerdo",
                                             "Mayoritariamente en Desacuerdo",
                                             "Ligeramente en Desacuerdo",
                                             "Neutro",
                                             "Ligeramente de Acuerdo",
                                             "Mayormente de Acuerdo",
                                             "Totalmene de Acuerdo"))

redis_data1 <- to_factor(redis_data1) # para convertir un objeto labelled en vectores de tipo factor

install.packages("likert")
library(likert)
redis1_likert <- likert(as.data.frame(redis_data1)) #tiene que ser un objeto con vectores de tipo factor

plot(redis1_likert,
     type = "bar",
     group.order =  names(redis_data1)) # in the order of the names of the data set

#demo of likert package can be found here:
#https://github.com/jbryer/likert/blob/master/demo/likert.R

#otra opción
# sjp.likert(redis_data1)

##### 1.4.2 Fiabilidad Redistribución #####
sjt.itemanalysis(data1[,c("Q1_red1","Q1_red2","Q1_red3","Q1_red4","Q1_red5",
                          "Q1_red6","Q1_red7_rec","Q1_red8","Q1_red9","Q1_red10")],
                 encoding = "Windows-1252") ### clave para presentar los descriptivos por escala


##### 1.4.3 Dimensionalidad Redistribución #####

parallel <- fa.parallel(data1[,c("Q1_red1","Q1_red2","Q1_red3","Q1_red4","Q1_red5",
                                 "Q1_red6","Q1_red7_rec","Q1_red8","Q1_red9","Q1_red10")],
                        fm = 'minres', fa = 'fa')

redis1_two_factor <- fa((data1[,c("Q1_red1","Q1_red2","Q1_red3","Q1_red4","Q1_red5",
                                  "Q1_red6","Q1_red7_rec","Q1_red8","Q1_red9","Q1_red10")]),
                        nfactors = 2,rotate = "varimax",fm="minres")

print.psych(redis1_two_factor, cut = 0.3, sort = TRUE)

fa.diagram(redis1_two_factor)

redis1_two_factorb <- fa((data1[,c("Q1_red1","Q1_red3","Q1_red4","Q1_red5",
                                  "Q1_red6","Q1_red7_rec","Q1_red8","Q1_red9")]),
                        nfactors = 2,rotate = "varimax",fm="minres")

print.psych(redis1_two_factorb, cut = 0.3, sort = TRUE)






# fiabilidad de las subdimensiones

sjt.itemanalysis(data1[,c("Q1_red1","Q1_red4","Q1_red6","Q1_red7_rec")])

sjt.itemanalysis(data1[,c("Q1_red3","Q1_red5","Q1_red8","Q1_red9")])


##### 1.5 Análisis escala perceción desigualdad ####
##### 1.5.1 Descriptivos percepción desigualdad ####

fram_data1 <- subset(data1, select = c("framing_fram1","framing_fram2","framing_fram3",
                                       "framing_fram4","framing_fram5","framing_fram6"))
get_labels(fram_data1, include.values = "p")
fram_data1 <- set_labels(fram_data1, labels =  c("1","2","3","4","5","6","7","8","9","10"))
fram_data1 <- to_factor(fram_data1)
fram1_likert <- likert(as.data.frame(fram_data1))

plot(fram1_likert,
     type = "bar",
     group.order = names(fram_data1))

##### 1.5.2 Fiabilidad percepciónn desigualdad####
sjt.itemanalysis(data1[,c("framing_fram1","framing_fram2","framing_fram3",
                          "framing_fram4","framing_fram5","framing_fram6")])


##### 1.5.3 Dimensionalidad percepción desigualdad ####

parallel <- fa.parallel(data1[,c("framing_fram1","framing_fram2","framing_fram3",
                                 "framing_fram4","framing_fram5","framing_fram6")],
                        fm = 'minres', fa = 'fa')

fram1_two_factor <- fa((data1[,c("framing_fram1","framing_fram2","framing_fram3",
                                 "framing_fram4","framing_fram5","framing_fram6")]),
                        nfactors = 2,rotate = "varimax",fm="minres")

print.psych(fram1_two_factor, cut = 0.3, sort = TRUE)

fa.diagram(fram1_two_factor)

memisc::description(data1a[,c("framing1", "framing2", "framing3",
                              "framing4", "framing5", "framing6")])

data1a <- colrename(data1a,  ###Para cambiar los nombres de los ítems en el objeto data.set de memisc
          "framing_" ="framing1",
          "framin_1" ="framing2",
          "framin_2" = "framing3",
          "framin_3" = "framing4",
          "framin_4" = "framing5",
          "framin_5" = "framing6")

##### 1.6 Cómputo de variables ####

data1 <- data1 %>%
  mutate(framing = (framing_fram1+framing_fram2+framing_fram3+framing_fram4+framing_fram5+framing_fram6)/6, # framing
         menostien = (framing_fram4+framing_fram5)/2, # framing los que menos tienen
         mastien = (framing_fram1+framing_fram2+framing_fram3+framing_fram6)/4, # framing los que más tienen
         jse = (Q1_jse1+Q1_jse2+Q1_jse3)/3, # justficación del sistema económico
         patrio = (Q1_patrio1 + Q1_patrio2)/2, #  Patriotismo
         sdo = (sdo7_dom1+sdo7_dom2_rec+sdo7_dom3+sdo7_dom4+
                  sdo7_dom5+sdo7_dom6_rec+sdo7_dom7_rec+sdo7_dom8_rec+
                  sdo7_antieg1_rec+sdo7_antieg2+sdo7_antieg3_rec+sdo7_antieg4+
                  sdo7_antieg5_rec+sdo7_antieg6+sdo7_antieg7+sdo7_antieg8_rec)/16,
         sdo_dom = (sdo7_dom1+sdo7_dom2_rec+sdo7_dom3+sdo7_dom4+
                      sdo7_dom5+sdo7_dom6_rec+sdo7_dom7_rec+sdo7_dom8_rec)/8,
         sdo_antieg =  (sdo7_antieg1_rec+sdo7_antieg2+sdo7_antieg3_rec+sdo7_antieg4+
                          sdo7_antieg5_rec+sdo7_antieg6+sdo7_antieg7+sdo7_antieg8_rec)/8,
         redis = (Q1_red1+ Q1_red2+ Q1_red3+ Q1_red4+ Q1_red5+
                    Q1_red6+ Q1_red7_rec+ Q1_red8+ Q1_red9+Q1_red10)/10, # redistribución
         redis_reg = (Q1_red1+ Q1_red4+ Q1_red6+ Q1_red7_rec)/4, # redistribución centrado en el gobierno
         redis_prov = (Q1_red2+ Q1_red3+ Q1_red5+ Q1_red8+ Q1_red9)/4 #redistribución centrado en proveer
         )
names(data1)

##### 1.7 Estadísticos descriptivos variables computadas ####

describe(data1[,c(63:73)], fast = T) # estadísticos descriptivos
descr(data1[,c(63:73)]) # estadísticos descriptivos

sjt.corr(data1[,c(63:73)]) # tabla de correlaciones

cor.plot(data1[,c(63:73)], numbers =TRUE, stars = TRUE)





##### 2. Estudio 2. #####

##### 2.1 Cargar datos estudio 2#####

library(haven)
data2_raw <- read_sav("data2_raw.sav")

data2 <- data2_raw[,c(1,15:74)]

##### 2.1.1 Recodificar variables#####

## Redis7 is a reversed score item
data2$redis7 <- to_factor(data2$redis7) ### To convert labelled data into factor, due to consntrain imposed by labelled data
data2$redis7 <- as.numeric(data2$redis7) ### to convert factors in numeric data
data2$redis7r <- dplyr::recode(data2$redis7, `1` = 7L, `2` = 6L, '3' = 5L, '4' = 4L, '5' = 3L, '6' = 2L, '7' = 1L) ### To recode values, so now higher values, higher the variable
table(data2$redis7)
table(data2$redis7r) ###To check if converson was conducted appropriately

data2$desgral <- to_factor(data2$desgral) ### To convert labelled data into factor, due to consntrain imposed by labelled data
data2$desgral <- as.numeric(data2$desgral) ### to convert factors in numeric data
data2$desgralr <- dplyr::recode(data2$desgral, `1` = 7L, `2` = 6L, '3' = 5L, '4' = 4L, '5' = 3L, '6' = 2L, '7' = 1L) ### To recode values, so now higher values, higher the variable
table(data2$desgral)
table(data2$desgralr) ###To check if converson was conducted appropriately

data2$desgral2 <- to_factor(data2$desgral2) ### To convert labelled data into factor, due to consntrain imposed by labelled data
data2$desgral2 <- as.numeric(data2$desgral2) ### to convert factors in numeric data
data2$desgral2r <- dplyr::recode(data2$desgral2, `1` = 7L, `2` = 6L, '3' = 5L, '4' = 4L, '5' = 3L, '6' = 2L, '7' = 1L) ### To recode values, so now higher values, higher the variable
table(data2$desgral2)
table(data2$desgral2r) ###To check if converson was conducted appropriately


##### 2.2 Libro de códigos #####

library(codebook)
cdbk_data2 <- codebook_table(data2)
names(cdbk_data2)
cdbk_data2 <- cdbk_data2[,c("name","label","data_type","value_labels","missing","complete","n")]

knitr::kable(cdbk_data2)

show_html(cdbk_data2)

#Para usar el paquete memisc
library(memisc)
data2a <- as.data.set(spss.portable.file("data2_raw.por"))
data2a <- subset(data2a, select= c(1,15:74))
cdbk_data2a <- memisc::codebook(data2a)
show_html(cdbk_data2a)

memisc::description(data2a)



#### 2.3 Estadísticos descrpitivos por item####

library(sjmisc)
descrip_data2 <- descr(data2[,c(2:25, 30:51, 53)])
knitr::kable(descrip_data2)

##### 2.4 Análisis de los instrumentos de medida#####
##### 2.4.1 Redistribucion ####
##### 2.4.1.1 Descriptivos redistribución#####

redis_data2 <- data2[,c("redis1", "redis2", "redis3", "redis4", "redis5",
                        "redis6", "redis7", "redis8", "redis9")]

get_labels(redis_data2, include.non.labelled = TRUE) #se puede borrar

redis_data2 <- set_labels(redis_data2, labels = c("Totalmente en Desacuerdo",
                                                  "Mayoritariamente en Desacuerdo",
                                                  "Ligeramente en Desacuerdo",
                                                  "Neutro",
                                                  "Ligeramente de Acuerdo",
                                                  "Mayormente de Acuerdo",
                                                  "Totalmene de Acuerdo"))

redis_data2 <- to_factor(redis_data2) # para convertir un objeto labelled en vectores de tipo factor

install.packages("likert")
library(likert)
redis2_likert <- likert(as.data.frame(redis_data2)) #tiene que ser un objeto con vectores de tipo factor

plot(redis2_likert,
     type = "bar",
     group.order =  names(redis_data2)) # in the order of the names of the data set

##### 2.4.1.2 Fiabilidad Redistribución ####

var_label(data2$redis7) # le asigno la etiqueta que se perdió en algún momeneto de la transformación de los dato
var_label(data2$redis7) <- 'No hay ninguna necesidad de cambiar la distribución de ingresos económicos en Colombia.'
var_label(data2$redis7r) <- 'No hay ninguna necesidad de cambiar la distribución de ingresos económicos en Colombia. (reco)'

sjt.itemanalysis(data2[,c("redis1", "redis2", "redis3", "redis4", "redis5",
                          "redis6", "redis7r", "redis8", "redis9")],
                 encoding = "Windows-1252") ### clave para presentar los descriptivos por escala

##### 2.4.1.3 Dimensionalidad redistribución: Análisis factorial confirmatorio ####

redis.model1 <- 'redist  =~ redis6 + redis1 +  redis7r + redis4
                ayudar  =~ redis3 + redis9 + redis5 + redis8
'
redis.fit1 <- cfa(redis.model1, data = data2)

summary(redis.fit1, standardized = TRUE, fit.measures = TRUE, rsq = TRUE, modindices=TRUE)
fitmeasures(redis.fit1)
# x^2^~(gl=19)~ = 242.52, p<.001, RMSEA = .080, CFI = .900; TLI = .853; SRMR = .050

redis.model2 <- 'redist  =~ redis6 + redis1 +  redis7r + redis4
                ayudar  =~ redis3 + redis9 + redis5 + redis8
                redis3 ~~ redis8
                '
redis.fit2 <- cfa(redis.model2, data = data2)

summary(redis.fit2, standardized = TRUE, fit.measures = TRUE, rsq = TRUE, modindices=TRUE)
fitmeasures(redis.fit2)
# x^2^~(gl=18)~ = 164.23, p<.001, RMSEA = .067, CFI = .935; TLI = .899; SRMR = .041

redis.model3 <- 'redist  =~ redis6 + redis1 +  redis7r + redis4
                ayudar  =~ redis3 + redis9 + redis5
                '
redis.fit3 <- cfa(redis.model3, data = data2)

summary(redis.fit3, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
fitmeasures(redis.fit3)
# x^2^~(gl=13)~ = 53.24, p<.001, RMSEA = .041, CFI = .979; TLI = .967; SRMR = .027

anova(redis.fit1, redis.fit2)
anova(redis.fit2, redis.fit3)

pacman::p_load(semPlot)
semPaths(redis.fit, whatLabels = "par", layout= "tree")




##### 2.4.1.4 fiabilidad dimensión: redistribución recursos####

sjt.itemanalysis(data2[,c("redis6","redis1","redis7r","redis4")],
                 encoding = "Windows-1252")

##### 2.4.1.5 fiabilidad dimensión: Ayudar a los necesitados####

sjt.itemanalysis(data2[,c("redis3","redis9","redis8","redis5")],
                 encoding = "Windows-1252")

##### 2.4.2 Percepcion de la desigualdad (fraing) #####
##### 2.4.2.1 Descriptivos percepción de la desigualdad #####

fram_data2 <- data2[,c("fram1", "fram2","fram3", "fram4", "fram5","fram6",
                       "fram7","fram8","fram9","fram10","fram11")]

get_labels(fram_data2, include.non.labelled = TRUE) #se puede borrar
fram_data2 <- set_labels(fram_data2, labels = c("1", "2","3","4","5","6","7","8","9","10"))
fram_data2 <- to_factor(fram_data2) # para convertir un objeto labelled en vectores de tipo factor

library(likert)
fram2_likert <- likert(as.data.frame(fram_data2)) #tiene que ser un objeto con vectores de tipo factor

plot(fram2_likert,
     type = "bar",
     group.order =  names(fram_data2)) # in the order of the names of the data set


knitr::kable(descr(fram_data2))

##### 2.4.2.2 Dimensionalidad percepción de la desigualdad ####

parallel <- fa.parallel((data2[,c("fram1", "fram2","fram3", "fram4", "fram5",
                                   "fram6","fram7","fram8","fram9","fram10","fram11")]),
                        fm = 'minres', fa = 'fa')
fram2_twofactor <- fa((data2[,c("fram1", "fram2","fram3", "fram4", "fram5",
                                 "fram6","fram7","fram8","fram9","fram10","fram11")]),
                      nfactors = 2,rotate = "varimax",fm="minres")
print.psych(fram2_twofactor, cut = 0.3, sort = TRUE)
fa.diagram(fram2_twofactor)


##### 2.4.2.3 Fiabilidad percepción de la desigualdad ####

sjt.itemanalysis(data2[,c("fram1", "fram2","fram3", "fram4", "fram5",
                          "fram6","fram7","fram8","fram9","fram10","fram11")],
                 encoding = "Windows-1252") ### clave para presentar los descriptivos por escala

sjt.itemanalysis(data2[,c("fram2","fram5","fram6","fram7","fram8","fram9")],
                 encoding = "Windows-1252") ### clave para presentar los descriptivos por escala

sjt.itemanalysis(data2[,c("fram1", "fram3", "fram4","fram10","fram11")],
                 encoding = "Windows-1252") ### clave para presentar los descriptivos por escala

##### 2.4.3 Meritocracia ####
##### 2.4.3.1 Descriptivos meritocracia ####

merit_data2 <- data2[,c("merit1", "merit2", "merit3", "merit4", "merit5", "merit6")]


get_labels(merit_data2, include.non.labelled = TRUE) #se puede borrar
merit_data2 <- to_factor(merit_data2) # para convertir un objeto labelled en vectores de tipo factor

library(likert)
merit2_likert <- likert(as.data.frame(merit_data2)) #tiene que ser un objeto con vectores de tipo factor

plot(merit2_likert,
     type = "bar",
     group.order =  names(merit_data2)) # in the order of the names of the data set

knitr::kable(descr(merit_data2))

#### 2.4.3.2 Fiabilidad meritocracia ####

sjt.itemanalysis(data2[,c("merit1", "merit2", "merit3", "merit4", "merit5", "merit6")],
                 encoding = "Windows-1252")

##### 2.4.3.3 Dimensionalidad meritocraia ####

#CFA meritocracia
merit.cfa1 <- 'meritA  =~ merit1+ merit2 + merit3+ merit4+ merit5+ merit6
              '
merit_one_factor <- cfa(merit.cfa1, data = data2)
summary(merit_one_factor, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
# CFI=.915; TLI=.858;RMSEA=.129;SRMR=.050

# EFA meritocracy
parallel <- fa.parallel((data2[,merit]), fm = 'minres', fa = 'fa')
merit_fa <- fa((data2[,merit]),nfactors = 2,rotate = "varimax",fm="minres")
print.psych(merit_fa, cut = 0.3, sort = TRUE)
fa.diagram(merit_fa)

# CFA meritocracia dos factores
merit.cfa2 <- 'meritA  =~  merit3+ merit4+ merit5  + merit6
              meritB   =~  merit1+ merit2
'
merit_two_factor <- cfa(merit.cfa2, data = data2)
summary(merit_two_factor, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
# CFI=.990; TLI=.982;RMSEA=.047;SRMR=.018


### Alphas

psych::alpha(data2[,merit]) # meritocracy .81
psych::alpha(data2[,c("merit3", "merit4", "merit5", "merit6")]) #alpha=.75
psych::alpha(data2[,jse])   # economic system justification .70

##### 2.4.4 Justificación del sistema económico #####
##### 2.4.4.1 Descriptivos Justificación del sistema económico ####

jse_data2 <- data2[,c("jse1","jse2","jse3","jse4","jse5","jse6","jse7")]


get_labels(jse_data2, include.non.labelled = TRUE) #se puede borrar
jse_data2 <- to_factor(jse_data2) # para convertir un objeto labelled en vectores de tipo factor

library(likert)
jse2_likert <- likert(as.data.frame(jse_data2)) #tiene que ser un objeto con vectores de tipo factor

plot(jse2_likert,
     type = "bar",
     group.order =  names(jse_data2)) # in the order of the names of the data set

knitr::kable(descr(jse_data2))

##### 2.4.4.2 Fiabildad justificación del sistema económico #####

sjt.itemanalysis(data2[,c("jse1","jse2","jse3","jse4","jse5","jse6","jse7")],
                 encoding = "Windows-1252")

##### 2.4.4.3 Dimensionalidad de la justificación del sistema económico#####

jse_cfa <- 'jse  =~  jse1+  jse2+  jse3+  jse4+  jse5+  jse6+  jse7'
jse_one_factor <- cfa(jse_cfa, data = data2)
summary(jse_one_factor, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
# CFI=.912; TLI=.868;RMSEA=.082;SRMR=.045



##### 2.5 Cómputo de variables de variables Estudio 2  #####

data2 <- data2 %>%
  mutate(pergap = (desperal2/desperba2), # 3.1 Brecha salarial percibida
         lnpergap = log(desperal2/desperba2),
         idgap = (desidal2/desidba2), # 3.2 Brecha salarial ideal
         lnidgap = log(desidal2/desidba2),
         framing = (fram1+fram2+fram3+fram4+fram5+fram6+fram7+fram8+fram9+fram10+fram11)/11, # framing
         menostien = (fram2+fram5+fram6+fram7+fram8+fram9)/6, # framing los que menos tienen
         mastien = (fram1+fram3+fram4+fram10+fram11)/5, # framing los que más tienen
         pergral = (desgral+desgral)/2, # percepción gral de la desigualdad (evaluación)
         merit = (merit1+ merit2+ merit3+ merit4+ merit5+ merit6)/6, #meritocracia
         merit_duro = (merit1+merit2)/2, # creenia en la meritoracia
         merit_perc = (merit3+ merit4+ merit5+ merit6)/4,
         jse = (jse1+jse2+jse3+jse4+jse5+jse6+jse7)/7, # justficación del sistema económico
         redis = (redis1+ redis2+ redis3+ redis4+ redis5+ redis6+ redis7r+ redis8+ redis9)/9, # redistribución
         redis_reg = (redis1+ redis4+ redis6+ redis7r)/4, # redistribución centrado en el gobierno
         redis_prov = (redis3+ redis5+ redis9)/3, # eliminé ítems 2 y 8, según las características del FA
         )

knitr::kable(descr(data2[,c("lnpergap","lnidgap","framing","menostien","mastien",
               "merit","merit_duro","merit_perc","jse","redis","redis_reg","redis_prov")]))

sjt.corr(data2[,c("lnpergap","lnidgap","desfrec","pergral","framing", "menostien","mastien",
                  "merit","merit_duro","merit_perc","jse", "ideopol",
                  "redis","redis_reg","redis_prov",  "redis1",
                  "ingresos", "statsub", "educa",
                  "sexo", "edad2")],
         var.labels = c("lnpergap","lnidgap","desfrec","pergral","framing", "menostien","mastien",
                        "merit","merit_duro","merit_perc","jse", "ideopol",
                        "redis","redis_reg","redis_prov",  "redis1",
                        "ingresos", "statsub", "educa",
                        "sexo", "edad2"))

#Correlaciones para el artículo
sjt.corr(data2[,c("redis_reg","redis_prov",
                  "merit","jse", "ideopol",
                  "lnpergap","lnidgap","pergral","desfrec",
                  "ingresos", "statsub", "educa")],
         var.labels = c("Redistribution (regulation)","Redistribution (social spending)",
                        "Meritocracy","Economic system justification", "Political ideology (left-right)",
                        "Perceived income gap","Ideal income gap","General perception of inequality","Frequency perception of inequality",
                        "Income", "Subjective status", "Educational level"))

# creating Data 3 for regression analysis

data2$departamento[data2$departamento=="españa"] <- NA ### discarding a case from Spain

data2$depto2 <- as.factor(data2$departamento)
data2$depto2[data2$depto2==""] <-  NA

data2$depto2 <- droplevels(data2$depto2)

ggplot(data=na.omit(data2), aes(depto2,jse), na.rm=TRUE) +
  geom_violin(scale="count", aes(fill = factor(depto2)))

names(data2)

# centering predictors
data2$lnpergap_cen <- as.numeric(scale(data2$lnpergap, center = TRUE, scale = FALSE))
data2$lnidegap_cen <- as.numeric(scale(data2$lnidgap, center = TRUE, scale = FALSE))
data2$framing_cen <- as.numeric(scale(data2$framing, center = TRUE, scale = FALSE))
data2$menostien_cen <- as.numeric(scale(data2$menostien, center = TRUE, scale = FALSE))
data2$mastien_cen <- as.numeric(scale(data2$mastien, center = TRUE, scale = FALSE))
data2$pergral_cen <- as.numeric(scale(data2$pergral, center = TRUE, scale = FALSE))
data2$merit_cen <- as.numeric(scale(data2$merit, center = TRUE, scale = FALSE))
data2$merit_duro_cen <- as.numeric(scale(data2$merit_duro, center = TRUE, scale = FALSE))
data2$merit_perc_cen <- as.numeric(scale(data2$merit_perc, center = TRUE, scale = FALSE))
data2$jse_cen <- as.numeric(scale(data2$jse, center = TRUE, scale = FALSE))




fit1 <- lm(redis_reg ~ lnpergap_cen + pergral_cen + desfrec + framing_cen + jse_cen +
                        sexo + edad2 + statsub + ingresos + educa,
          data= data2)

fit2 <- lm(redis_reg ~ lnpergap_cen + pergral_cen + desfrec + framing_cen + #percepciones desigualdad
            jse_cen + # ideología que justifica el sistema
            framing_cen*jse_cen + lnpergap_cen*jse_cen+ # interacciones
            sexo + edad2 + statsub + ingresos + educa,# control por ses y sociodemograficas
            #control efectos fijos región
           data= data2)

fit3 <- lm(redis_prov ~ lnpergap_cen + pergral_cen + desfrec + framing_cen + #percepciones desigualdad
             jse_cen + # ideología que justifica el sistema
             framing_cen*jse_cen + lnpergap_cen*jse_cen+ # interacciones
             sexo + edad2 + statsub + ingresos + educa, # control por ses y sociodemograficas
              #control efectos fijos región
           data= data2)


sjt.lm(fit2, fit3,
       emph.p = TRUE, p.zero = FALSE,show.se = TRUE,
       string.dv = "Support for redistribution",
       digits.est = 3,digits.std = 3)

fit2 <- lm(redis_prov ~ lnpergap_cen + pergral_cen + desfrec + framing_cen +
             framing_cen*merit+ framing_cen*jse+
             ideopol + sexo + edad2 + statsub + ingresos + educa,
           data= data2)
summary(fit2)


plot_model(fit, type= "pred", terms = c("framing_cen", "jse_cen"))
plot_model(fit2, type = "int",
           title = "Interaction between Perceived inequality and Economic system justification on Support for redistribution",
           axis.title = "Support for redistribution")
get_model_data(fit2, type = "int")
library(reghelper)
graph_model_q(fit2, y='redis_reg', x='jse_cen', lines='framing_cen')
graph_model_q(fit, y='redis_reg', x='lnpergap_cen', lines='jse_cen')

#to create dummy code from continuous variable framing of inequality
data2$framing_dum <- ifelse(data2$framing <= 5, 0,
                            ifelse(data2$framing >= 5, 1, NA)
                            )

data2$depto_bogota_ref <- ifelse(data2$depto2 == "bogotá", 0, 0)
data2$depto_centro <- ifelse(data2$depto2 == "boyacá, cundinamarca y casanare", 1, 0)
data2$depto_caribe <- ifelse(data2$depto2 == "caribe", 1, 0)
data2$depto_nariño <- ifelse(data2$depto2 == "nariño", 1, 0)
data2$depto_valle <- ifelse(data2$depto2 == "valle del cauca", 1, 0)


library(dplyr)
library(basedosdados)
library(stargazer)

#TSE
#geral
basedosdados::set_billing_id("auxilio-brasil-eleicoes")
datatse <- read_sql("SELECT * FROM `basedosdados.br_tse_eleicoes.resultados_partido_municipio`")
datatse2 <- read_sql("SELECT * FROM `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio`")
datatse3 <- read_sql("SELECT * FROM `basedosdados.br_tse_eleicoes.perfil_eleitorado_municipio_zona`")
#Votos
datatse <- datatse %>%
  filter(ano == 2010 | ano == 2014 | ano == 2018 | ano == 2022,
         cargo == "presidente")
datatse <- datatse[!is.na(datatse$id_municipio),]
votos_validos <- datatse %>%
  group_by(id_municipio, ano, turno) %>%
  summarise(votos_validos = sum(votos_nominais))
datatse <- datatse[,c(1:2, 4:6, 8:10)]
datatse3avia <- datatse %>%
  filter(!numero_partido %in% c(13, 17, 22, 45) | (numero_partido == 45 & ano == 2018))
votos_validos3avia <- datatse3avia %>%
  group_by(id_municipio, ano, turno) %>%
  summarise(votos_3a_via = sum(votos_nominais))
datatse_direita <- datatse %>%
  filter(numero_partido %in% c(17, 22, 45)) %>%
  filter(!(numero_partido == 45 & ano %in% c(2018, 2022))) %>%
  arrange(id_municipio, ano, turno)
datatse <- datatse %>%
  filter(numero_partido %in% c(13)) %>%
  arrange(id_municipio, ano, turno)
datatse <- left_join(datatse, datatse_direita,  by = c("id_municipio", "ano", "sigla_uf", "id_municipio_tse", "turno"))
colnames(datatse)[6] <- "numero_partido_esquerda"
colnames(datatse)[7] <- "sigla_partido_esquerda"
colnames(datatse)[8] <- "votos_esquerda"
colnames(datatse)[9] <- "numero_partido_direita"
colnames(datatse)[10] <- "sigla_partido_direita"
colnames(datatse)[11] <- "votos_direita"
datatse <- left_join(datatse, votos_validos3avia, by = c("id_municipio", "ano", "turno"))
datatse <- left_join(datatse, votos_validos, by = c("id_municipio", "ano", "turno"))
datatse$vote_share_esquerda <- datatse$votos_esquerda/datatse$votos_validos
datatse$vote_share_direita <- datatse$votos_direita/datatse$votos_validos
datatse$vote_share_3avia <- datatse$votos_3a_via/datatse$votos_validos
datatse <- datatse[,c(1:4, 6:8, 16, 9:11, 14, 12, 15, 13)]
#Eleitores aptos
datatse2 <- datatse2 %>%
  filter(ano == 2010 | ano == 2014 | ano == 2018 | ano == 2022,
         cargo == "presidente")
datatse2 <- datatse2[!is.na(datatse2$id_municipio),]
datatse2 <- datatse2[,c(1:2, 4:6, 8, 11, 13:17, 19:22)]
#Características
datatse3 <- datatse3 %>%
  filter(ano == 2010 | ano == 2014 | ano == 2018 | ano == 2022)
  #genero
datatse4 <- datatse3 %>%
  group_by(id_municipio, ano, genero) %>%
  summarise(mulheres = sum(eleitores))
proporcao_mulheres <- datatse4 %>%
  group_by(id_municipio, ano) %>%
  summarise(total = sum(mulheres))
datatse4 <- left_join(datatse4, proporcao_mulheres, by = c("id_municipio", "ano"))
rm(proporcao_mulheres)
datatse4$proporcao_mulheres <- sprintf("%.6f", datatse4$mulheres / datatse4$total)
datatse4 <- datatse4 %>%
  filter(genero == "feminino")
datatse4 <- datatse4[, c(1:2, 5, 4, 6)]
datatse4
  #estado cívil
proporcao_estado_civil <- datatse3 %>%
  group_by(id_municipio, ano, estado_civil) %>%
  summarise(quantidade_estado_civil = sum(eleitores))
total_estado_civil <- proporcao_estado_civil %>%
  group_by(id_municipio, ano) %>%
  summarise(total = sum(quantidade_estado_civil))
proporcao_estado_civil <- left_join(proporcao_estado_civil, total_estado_civil, by = c("id_municipio", "ano"))
proporcao_estado_civil$proporcao_estado_civil <- sprintf("%.6f", proporcao_estado_civil$quantidade_estado_civil/proporcao_estado_civil$total)
    #casados
proporcao_casados <- proporcao_estado_civil %>%
  filter(estado_civil == "casado(a)")
colnames(proporcao_casados)[6] <- "proporcao_casados"
proporcao_casados <- proporcao_casados[,c(1:2, 4:6)]
colnames(proporcao_casados)[3] <- "quantidade_casados"
    #divorciados
proporcao_divorciados <- proporcao_estado_civil %>%
  filter(estado_civil == "divorciado(a)")
colnames(proporcao_divorciados)[6] <- "proporcao_divorciados"
proporcao_divorciados <- proporcao_divorciados[,c(1:2, 4:6)]
colnames(proporcao_divorciados)[3] <- "quantidade_divorciados"
#viuvos
proporcao_viuvos <- proporcao_estado_civil %>%
  filter(estado_civil == "viuvo(a)")
colnames(proporcao_viuvos)[6] <- "proporcao_viuvos"
proporcao_viuvos <- proporcao_viuvos[,c(1:2, 4:6)]
colnames(proporcao_viuvos)[3] <- "quantidade_viuvos"

proporcao_estado_civil <- left_join(proporcao_casados, proporcao_divorciados, by = c("id_municipio", "ano", "total"))
proporcao_estado_civil <- left_join(proporcao_estado_civil, proporcao_viuvos, by = c("id_municipio", "ano", "total"))
rm(proporcao_casados)
rm(proporcao_divorciados)
rm(proporcao_viuvos)
datatse4 <- left_join(datatse4, proporcao_estado_civil, by = c("id_municipio", "ano", "total"))
rm(proporcao_estado_civil)
rm(total_estado_civil)
datatse4 <- datatse4[!is.na(datatse4$id_municipio),]
datatse4$quantidade_casados[is.na(datatse4$quantidade_casados)] <- 0
datatse4$proporcao_casados[is.na(datatse4$proporcao_casados)] <- 0
datatse4$quantidade_divorciados[is.na(datatse4$quantidade_divorciados)] <- 0
datatse4$proporcao_divorciados[is.na(datatse4$proporcao_divorciados)] <- 0
datatse4$quantidade_viuvos[is.na(datatse4$quantidade_viuvos)] <- 0
datatse4$proporcao_viuvos[is.na(datatse4$proporcao_viuvos)] <- 0
#idade
proporcao_idade <- datatse3 %>%
  group_by(id_municipio, ano, grupo_idade) %>%
  summarise(quantidade_idade = sum(eleitores))
total_municipio_ano <- proporcao_idade %>%
  group_by(id_municipio, ano) %>%
  summarise(total = sum(quantidade_idade))
proporcao_idade_idosos <- proporcao_idade %>%
  filter(grupo_idade == "80 a 84 anos" | grupo_idade == "85 a 89 anos" | grupo_idade == "90 a 94 anos" | grupo_idade =="95 a 99 anos"| grupo_idade == "100 anos ou mais")
proporcao_idade_idosos <- proporcao_idade_idosos %>%
  group_by(id_municipio, ano) %>%
  summarise("quantidade_idade" = sum(quantidade_idade))
proporcao_idade_idosos$grupo_idade <- "superior a 79 anos"
proporcao_idade_idosos <- proporcao_idade_idosos[,c(1:2, 4, 3)]
proporcao_idade_jovens <- proporcao_idade %>%
  filter(grupo_idade == "18 anos" | grupo_idade == "19 anos" | grupo_idade == "20 anos")
proporcao_idade_jovens <- proporcao_idade_jovens %>%
  group_by(id_municipio, ano) %>%
  summarise("quantidade_idade" = sum(quantidade_idade))
proporcao_idade_jovens$grupo_idade <- "18 a 20 anos"
proporcao_idade_jovens <- proporcao_idade_jovens[,c(1:2, 4, 3)]
proporcao_idade_25_34 <- proporcao_idade %>%
  filter(grupo_idade == "25 a 29 anos" | grupo_idade == "30 a 34 anos")
proporcao_idade_35_44 <- proporcao_idade %>%
  filter(grupo_idade == "35 a 39 anos" | grupo_idade == "40 a 44 anos")
proporcao_idade_jovens_45_59 <- proporcao_idade %>%
  filter(grupo_idade == "45 a 49 anos" | grupo_idade == "50 a 54 anos" | grupo_idade == "55 a 59 anos")
proporcao_idade_jovens_60_69 <- proporcao_idade %>%
  filter(grupo_idade == "60 a 64 anos" | grupo_idade == "65 a 69 anos")
proporcao_idade_jovens_70_79 <- proporcao_idade %>%
  filter(grupo_idade == "70 a 74 anos" | grupo_idade == "75 a 79 anos")
proporcao_idade_25_34 <- proporcao_idade_25_34 %>%
  group_by(id_municipio, ano) %>%
  summarise("quantidade_idade" = sum(quantidade_idade))
proporcao_idade_35_44 <- proporcao_idade_35_44 %>%
  group_by(id_municipio, ano) %>%
  summarise("quantidade_idade" = sum(quantidade_idade))
proporcao_idade_jovens_45_59 <- proporcao_idade_jovens_45_59 %>%
  group_by(id_municipio, ano) %>%
  summarise("quantidade_idade" = sum(quantidade_idade))
proporcao_idade_jovens_60_69 <- proporcao_idade_jovens_60_69 %>%
  group_by(id_municipio, ano) %>%
  summarise("quantidade_idade" = sum(quantidade_idade))
proporcao_idade_jovens_70_79 <- proporcao_idade_jovens_70_79 %>%
  group_by(id_municipio, ano) %>%
  summarise("quantidade_idade" = sum(quantidade_idade))
proporcao_idade_25_34$grupo_idade <- "25 a 34 anos"
proporcao_idade_35_44$grupo_idade <- "35 a 44 anos"
proporcao_idade_jovens_45_59$grupo_idade <- "45 a 59 anos"
proporcao_idade_jovens_60_69$grupo_idade <- "60 a 69 anos"
proporcao_idade_jovens_70_79$grupo_idade <- "70 a 79 anos"
proporcao_idade_25_34 <- proporcao_idade_25_34[,c(1:2, 4, 3)]
proporcao_idade_35_44 <- proporcao_idade_35_44[,c(1:2, 4, 3)]
proporcao_idade_jovens_45_59 <- proporcao_idade_jovens_45_59[,c(1:2, 4, 3)]
proporcao_idade_jovens_60_69 <- proporcao_idade_jovens_60_69[,c(1:2, 4, 3)]
proporcao_idade_jovens_70_79 <- proporcao_idade_jovens_70_79[,c(1:2, 4, 3)]
proporcao_idade <- proporcao_idade %>%
  filter(grupo_idade %in% c("16 anos","17 anos","18 a 20 anos","21 a 24 anos","superior a 79 anos", "invalido", "25 a 34 anos", "35 a 44 anos", "45 a 59 anos", "60 a 69 anos", "70 a 79 anos"))
proporcao_idade <- bind_rows(proporcao_idade, proporcao_idade_idosos)
proporcao_idade <- bind_rows(proporcao_idade, proporcao_idade_jovens)
proporcao_idade <- bind_rows(proporcao_idade, proporcao_idade_25_34)
proporcao_idade <- bind_rows(proporcao_idade, proporcao_idade_35_44)
proporcao_idade <- bind_rows(proporcao_idade, proporcao_idade_jovens_45_59)
proporcao_idade <- bind_rows(proporcao_idade, proporcao_idade_jovens_60_69)
proporcao_idade <- bind_rows(proporcao_idade, proporcao_idade_jovens_70_79)
rm(proporcao_idade_25_34)
rm(proporcao_idade_35_44)
rm(proporcao_idade_jovens_45_59)
rm(proporcao_idade_jovens_60_69)
rm(proporcao_idade_jovens_70_79)
rm(proporcao_idade_jovens)
rm(proporcao_idade_idosos)
proporcao_idade <- proporcao_idade %>%
  arrange(id_municipio, ano, grupo_idade)
proporcao_idade <- left_join(proporcao_idade, total_municipio_ano, by = c("id_municipio", "ano"))
rm(total_municipio_ano)
proporcao_idade$proporcao_idade <- sprintf("%.6f", proporcao_idade$quantidade_idade/proporcao_idade$total)
proporcao_idade <- proporcao_idade[,c(1:4, 6, 5)]
#16
proporcao_16 <- proporcao_idade %>%
  filter(grupo_idade == "16 anos")
colnames(proporcao_16)[4] <- "quantidade_16"
colnames(proporcao_16)[5] <- "proporcao_16"
#17
proporcao_17 <- proporcao_idade %>%
  filter(grupo_idade == "17 anos")
colnames(proporcao_17)[4] <- "quantidade_17"
colnames(proporcao_17)[5] <- "proporcao_17"
#18-20
proporcao_18_20 <- proporcao_idade %>%
  filter(grupo_idade == "18 a 20 anos")
colnames(proporcao_18_20)[4] <- "quantidade_18_20"
colnames(proporcao_18_20)[5] <- "proporcao_18_20"
#21-24
proporcao_21_24 <- proporcao_idade %>%
  filter(grupo_idade == "21 a 24 anos")
colnames(proporcao_21_24)[4] <- "quantidade_21_24"
colnames(proporcao_21_24)[5] <- "proporcao_21_24"
#25-34
proporcao_25_34 <- proporcao_idade %>%
  filter(grupo_idade == "25 a 34 anos")
colnames(proporcao_25_34)[4] <- "quantidade_25_29"
colnames(proporcao_25_34)[5] <- "proporcao_25_29"
#35-44
proporcao_35_44 <- proporcao_idade %>%
  filter(grupo_idade == "35 a 44 anos")
colnames(proporcao_35_44)[4] <- "quantidade_35_44"
colnames(proporcao_35_44)[5] <- "proporcao_35_44"
#45-59
proporcao_45_59 <- proporcao_idade %>%
  filter(grupo_idade == "45 a 59 anos")
colnames(proporcao_45_59)[4] <- "quantidade_45_59"
colnames(proporcao_45_59)[5] <- "proporcao_45_59"
#60-69
proporcao_60_69 <- proporcao_idade %>%
  filter(grupo_idade == "60 a 69 anos")
colnames(proporcao_60_69)[4] <- "quantidade_60_69"
colnames(proporcao_60_69)[5] <- "proporcao_60_69"
#70-79
proporcao_70_79 <- proporcao_idade %>%
  filter(grupo_idade == "70 a 79 anos")
colnames(proporcao_70_79)[4] <- "quantidade_70_79"
colnames(proporcao_70_79)[5] <- "proporcao_70_79"
#Suerior a 79
proporcao_superior_79 <- proporcao_idade %>%
  filter(grupo_idade == "superior a 79 anos")
colnames(proporcao_superior_79)[4] <- "quantidade_superior_79"
colnames(proporcao_superior_79)[5] <- "proporcao_superior_79"

proporcao_idade <- left_join(proporcao_17, proporcao_16, by = c("id_municipio", "ano", "total"))
proporcao_idade <- left_join(proporcao_idade, proporcao_18_20, by = c("id_municipio", "ano", "total"))
proporcao_idade <- left_join(proporcao_idade, proporcao_21_24, by = c("id_municipio", "ano", "total"))
proporcao_idade <- left_join(proporcao_idade, proporcao_25_34, by = c("id_municipio", "ano", "total"))
proporcao_idade <- left_join(proporcao_idade, proporcao_35_44, by = c("id_municipio", "ano", "total"))
proporcao_idade <- left_join(proporcao_idade, proporcao_45_59, by = c("id_municipio", "ano", "total"))
proporcao_idade <- left_join(proporcao_idade, proporcao_60_69, by = c("id_municipio", "ano", "total"))
proporcao_idade <- left_join(proporcao_idade, proporcao_70_79, by = c("id_municipio", "ano", "total"))
proporcao_idade <- left_join(proporcao_idade, proporcao_superior_79, by = c("id_municipio", "ano", "total"))
proporcao_idade <- proporcao_idade[,c(1:2, 8:9, 4:5, 11:12, 14:15, 17:18, 20:21, 23:24, 26:27, 29:30, 32:33, 6)]
datatse4 <- left_join(datatse4, proporcao_idade, by = c("id_municipio", "ano", "total"))
rm(proporcao_16)
rm(proporcao_17)
rm(proporcao_18_20)
rm(proporcao_21_24)
rm(proporcao_25_34)
rm(proporcao_35_44)
rm(proporcao_45_59)
rm(proporcao_60_69)
rm(proporcao_70_79)
rm(proporcao_superior_79)
rm(proporcao_idade)
  #instrucao
proporcao_instrucao <- datatse3 %>%
  group_by(id_municipio, ano, instrucao) %>%
  summarise(quantidade_instrucao = sum(eleitores))
total_municipio_ano <- proporcao_instrucao %>%
  group_by(id_municipio, ano) %>%
  summarise(total = sum(quantidade_instrucao))
proporcao_instrucao <- left_join(proporcao_instrucao, total_municipio_ano, by = c("id_municipio", "ano"))
proporcao_instrucao$proporcao_instrucao <- sprintf("%.6f", proporcao_instrucao$quantidade_instrucao/proporcao_instrucao$total)
    #analfabeto
proporcao_analfabeto <- proporcao_instrucao %>%
  filter(instrucao == "analfabeto")
proporcao_analfabeto <- proporcao_analfabeto[,c(1:2, 4:6)]
colnames(proporcao_analfabeto)[3] <- "quantidade_analfabeto"
colnames(proporcao_analfabeto)[5] <- "proporcao_analfabeto"
    #le e escreve
proporcao_le_e_escreve <- proporcao_instrucao %>%
  filter(instrucao == "le e escreve")
proporcao_le_e_escreve <- proporcao_le_e_escreve[,c(1:2, 4:6)]
colnames(proporcao_le_e_escreve)[3] <- "quantidade_le_e_escreve"
colnames(proporcao_le_e_escreve)[5] <- "proporcao_le_e_escreve"
    #fundamental incompleto
proporcao_fundamental_incompleto <- proporcao_instrucao %>%
  filter(instrucao == "ensino fundamental incompleto")
proporcao_fundamental_incompleto <- proporcao_fundamental_incompleto[,c(1:2, 4:6)]
colnames(proporcao_fundamental_incompleto)[3] <- "quantidade_fundamental_incompleto"
colnames(proporcao_fundamental_incompleto)[5] <- "proporcao_fundamental_incompleto"
    #fundamental completo
proporcao_fundamental_completo <- proporcao_instrucao %>%
  filter(instrucao == "ensino fundamental completo")
proporcao_fundamental_completo <- proporcao_fundamental_completo[,c(1:2, 4:6)]
colnames(proporcao_fundamental_completo)[3] <- "quantidade_fundamental_completo"
colnames(proporcao_fundamental_completo)[5] <- "proporcao_fundamental_completo"
    #medio incompleto
proporcao_medio_incompleto <- proporcao_instrucao %>%
  filter(instrucao == "ensino medio incompleto")
proporcao_medio_incompleto <- proporcao_medio_incompleto[,c(1:2, 4:6)]
colnames(proporcao_medio_incompleto)[3] <- "quantidade_medio_incompleto"
colnames(proporcao_medio_incompleto)[5] <- "proporcao_medio_incompleto"
    #medio completo
proporcao_medio_completo <- proporcao_instrucao %>%
  filter(instrucao == "ensino medio completo")
proporcao_medio_completo <- proporcao_medio_completo[,c(1:2, 4:6)]
colnames(proporcao_medio_completo)[3] <- "quantidade_medio_completo"
colnames(proporcao_medio_completo)[5] <- "proporcao_medio_completo"
    #superior incompleto
proporcao_superior_incompleto <- proporcao_instrucao %>%
  filter(instrucao == "ensino superior incompleto")
proporcao_superior_incompleto <- proporcao_superior_incompleto[,c(1:2, 4:6)]
colnames(proporcao_superior_incompleto)[3] <- "quantidade_superior_incompleto"
colnames(proporcao_superior_incompleto)[5] <- "proporcao_superior_incompleto"

proporcao_instrucao <- left_join(proporcao_analfabeto, proporcao_le_e_escreve, by = c("id_municipio", "ano", "total"))
proporcao_instrucao <- proporcao_instrucao[,c(1:2, 4, 3, 5:7)]
proporcao_instrucao <- left_join(proporcao_instrucao, proporcao_fundamental_incompleto, by = c("id_municipio", "ano", "total"))
proporcao_instrucao <- left_join(proporcao_instrucao, proporcao_fundamental_completo, by = c("id_municipio", "ano", "total"))
proporcao_instrucao <- left_join(proporcao_instrucao, proporcao_medio_incompleto, by = c("id_municipio", "ano", "total"))
proporcao_instrucao <- left_join(proporcao_instrucao, proporcao_medio_completo, by = c("id_municipio", "ano", "total"))
proporcao_instrucao <- left_join(proporcao_instrucao, proporcao_superior_incompleto, by = c("id_municipio", "ano", "total"))
datatse4 <- left_join(datatse4, proporcao_instrucao, by = c("id_municipio", "ano", "total"))
rm(proporcao_analfabeto)
rm(proporcao_le_e_escreve)
rm(proporcao_fundamental_incompleto)
rm(proporcao_fundamental_completo)
rm(proporcao_medio_incompleto)
rm(proporcao_medio_completo)
rm(proporcao_superior_incompleto)
rm(proporcao_instrucao)
  #Deficiência
proporcao_deficientes <- datatse3 %>%
  group_by(id_municipio, ano) %>%
  summarise(eleitores_deficientes = sum(eleitores_deficiencia))
proporcao_deficientes <- left_join(proporcao_deficientes, total_municipio_ano, by = c("id_municipio", "ano"))
proporcao_deficientes$proporcao_deficientes <- sprintf("%.6f", proporcao_deficientes$eleitores_deficientes/proporcao_deficientes$total)
proporcao_deficientes <- proporcao_deficientes[,c(1:2, 4, 3, 5)]
datatse4 <- left_join(datatse4, proporcao_deficientes, by = c("id_municipio", "ano", "total"))
rm(proporcao_deficientes)
rm(total_municipio_ano)

#Base final
rm(datatse3)
datatse <- left_join(datatse, datatse2, by = c("id_municipio", "ano", "turno", "sigla_uf", "votos_validos"))
datatse <- datatse %>%
  arrange(id_municipio, ano, turno)
datatse <- datatse[,c(1:15, 17:26)]
datatse <- left_join(datatse, datatse4, by = c("id_municipio", "ano"))
colnames(datatse)[43] <- "quantidade_25_34"
colnames(datatse)[44] <- "proporcao_25_34"
rm(datatse2)
rm(datatse4)
rm(total_municipio_ano)
rm(datatse)
#Portal da Transparencia
#2014
unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\201408_BolsaFamilia_Pagamentos.zip")
transp082014 <- read.csv("201408_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
transp082014$VALOR.PARCELA <- as.numeric(gsub(",", ".", transp082014$VALOR.PARCELA))
transp082014 <- transp082014 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA))
beneficiarios3 <- transp082014 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(individuals = n_distinct(CPF.FAVORECIDO))
transp082014 <- transp082014[,c(1, 4, 7:8)]
transp082014 <- transp082014[,c(2:3)]
unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\201409_BolsaFamilia_Pagamentos.zip")
transp092014 <- read.csv("201409_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
transp092014$VALOR.PARCELA <- as.numeric(gsub(",", ".", transp092014$VALOR.PARCELA))
transp092014 <- transp092014 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA))
beneficiarios4 <- transp092014 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(individuals = n_distinct(CPF.FAVORECIDO))
beneficiarios3 <- left_join(beneficiarios3, beneficiarios4, by = "CÓDIGO.MUNICÍPIO.SIAFI")
painel_monetario_3 <- full_join(transp082014, transp092014, by = "CÓDIGO.MUNICÍPIO.SIAFI")
transp092014 <- transp092014[,c(1, 4, 7:8)]
transp092014 <- transp092014[,c(2:3)]
painel3 <- full_join(transp082014, transp092014, by = "NIS.FAVORECIDO")
painel3 <- painel3 %>%
  arrange(CÓDIGO.MUNICÍPIO.SIAFI)
rm(transp082014)
rm(transp092014)
rm(beneficiarios4)
unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\201410_BolsaFamilia_Pagamentos.zip")
transp102014 <- read.csv("201410_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
transp102014$VALOR.PARCELA <- as.numeric(gsub(",", ".", transp102014$VALOR.PARCELA))
transp102014 <- transp102014 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA))
beneficiarios4 <- transp102014 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(individuals = n_distinct(CPF.FAVORECIDO))
beneficiarios3 <- left_join(beneficiarios3, beneficiarios4, by = "CÓDIGO.MUNICÍPIO.SIAFI")
painel_monetario_3 <- full_join(painel_monetario_3, transp102014, by = "CÓDIGO.MUNICÍPIO.SIAFI")
transp102014 <- transp102014[,c(4, 7)]
painel3 <- full_join(painel3, transp102014, by = "NIS.FAVORECIDO")
rm(transp102014)
rm(beneficiarios4)
unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\201411_BolsaFamilia_Pagamentos.zip")
transp112014 <- read.csv("201411_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
transp112014$VALOR.PARCELA <- as.numeric(gsub(",", ".", transp112014$VALOR.PARCELA))
transp112014 <- transp112014 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA))
beneficiarios4 <- transp112014 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(individuals = n_distinct(CPF.FAVORECIDO))
beneficiarios3 <- left_join(beneficiarios3, beneficiarios4, by = "CÓDIGO.MUNICÍPIO.SIAFI")
painel_monetario_3 <- full_join(painel_monetario_3, transp112014, by = "CÓDIGO.MUNICÍPIO.SIAFI")
transp112014 <- transp112014[,c(4, 7)]
painel3 <- full_join(painel3, transp112014, by = "NIS.FAVORECIDO")
rm(transp112014)
rm(beneficiarios4)
painel_monetario_3$ano <- 2014
#2018
unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201712_BolsaFamilia_Pagamentos.zip")
familia122017 <- read.csv("201712_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia122017$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia122017$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201801_BolsaFamilia_Pagamentos.zip")
familia012018 <- read.csv("201801_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia012018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia012018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201802_BolsaFamilia_Pagamentos.zip")
familia022018 <- read.csv("201802_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia022018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia022018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201803_BolsaFamilia_Pagamentos.zip")
familia032018 <- read.csv("201803_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia032018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia032018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201804_BolsaFamilia_Pagamentos.zip")
familia042018 <- read.csv("201804_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia042018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia042018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201805_BolsaFamilia_Pagamentos.zip")
familia052018 <- read.csv("201805_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia052018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia052018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201806_BolsaFamilia_Pagamentos.zip")
familia062018 <- read.csv("201806_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia062018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia062018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201807_BolsaFamilia_Pagamentos.zip")
familia072018 <- read.csv("201807_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia072018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia072018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201808_BolsaFamilia_Pagamentos.zip")
familia082018 <- read.csv("201808_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia082018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia082018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201809_BolsaFamilia_Pagamentos.zip")
familia092018 <- read.csv("201809_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia092018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia092018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201810_BolsaFamilia_Pagamentos.zip")
familia102018 <- read.csv("201810_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia102018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia102018$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Bolsa Família\\201811_BolsaFamilia_Pagamentos.zip")
familia112018 <- read.csv("201811_BolsaFamilia_Pagamentos.csv", sep = ";", encoding = "latin1")
familia112018$VALOR.PARCELA <- as.numeric(gsub(",", ".", familia112018$VALOR.PARCELA))

#2022
#Auxilio Emergencial
#ATENÇÃO: ESTOU CONTANDO TODOS INDIVIDUOS SEM CPF COMO 1
unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202112_AuxilioEmergencial.zip")
emergencial122021 <- read.csv("202112_AuxilioEmergencial.csv", fileEncoding = "latin1", sep = ";")
emergencial122021$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial122021$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202201_AuxilioEmergencial.zip")
emergencial012022 <- read.csv("202201_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial012022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial012022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202202_AuxilioEmergencial.zip")
emergencial022022 <- read.csv("202202_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial022022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial022022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202203_AuxilioEmergencial.zip")
emergencial032022 <- read.csv("202203_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial032022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial032022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202204_AuxilioEmergencial.zip")
emergencial042022 <- read.csv("202204_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial042022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial042022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202205_AuxilioEmergencial.zip")
emergencial052022 <- read.csv("202205_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial052022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial052022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202206_AuxilioEmergencial.zip")
emergencial062022 <- read.csv("202206_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial062022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial062022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202207_AuxilioEmergencial.zip")
emergencial072022 <- read.csv("202207_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial072022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial072022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202208_AuxilioEmergencial.zip")
emergencial082022 <- read.csv("202208_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial082022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial082022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202209_AuxilioEmergencial.zip")
emergencial092022 <- read.csv("202209_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial092022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial092022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202210_AuxilioEmergencial.zip")
emergencial102022 <- read.csv("202210_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial102022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial102022$VALOR.BENEFÍCIO))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio Emergencial\\202211_AuxilioEmergencial.zip")
emergencial112022 <- read.csv("202211_AuxilioEmergencial.csv", sep = ";", fileEncoding = "latin1")
emergencial112022$VALOR.BENEFÍCIO <- as.numeric(gsub(",", ".", emergencial112022$VALOR.BENEFÍCIO))

#Auxilio Brasil

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202112_Auxiliobrasil.zip")
brasil122021 <- read.csv("202112_Auxiliobrasil.csv", fileEncoding = "latin1", sep = ";")
brasil122021$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil122021$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202201_Auxiliobrasil.zip")
brasil012022 <- read.csv("202201_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil012022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil012022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202202_Auxiliobrasil.zip")
brasil022022 <- read.csv("202202_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil022022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil022022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202203_Auxiliobrasil.zip")
brasil032022 <- read.csv("202203_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil032022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil032022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202204_Auxiliobrasil.zip")
brasil042022 <- read.csv("202204_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil042022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil042022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202205_Auxiliobrasil.zip")
brasil052022 <- read.csv("202205_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil052022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil052022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202206_Auxiliobrasil.zip")
brasil062022 <- read.csv("202206_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil062022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil062022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202207_Auxiliobrasil.zip")
brasil072022 <- read.csv("202207_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil072022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil072022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202208_Auxiliobrasil.zip")
brasil082022 <- read.csv("202208_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil082022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil082022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202209_Auxiliobrasil.zip")
brasil092022 <- read.csv("202209_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil092022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil092022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202210_Auxiliobrasil.zip")
brasil102022 <- read.csv("202210_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil102022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil102022$VALOR.PARCELA))

unzip(zipfile = "C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Portal da Transparência\\Auxílio brasil\\202211_Auxiliobrasil.zip")
brasil112022 <- read.csv("202211_Auxiliobrasil.csv", sep = ";", fileEncoding = "latin1")
brasil112022$VALOR.PARCELA <- as.numeric(gsub(",", ".", brasil112022$VALOR.PARCELA))

#Checagem de NIS e CPF NA
#Bolsa Família de 2018 tem todos os membros da base com NIS
#Auxílio Brasil de 2022 tem apenas 1 pessoa da base sem NIS
#Auxílio Emergencial de 2022 tem 3 indivíduos da base sem CPF (Vários estão sem NIS e, por isso usei o CPF. Além disso
#um indivíduo em maio parece ter recebido 2 parcelas e por isso, mesmo sem identificação considerei que era apenas 1
#indivíduo)
familia122017$NumDigits <- nchar(as.character(familia122017$NIS.FAVORECIDO))
familia012018$NumDigits <- nchar(as.character(familia012018$NIS.FAVORECIDO))
familia022018$NumDigits <- nchar(as.character(familia022018$NIS.FAVORECIDO))
familia032018$NumDigits <- nchar(as.character(familia032018$NIS.FAVORECIDO))
familia042018$NumDigits <- nchar(as.character(familia042018$NIS.FAVORECIDO))
familia052018$NumDigits <- nchar(as.character(familia052018$NIS.FAVORECIDO))
familia062018$NumDigits <- nchar(as.character(familia062018$NIS.FAVORECIDO))
familia072018$NumDigits <- nchar(as.character(familia072018$NIS.FAVORECIDO))
familia082018$NumDigits <- nchar(as.character(familia082018$NIS.FAVORECIDO))
familia092018$NumDigits <- nchar(as.character(familia092018$NIS.FAVORECIDO))
familia102018$NumDigits <- nchar(as.character(familia102018$NIS.FAVORECIDO))
familia112018$NumDigits <- nchar(as.character(familia112018$NIS.FAVORECIDO))

emergencial122021$NumDigits <- nchar(as.character(emergencial122021$CPF.BENEFICIÁRIO))
emergencial012022$NumDigits <- nchar(as.character(emergencial012022$CPF.BENEFICIÁRIO))
emergencial022022$NumDigits <- nchar(as.character(emergencial022022$CPF.BENEFICIÁRIO))
emergencial032022$NumDigits <- nchar(as.character(emergencial032022$CPF.BENEFICIÁRIO))
emergencial042022$NumDigits <- nchar(as.character(emergencial042022$CPF.BENEFICIÁRIO))
emergencial052022$NumDigits <- nchar(as.character(emergencial052022$CPF.BENEFICIÁRIO))
emergencial062022$NumDigits <- nchar(as.character(emergencial062022$CPF.BENEFICIÁRIO))
emergencial072022$NumDigits <- nchar(as.character(emergencial072022$CPF.BENEFICIÁRIO))
emergencial082022$NumDigits <- nchar(as.character(emergencial082022$CPF.BENEFICIÁRIO))
emergencial092022$NumDigits <- nchar(as.character(emergencial092022$CPF.BENEFICIÁRIO))
emergencial102022$NumDigits <- nchar(as.character(emergencial102022$CPF.BENEFICIÁRIO))
emergencial112022$NumDigits <- nchar(as.character(emergencial112022$CPF.BENEFICIÁRIO))

brasil122021$NumDigits <- nchar(as.character(brasil122021$NIS.FAVORECIDO))
brasil012022$NumDigits <- nchar(as.character(brasil012022$NIS.FAVORECIDO))
brasil022022$NumDigits <- nchar(as.character(brasil022022$NIS.FAVORECIDO))
brasil032022$NumDigits <- nchar(as.character(brasil032022$NIS.FAVORECIDO))
brasil042022$NumDigits <- nchar(as.character(brasil042022$NIS.FAVORECIDO))
brasil052022$NumDigits <- nchar(as.character(brasil052022$NIS.FAVORECIDO))
brasil062022$NumDigits <- nchar(as.character(brasil062022$NIS.FAVORECIDO))
brasil072022$NumDigits <- nchar(as.character(brasil072022$NIS.FAVORECIDO))
brasil082022$NumDigits <- nchar(as.character(brasil082022$NIS.FAVORECIDO))
brasil092022$NumDigits <- nchar(as.character(brasil092022$NIS.FAVORECIDO))
brasil102022$NumDigits <- nchar(as.character(brasil102022$NIS.FAVORECIDO))
brasil112022$NumDigits <- nchar(as.character(brasil112022$NIS.FAVORECIDO))

familia_not_11_12 <- familia122017[familia122017$NumDigits != 11, ]
familia_not_11_01 <- familia012018[familia012018$NumDigits != 11, ]
familia_not_11_02 <- familia022018[familia022018$NumDigits != 11, ]
familia_not_11_03 <- familia032018[familia032018$NumDigits != 11, ]
familia_not_11_04 <- familia042018[familia042018$NumDigits != 11, ]
familia_not_11_05 <- familia052018[familia052018$NumDigits != 11, ]
familia_not_11_06 <- familia062018[familia062018$NumDigits != 11, ]
familia_not_11_07 <- familia072018[familia072018$NumDigits != 11, ]
familia_not_11_08 <- familia082018[familia082018$NumDigits != 11, ]
familia_not_11_09 <- familia092018[familia092018$NumDigits != 11, ]
familia_not_11_10 <- familia102018[familia102018$NumDigits != 11, ]
familia_not_11_11 <- familia112018[familia112018$NumDigits != 11, ]
emergencial_not_14_12 <- emergencial122021[emergencial122021$NumDigits != 14, ]
emergencial_not_14_01 <- emergencial012022[emergencial012022$NumDigits != 14, ]
emergencial_not_14_02 <- emergencial022022[emergencial022022$NumDigits != 14, ]
emergencial_not_14_03 <- emergencial032022[emergencial032022$NumDigits != 14, ]
emergencial_not_14_04 <- emergencial042022[emergencial042022$NumDigits != 14, ]
emergencial_not_14_05 <- emergencial052022[emergencial052022$NumDigits != 14, ]
emergencial_not_14_06 <- emergencial062022[emergencial062022$NumDigits != 14, ]
emergencial_not_14_07 <- emergencial072022[emergencial072022$NumDigits != 14, ]
emergencial_not_14_08 <- emergencial082022[emergencial082022$NumDigits != 14, ]
emergencial_not_14_09 <- emergencial092022[emergencial092022$NumDigits != 14, ]
emergencial_not_14_10 <- emergencial102022[emergencial102022$NumDigits != 14, ]
emergencial_not_14_11 <- emergencial112022[emergencial112022$NumDigits != 14, ]
brasil_not_11_12 <- brasil122021[brasil122021$NumDigits != 11, ]
brasil_not_11_01 <- brasil012022[brasil012022$NumDigits != 11, ]
brasil_not_11_02 <- brasil022022[brasil022022$NumDigits != 11, ]
brasil_not_11_03 <- brasil032022[brasil032022$NumDigits != 11, ]
brasil_not_11_04 <- brasil042022[brasil042022$NumDigits != 11, ]
brasil_not_11_05 <- brasil052022[brasil052022$NumDigits != 11, ]
brasil_not_11_06 <- brasil062022[brasil062022$NumDigits != 11, ]
brasil_not_11_07 <- brasil072022[brasil072022$NumDigits != 11, ]
brasil_not_11_08 <- brasil082022[brasil082022$NumDigits != 11, ]
brasil_not_11_09 <- brasil092022[brasil092022$NumDigits != 11, ]
brasil_not_11_10 <- brasil102022[brasil102022$NumDigits != 11, ]
brasil_not_11_11 <- brasil112022[brasil112022$NumDigits != 11, ]

#Checagem NA valores <0 e valores não númericos para coluna do valor do benefício
#Nenhum dos auxílios possui qualquer um desses valores irregulares

sum(is.na(familia122017$VALOR.PARCELA))
sum(familia122017$VALOR.PARCELA < 0)
sum(sapply(familia122017$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia012018$VALOR.PARCELA))
sum(familia012018$VALOR.PARCELA < 0)
sum(sapply(familia012018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia022018$VALOR.PARCELA))
sum(familia022018$VALOR.PARCELA < 0)
sum(sapply(familia022018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia032018$VALOR.PARCELA))
sum(familia032018$VALOR.PARCELA < 0)
sum(sapply(familia032018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia042018$VALOR.PARCELA))
sum(familia042018$VALOR.PARCELA < 0)
sum(sapply(familia042018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia052018$VALOR.PARCELA))
sum(familia052018$VALOR.PARCELA < 0)
sum(sapply(familia052018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia062018$VALOR.PARCELA))
sum(familia062018$VALOR.PARCELA < 0)
sum(sapply(familia062018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia072018$VALOR.PARCELA))
sum(familia072018$VALOR.PARCELA < 0)
sum(sapply(familia072018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia082018$VALOR.PARCELA))
sum(familia082018$VALOR.PARCELA < 0)
sum(sapply(familia082018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia092018$VALOR.PARCELA))
sum(familia092018$VALOR.PARCELA < 0)
sum(sapply(familia092018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia102018$VALOR.PARCELA))
sum(familia102018$VALOR.PARCELA < 0)
sum(sapply(familia102018$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(familia112018$VALOR.PARCELA))
sum(familia112018$VALOR.PARCELA < 0)
sum(sapply(familia112018$VALOR.PARCELA, function(x) !is.double(x)))

sum(is.na(brasil122021$VALOR.PARCELA))
sum(brasil122021$VALOR.PARCELA < 0)
sum(sapply(brasil122021$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil012022$VALOR.PARCELA))
sum(brasil012022$VALOR.PARCELA < 0)
sum(sapply(brasil012022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil022022$VALOR.PARCELA))
sum(brasil022022$VALOR.PARCELA < 0)
sum(sapply(brasil012022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil032022$VALOR.PARCELA))
sum(brasil032022$VALOR.PARCELA < 0)
sum(sapply(brasil032022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil042022$VALOR.PARCELA))
sum(brasil042022$VALOR.PARCELA < 0)
sum(sapply(brasil042022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil052022$VALOR.PARCELA))
sum(brasil052022$VALOR.PARCELA < 0)
sum(sapply(brasil052022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil062022$VALOR.PARCELA))
sum(brasil062022$VALOR.PARCELA < 0)
sum(sapply(brasil062022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil072022$VALOR.PARCELA))
sum(brasil072022$VALOR.PARCELA < 0)
sum(sapply(brasil072022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil082022$VALOR.PARCELA))
sum(brasil082022$VALOR.PARCELA < 0)
sum(sapply(brasil082022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil092022$VALOR.PARCELA))
sum(brasil092022$VALOR.PARCELA < 0)
sum(sapply(brasil092022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil102022$VALOR.PARCELA))
sum(brasil102022$VALOR.PARCELA < 0)
sum(sapply(brasil102022$VALOR.PARCELA, function(x) !is.double(x)))
sum(is.na(brasil112022$VALOR.PARCELA))
sum(brasil112022$VALOR.PARCELA < 0)
sum(sapply(brasil112022$VALOR.PARCELA, function(x) !is.double(x)))

sum(is.na(emergencial122021$VALOR.BENEFÍCIO))
sum(emergencial122021$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial012022$VALOR.BENEFÍCIO))
sum(emergencial012022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial022022$VALOR.BENEFÍCIO))
sum(emergencial022022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial032022$VALOR.BENEFÍCIO))
sum(emergencial032022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(!is.numeric(emergencial032022$VALOR.BENEFÍCIO))
sum(is.na(emergencial042022$VALOR.BENEFÍCIO))
sum(emergencial042022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(!is.numeric(emergencial042022$VALOR.BENEFÍCIO))
sum(is.na(emergencial052022$VALOR.BENEFÍCIO))
sum(emergencial052022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial062022$VALOR.BENEFÍCIO))
sum(emergencial062022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial072022$VALOR.BENEFÍCIO))
sum(emergencial072022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial082022$VALOR.BENEFÍCIO))
sum(emergencial082022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial092022$VALOR.BENEFÍCIO))
sum(emergencial092022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial102022$VALOR.BENEFÍCIO))
sum(emergencial102022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))
sum(is.na(emergencial112022$VALOR.BENEFÍCIO))
sum(emergencial112022$VALOR.BENEFÍCIO < 0)
sum(sapply(emergencial122021$VALOR.BENEFÍCIO, function(x) !is.double(x)))

length(unique(familia122017$VALOR.PARCELA))
familia122017 <- familia122017 %>%
  select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
  summarise(
    UF = first(UF),
    CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
    NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
    CPF.FAVORECIDO = first(CPF.FAVORECIDO),
    NIS.FAVORECIDO = first(NIS.FAVORECIDO),
    NOME.FAVORECIDO = first(NOME.FAVORECIDO),
    VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia012018 <- familia012018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia022018 <- familia022018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia032018 <- familia032018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia042018 <- familia042018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia052018 <- familia052018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia062018 <- familia062018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia072018 <- familia072018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia082018 <- familia082018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia092018 <- familia092018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia102018 <- familia102018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  familia112018 <- familia112018 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
  group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  
  brasil122021 <- brasil122021 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  brasil012022 <- brasil012022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  +++brasil042022 <- brasil042022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  brasil052022 <- brasil052022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  brasil062022 <- brasil062022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  brasil072022 <- brasil072022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
     ----= first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  brasil082022 <- brasil082022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  brasil092022 <- brasil092022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  brasil102022 <- brasil102022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  brasil112022 <- brasil112022 %>%
    select(UF, CÓDIGO.MUNICÍPIO.SIAFI, NOME.MUNICÍPIO, CPF.FAVORECIDO, NIS.FAVORECIDO, NOME.FAVORECIDO, VALOR.PARCELA) %>%
    group_by(NIS.FAVORECIDO) %>%
    summarise(
      UF = first(UF),
      CÓDIGO.MUNICÍPIO.SIAFI = first(CÓDIGO.MUNICÍPIO.SIAFI),
      NOME.MUNICÍPIO = first(NOME.MUNICÍPIO),
      CPF.FAVORECIDO = first(CPF.FAVORECIDO),
      NIS.FAVORECIDO = first(NIS.FAVORECIDO),
      NOME.FAVORECIDO = first(NOME.FAVORECIDO),
      VALOR.PARCELA = sum(VALOR.PARCELA)
    )
  
familia122017 <- familia122017 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia012018 <- familia012018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia022018_2 <- familia022018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia032018 <- familia032018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia042018 <- familia042018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia052018 <- familia052018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia062018 <- familia062018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia072018 <- familia072018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia082018 <- familia082018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia092018 <- familia092018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia102018 <- familia102018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
familia112018 <- familia112018 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
emergencial122021 <- emergencial122021 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial012022 <- emergencial012022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial022022 <- emergencial022022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial032022 <- emergencial032022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial042022 <- emergencial042022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial052022 <- emergencial052022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial062022 <- emergencial062022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial072022 <- emergencial072022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial082022 <- emergencial082022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial092022 <- emergencial092022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial102022 <- emergencial102022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
emergencial112022 <- emergencial112022 %>%
  group_by(CÓDIGO.MUNICÍPIO.IBGE) %>%
  summarise(monetary_value = sum(VALOR.BENEFÍCIO),
            individuals = n_distinct(CPF.BENEFICIÁRIO))
brasil122021 <- brasil122021 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil012022 <- brasil012022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil022022 <- brasil022022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil032022 <- brasil032022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil042022 <- brasil042022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil052022 <- brasil052022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil062022 <- brasil062022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil072022 <- brasil072022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil082022 <- brasil082022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil092022 <- brasil092022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil102022 <- brasil102022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))
brasil112022 <- brasil112022 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(monetary_value = sum(VALOR.PARCELA),
            individuals = n_distinct(NIS.FAVORECIDO))

familia2018 <- left_join(familia122017, familia012018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia022018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia032018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia042018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia052018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia062018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia072018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia082018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia092018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia102018, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  left_join(., familia112018, by = "CÓDIGO.MUNICÍPIO.SIAFI") 
colnames(familia2018) <- c("CÓDIGO.MUNICÍPIO.SIAFI","monetary_value_dec", "individuals_dec", "monetary_value_jan", "individuals_jan", "monetary_value_feb", "individuals_feb","monetary_value_mar", "individuals_mar","monetary_value_apr", "individuals_apr","monetary_value_may", "individuals_may","monetary_value_jun", "individuals_jun","monetary_value_jul", "individuals_jul","monetary_value_aug", "individuals_aug","monetary_value_sep", "individuals_sep","monetary_value_oct", "individuals_oct","monetary_value_nov", "individuals_nov")
familia2018$ano <- 2018
familia2018 <- familia2018[,c(1, 26, 2:25)]
emergencial2022 <- full_join(emergencial122021, emergencial012022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial022022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial032022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial042022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial052022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial062022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial072022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial082022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial092022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial102022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., emergencial112022, by = "CÓDIGO.MUNICÍPIO.SIAFI") 
colnames(emergencial2022) <- c("CÓDIGO.MUNICÍPIO.SIAFI","monetary_value_dec", "individuals_dec", "monetary_value_jan", "individuals_jan", "monetary_value_feb", "individuals_feb","monetary_value_mar", "individuals_mar","monetary_value_apr", "individuals_apr","monetary_value_may", "individuals_may","monetary_value_jun", "individuals_jun","monetary_value_jul", "individuals_jul","monetary_value_aug", "individuals_aug","monetary_value_sep", "individuals_sep","monetary_value_oct", "individuals_oct","monetary_value_nov", "individuals_nov")
emergencial2022$ano <- 2022
emergencial2022 <- emergencial2022[, c(1, 26, 2:25)]
brasil2022 <- full_join(brasil122021, brasil012022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil022022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil032022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil042022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil052022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil062022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil072022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil082022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil092022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil102022, by = "CÓDIGO.MUNICÍPIO.SIAFI") %>%
  full_join(., brasil112022, by = "CÓDIGO.MUNICÍPIO.SIAFI") 
colnames(brasil2022) <- c("CÓDIGO.MUNICÍPIO.SIAFI","monetary_value_dec", "individuals_dec", "monetary_value_jan", "individuals_jan", "monetary_value_feb", "individuals_feb","monetary_value_mar", "individuals_mar","monetary_value_apr", "individuals_apr","monetary_value_may", "individuals_may","monetary_value_jun", "individuals_jun","monetary_value_jul", "individuals_jul","monetary_value_aug", "individuals_aug","monetary_value_sep", "individuals_sep","monetary_value_oct", "individuals_oct","monetary_value_nov", "individuals_nov")
brasil2022$ano <- 2022
brasil2022 <- brasil2022[, c(1, 26, 2:25)]

rm(familia012018)
rm(familia022018)
rm(familia032018)
rm(familia042018)
rm(familia052018)
rm(familia062018)
rm(familia072018)
rm(familia082018)
rm(familia092018)
rm(familia102018)
rm(familia112018)
rm(familia122017)
rm(emergencial012022)
rm(emergencial022022)
rm(emergencial032022)
rm(emergencial042022)
rm(emergencial052022)
rm(emergencial062022)
rm(emergencial072022)
rm(emergencial082022)
rm(emergencial092022)
rm(emergencial102022)
rm(emergencial112022)
rm(emergencial122021)
rm(brasil012022)
rm(brasil022022)
rm(brasil032022)
rm(brasil042022)
rm(brasil052022)
rm(brasil062022)
rm(brasil072022)
rm(brasil082022)
rm(brasil092022)
rm(brasil102022)
rm(brasil112022)
rm(brasil122021)

rm(familia_not_11_12)
rm(familia_not_11_01)
rm(familia_not_11_02)
rm(familia_not_11_03)
rm(familia_not_11_04)
rm(familia_not_11_05)
rm(familia_not_11_06)
rm(familia_not_11_07)
rm(familia_not_11_08)
rm(familia_not_11_09)
rm(familia_not_11_10)
rm(familia_not_11_11)
rm(emergencial_not_11_12)
rm(emergencial_not_11_01)
rm(emergencial_not_11_02)
rm(emergencial_not_11_03)
rm(emergencial_not_11_04)
rm(emergencial_not_11_05)
rm(emergencial_not_11_06)
rm(emergencial_not_11_07)
rm(emergencial_not_11_08)
rm(emergencial_not_11_09)
rm(emergencial_not_11_10)
rm(emergencial_not_11_11)
rm(brasil_not_11_12)
rm(brasil_not_11_01)
rm(brasil_not_11_02)
rm(brasil_not_11_03)
rm(brasil_not_11_04)
rm(brasil_not_11_05)
rm(brasil_not_11_06)
rm(brasil_not_11_07)
rm(brasil_not_11_08)
rm(brasil_not_11_09)
rm(brasil_not_11_10)
rm(brasil_not_11_11)


#Dados portal da transparência
setwd("C:\\Users\\lucas\\Documents\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Tabelas tratadas")
siafi_ibge <- read.csv("Relação dados SIAFI-IBGE.csv", header = F, encoding = "latin1", sep = ";")
siafi_ibge <- siafi_ibge[, c(1,5)]
colnames (siafi_ibge) <- c("CÓDIGO.MUNICÍPIO.SIAFI", "CÓDIGO.MUNICÍPIO.IBGE")
brasil2022 <- left_join(brasil2022, siafi_ibge, by = "CÓDIGO.MUNICÍPIO.SIAFI")
brasil2022 <- brasil2022[, c(27, 2:26)]brasil2022 <- left_join(brasil2022, siafi_ibge, by = "CÓDIGO.MUNICÍPIO.SIAFI")
familia2018 <- left_join(familia2018, siafi_ibge, by = "CÓDIGO.MUNICÍPIO.SIAFI")
familia2018 <- familia2018[, c(27, 2:26)]
emergencial2022 <- replace(emergencial2022, is.na(emergencial2022), 0)
transp2022 <- left_join(brasil2022, emergencial2022, by = c("CÓDIGO.MUNICÍPIO.IBGE", "ano"))
for (i in 3:26) {
  transp2022[,i] <- transp2022[,i] + transp2022[,i + 24]
}
transp2022 <- transp2022[,1:26]
colnames(transp2022) <- c("CÓDIGO.MUNICÍPIO.IBGE", "ano", "monetary_value_12_2021", "individuals_12_2021", "monetary_value_01_2022", "individuals_01_2022", "monetary_value_02_2022", "individuals_02_2022","monetary_value_03_2022", "individuals_03_2022","monetary_value_04_2022", "individuals_04_2022","monetary_value_05_2022", "individuals_05_2022","monetary_value_06_2022", "individuals_06_2022","monetary_value_07_2022", "individuals_07_2022","monetary_value_08_2022", "individuals_08_2022","monetary_value_09_2022", "individuals_09_2022","monetary_value_10_2022", "individuals_10_2022","monetary_value_11_2022", "individuals_11_2022")
transp <- rbind(familia2018, transp2022)

#Painel
painel <- painel[,c(1, 3:9, 10, 13, 17, 18, 21, 25, 26, 29, 33)]
painel2 <- painel2[,c(1, 3:9, 10, 13, 17, 18, 21, 25, 26, 29, 33)]
painel$CÓDIGO.MUNICÍPIO.SIAFI <- as.integer(ifelse(is.na(painel$CÓDIGO.MUNICÍPIO.SIAFI.x), 
                      ifelse(is.na(painel$CÓDIGO.MUNICÍPIO.SIAFI.y),
                      ifelse(is.na(painel$CÓDIGO.MUNICÍPIO.SIAFI.x.x),
                      ifelse(is.na(painel$CÓDIGO.MUNICÍPIO.SIAFI.y.y),
                             "NA", painel$CÓDIGO.MUNICÍPIO.SIAFI.y.y),
                              painel$CÓDIGO.MUNICÍPIO.SIAFI.x.x),
                              painel$CÓDIGO.MUNICÍPIO.SIAFI.y),
                              painel$CÓDIGO.MUNICÍPIO.SIAFI.x))
painel2$CÓDIGO.MUNICÍPIO.SIAFI <- as.integer(ifelse(is.na(painel2$CÓDIGO.MUNICÍPIO.SIAFI.x), 
                                      ifelse(is.na(painel2$CÓDIGO.MUNICÍPIO.SIAFI.y),
                                        ifelse(is.na(painel2$CÓDIGO.MUNICÍPIO.SIAFI.x.x),
                                          ifelse(is.na(painel2$CÓDIGO.MUNICÍPIO.SIAFI.y.y),
                                            "NA", painel2$CÓDIGO.MUNICÍPIO.SIAFI.y.y),
                                              painel2$CÓDIGO.MUNICÍPIO.SIAFI.x.x),
                                               painel2$CÓDIGO.MUNICÍPIO.SIAFI.y),
                                                painel2$CÓDIGO.MUNICÍPIO.SIAFI.x))
painel3$CÓDIGO.MUNICÍPIO.SIAFI <- as.integer(ifelse(is.na(painel3$CÓDIGO.MUNICÍPIO.SIAFI.x), 
                                                    ifelse(is.na(painel3$CÓDIGO.MUNICÍPIO.SIAFI.y),
                                                           ifelse(is.na(painel3$CÓDIGO.MUNICÍPIO.SIAFI.x.x),
                                                                  ifelse(is.na(painel3$CÓDIGO.MUNICÍPIO.SIAFI.y.y),
                                                                         "NA", painel3$CÓDIGO.MUNICÍPIO.SIAFI.y.y),
                                                                  painel3$CÓDIGO.MUNICÍPIO.SIAFI.x.x),
                                                           painel3$CÓDIGO.MUNICÍPIO.SIAFI.y),
                                                    painel3$CÓDIGO.MUNICÍPIO.SIAFI.x))
painel_monetario <- rbind(painel_monetario, painel_monetario_2)
painel_monetario <- painel_monetario %>%
  mutate(ano = ifelse(row_number() <= 5570, 2018, 2022))
relação_dados_siafi_ibge$CÓDIGO.MUNICÍPIO.SIAFI <- as.integer(relação_dados_siafi_ibge$CÓDIGO.MUNICÍPIO.SIAFI)
painel_monetario <- left_join(painel_monetario, relação_dados_siafi_ibge, by = "CÓDIGO.MUNICÍPIO.SIAFI")
painel_monetario$total_value <- painel_monetario$monetary_value.x + painel_monetario$monetary_value.y + painel_monetario$monetary_value.x.x + painel_monetario$monetary_value.y.y 
painel_monetario_3 <- left_join(painel_monetario_3, relação_dados_siafi_ibge, by = "CÓDIGO.MUNICÍPIO.SIAFI")
painel_monetario_3$total_value <- painel_monetario_3$monetary_value.x + painel_monetario_3$monetary_value.y + painel_monetario_3$monetary_value.x.x + painel_monetario_3$monetary_value.y.y
painel_monetario <- rbind(painel_monetario, painel_monetario_3)
rm(painel_monetario_3)
painel_monetario <- painel_monetario %>%
  arrange(id_municipio, ano)
beneficiarios <- painel %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(individuals = n_distinct(CPF.FAVORECIDO))
beneficiarios2 <- painel2 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(individuals = n_distinct(CPF.FAVORECIDO))
beneficiarios3 <- painel3 %>%
  group_by(CÓDIGO.MUNICÍPIO.SIAFI) %>%
  summarise(individuals = n_distinct(CPF.FAVORECIDO))
beneficiarios <- rbind(beneficiarios, beneficiarios2)
beneficiarios$ano <- ifelse(1:nrow(painel) <= 5570, 2018, 2022)
beneficiarios3$ano <- 2014
beneficiarios <- rbind(beneficiarios, beneficiarios3)
rm(beneficiarios3)
beneficiarios <- beneficiarios %>%
  arrange(CÓDIGO.MUNICÍPIO.SIAFI, ano)
relação_dados_siafi_ibge <- read.csv2("C:/Users/lucas/Documents/Profissional/Faculdade/Mestrado/Dissertação/Dados/Relação dados SIAFI-IBGE.csv", header=FALSE, encoding = "latin1")
colnames (relação_dados_siafi_ibge) <- c("CÓDIGO.MUNICÍPIO.SIAFI", "numero", "nome_municipio", "uf", "id_municipio")
relação_dados_siafi_ibge$CÓDIGO.MUNICÍPIO.SIAFI <- as.character(relação_dados_siafi_ibge$CÓDIGO.MUNICÍPIO.SIAFI)
relação_dados_siafi_ibge <- relação_dados_siafi_ibge[,c(1,5)]
painel <- left_join(beneficiarios, relação_dados_siafi_ibge, by = "CÓDIGO.MUNICÍPIO.SIAFI")
painel <- painel[, c(4, 3, 2)]
painel$id_municipio <- as.character(painel$id_municipio)
painel$ano <- as.integer(painel$ano)
painel <- left_join(datatse, painel, by = c("id_municipio", "ano"))
painel$proporcao_beneficiarios <- painel$individuals/painel$aptos
painel <- painel[, c(1:25, 71:72, 26:70)]
painel_monetario$id_municipio <- as.character(painel_monetario$id_municipio)
painel_monetario$ano <- as.integer(painel_monetario$ano)
painel <- left_join(painel, painel_monetario, by = c("id_municipio","ano"))
painel <- painel[,c(1:72, 78)]
painel$log_PARCELA_per_capta <- log(painel$total_value/painel$individuals)
painel_robustez_1 <- painel %>%
  filter(ano == 2014 | ano == 2018 | turno == 1)
painel_robustez_1turno <- painel_robustez %>%
  filter(turno == 1)
rm(relação_dados_siafi_ibge)
colnames(painel_robustez_1turno)[40] <- "quantidade_25_34"
colnames(painel_robustez_1turno)[41] <- "proporcao_25_34"
#Summary statistics
media_quantidade_16 <- mean(painel_1turno$quantidade_16, na.rm = T)
painel_1turno$quantidade_16[is.na(painel_1turno$quantidade_16)] <- media_quantidade_16
rm(media_quantidade_16)
summary_statistics <- data.frame("2018" = c(sum(painel_1turno$votos_direita[painel_1turno$ano == "2018"])/sum(painel_1turno$votos_validos[painel_1turno$ano == "2018"]),
                                            1 - sum(painel_1turno$votos_direita[painel_1turno$ano == "2018"])/sum(painel_1turno$votos_validos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$individuals[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$mulheres[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            1 - sum(painel_1turno$mulheres[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_analfabeto[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_le_e_escreve[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_fundamental_incompleto[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_fundamental_completo[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_medio_incompleto[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_medio_completo[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_superior_incompleto[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_16[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_17[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_18_20[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_21_24[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_25_34[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_35_44[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_45_59[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_60_69[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_70_79[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_superior_79[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_casados[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_divorciados[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$quantidade_viuvos[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]),
                                            sum(painel_1turno$eleitores_deficientes[painel_1turno$ano == "2018"])/sum(painel_1turno$aptos[painel_1turno$ano == "2018"]))
,
                                 "2022" = c(sum(painel_1turno$votos_direita[painel_1turno$ano == "2022"])/sum(painel_1turno$votos_validos[painel_1turno$ano == "2022"]),
                                            1 - sum(painel_1turno$votos_direita[painel_1turno$ano == "2022"])/sum(painel_1turno$votos_validos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$individuals[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$mulheres[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            1 - sum(painel_1turno$mulheres[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_analfabeto[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_le_e_escreve[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_fundamental_incompleto[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_fundamental_completo[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_medio_incompleto[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_medio_completo[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_superior_incompleto[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_16[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_17[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_18_20[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_21_24[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_25_34[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_35_44[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_45_59[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_60_69[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_70_79[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_superior_79[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_casados[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_divorciados[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$quantidade_viuvos[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"]),
                                            sum(painel_1turno$eleitores_deficientes[painel_1turno$ano == "2022"])/sum(painel_1turno$aptos[painel_1turno$ano == "2022"])),
row.names = c("Incumbent", "Opposition", "Fraction of Beneficiaries", "Female", "Male", "Illiterate", "Read and write, but without formal education", "Primary Incomplete", "Primary Complete", "High School Incomplete", "High School Complete", "College Incomplete", "age 16", "age 17", "age 18-20", "age 21-24", "age 25-34", "age 35-44", "age 45-59", "age 60-69", "age 70-79", "age over 79", "Married", "Divorced", "Widower", "disabled")
)

#Regressões
colnames(painel)[40] <- "quantidade_25_34"
colnames(painel)[41] <- "proporcao_25_34"
painel$proporcao_mulheres <- as.numeric(painel$proporcao_mulheres)
painel$proporcao_casados <- as.numeric(painel$proporcao_casados)
painel$proporcao_divorciados <- as.numeric(painel$proporcao_divorciados)
painel$proporcao_viuvos <- as.numeric(painel$proporcao_viuvos)
painel$proporcao_16 <- as.numeric(painel$proporcao_16)
painel$proporcao_17 <- as.numeric(painel$proporcao_17)
painel$proporcao_18_20 <- as.numeric(painel$proporcao_18_20)
painel$proporcao_21_24 <- as.numeric(painel$proporcao_21_24)
painel$proporcao_25_34 <- as.numeric(painel$proporcao_25_34)
painel$proporcao_35_44 <- as.numeric(painel$proporcao_35_44)
painel$proporcao_45_59 <- as.numeric(painel$proporcao_45_59)
painel$proporcao_60_69 <- as.numeric(painel$proporcao_60_69)
painel$proporcao_70_79 <- as.numeric(painel$proporcao_70_79)
painel$proporcao_analfabeto <- as.numeric(painel$proporcao_analfabeto)
painel$proporcao_le_e_escreve <- as.numeric(painel$proporcao_le_e_escreve)
painel$proporcao_fundamental_incompleto <- as.numeric(painel$proporcao_fundamental_incompleto)
painel$proporcao_fundamental_completo <- as.numeric(painel$proporcao_fundamental_completo)
painel$proporcao_medio_incompleto <- as.numeric(painel$proporcao_medio_incompleto)
painel$proporcao_medio_completo <- as.numeric(painel$proporcao_medio_completo)
painel$proporcao_superior_incompleto <- as.numeric(painel$proporcao_superior_incompleto)
painel$proporcao_deficientes <- as.numeric(painel$proporcao_deficientes)
#Turnout
  #1 turno
painel_1turno <- painel %>%
  filter(ano == 2018 | ano == 2022) %>%
  filter(turno == 1)
rm(painel)
regressao <- lm(proporcao_comparecimento ~ proporcao_beneficiarios, data = painel_1turno)
stargazer(regressao, type = "text")
regressao_monetaria <- lm(proporcao_comparecimento ~ log_PARCELA_per_capta, 
                          data = painel_1turno)
stargazer(regressao_monetaria, type = "text")
regressao2 <- lm(proporcao_comparecimento ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio)
                 , data = painel_1turno)
summary(regressao2)
regressao_monetaria_2 <- lm(proporcao_comparecimento ~ log_PARCELA_per_capta + 
                              factor(ano) + factor(id_municipio)
                            , data = painel_1turno)
summary(regressao_monetaria_2)
sum(is.na(painel_1turno$proporcao_16))
typeof(painel_1turno$proporcao_16)
painel_1turno$proporcao_mulheres <- as.numeric(painel_1turno$proporcao_mulheres)
painel_1turno$proporcao_casados <- as.numeric(painel_1turno$proporcao_casados)
painel_1turno$proporcao_divorciados <- as.numeric(painel_1turno$proporcao_divorciados)
painel_1turno$proporcao_viuvos <- as.numeric(painel_1turno$proporcao_viuvos)
painel_1turno$proporcao_16 <- as.numeric(painel_1turno$proporcao_16)
painel_1turno$proporcao_17 <- as.numeric(painel_1turno$proporcao_17)
painel_1turno$proporcao_18_20 <- as.numeric(painel_1turno$proporcao_18_20)
painel_1turno$proporcao_21_24 <- as.numeric(painel_1turno$proporcao_21_24)
painel_1turno$proporcao_25_34 <- as.numeric(painel_1turno$proporcao_25_34)
painel_1turno$proporcao_35_44 <- as.numeric(painel_1turno$proporcao_35_44)
painel_1turno$proporcao_45_59 <- as.numeric(painel_1turno$proporcao_45_59)
painel_1turno$proporcao_60_69 <- as.numeric(painel_1turno$proporcao_60_69)
painel_1turno$proporcao_70_79 <- as.numeric(painel_1turno$proporcao_70_79)
painel_1turno$proporcao_analfabeto <- as.numeric(painel_1turno$proporcao_analfabeto)
painel_1turno$proporcao_le_e_escreve <- as.numeric(painel_1turno$proporcao_le_e_escreve)
painel_1turno$proporcao_fundamental_incompleto <- as.numeric(painel_1turno$proporcao_fundamental_incompleto)
painel_1turno$proporcao_fundamental_completo <- as.numeric(painel_1turno$proporcao_fundamental_completo)
painel_1turno$proporcao_medio_incompleto <- as.numeric(painel_1turno$proporcao_medio_incompleto)
painel_1turno$proporcao_medio_completo <- as.numeric(painel_1turno$proporcao_medio_completo)
painel_1turno$proporcao_superior_incompleto <- as.numeric(painel_1turno$proporcao_superior_incompleto)
painel_1turno$proporcao_deficientes <- as.numeric(painel_1turno$proporcao_deficientes)
media_proporcao_16 <- mean(painel_1turno$proporcao_16, na.rm = T)
painel_1turno$proporcao_16[is.na(painel_1turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
regressao3 <- lm(proporcao_comparecimento ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                   proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_1turno)
summary(regressao3)
regressao4 <- lm(proporcao_comparecimento ~ proporcao_beneficiarios + factor(ano) + proporcao_mulheres + 
                   proporcao_casados + proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_1turno)
summary(regressao4)
regressao_monetaria_3 <-lm(proporcao_comparecimento ~ log_PARCELA_per_capta + factor(ano) + 
                   factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                   proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_1turno)
regressao_monetaria_4 <-lm(proporcao_comparecimento ~ log_PARCELA_per_capta + factor(ano) + 
                             proporcao_mulheres + proporcao_casados + proporcao_divorciados + 
                             proporcao_viuvos + proporcao_16 + proporcao_17 + proporcao_18_20 + 
                             proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                             proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                             proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                             proporcao_fundamental_completo + proporcao_medio_incompleto + 
                             proporcao_medio_completo + proporcao_superior_incompleto + proporcao_deficientes
                           , data = painel_1turno)
summary(regressao_monetaria_4)  
#2 turno
painel_2turno <- painel %>%
  filter(ano == 2018 | ano == 2022) %>%
  filter(turno == 2)
rm(painel)
regressao <- lm(proporcao_comparecimento ~ proporcao_beneficiarios, data = painel_2turno)
stargazer(regressao, type = "text")
regressao_monetaria <- lm(proporcao_comparecimento ~ log_PARCELA_per_capta, 
                          data = painel_2turno)
stargazer(regressao_monetaria, type = "text")
regressao2 <- lm(proporcao_comparecimento ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio)
                 , data = painel_2turno)
summary(regressao2)
regressao_monetaria_2 <- lm(proporcao_comparecimento ~ log_PARCELA_per_capta + 
                              factor(ano) + factor(id_municipio)
                            , data = painel_2turno)
summary(regressao_monetaria_2)
painel_2turno$proporcao_mulheres <- as.numeric(painel_2turno$proporcao_mulheres)
painel_2turno$proporcao_casados <- as.numeric(painel_2turno$proporcao_casados)
painel_2turno$proporcao_divorciados <- as.numeric(painel_2turno$proporcao_divorciados)
painel_2turno$proporcao_viuvos <- as.numeric(painel_2turno$proporcao_viuvos)
painel_2turno$proporcao_16 <- as.numeric(painel_2turno$proporcao_16)
painel_2turno$proporcao_17 <- as.numeric(painel_2turno$proporcao_17)
painel_2turno$proporcao_18_20 <- as.numeric(painel_2turno$proporcao_18_20)
painel_2turno$proporcao_21_24 <- as.numeric(painel_2turno$proporcao_21_24)
painel_2turno$proporcao_25_34 <- as.numeric(painel_2turno$proporcao_25_34)
painel_2turno$proporcao_35_44 <- as.numeric(painel_2turno$proporcao_35_44)
painel_2turno$proporcao_45_59 <- as.numeric(painel_2turno$proporcao_45_59)
painel_2turno$proporcao_60_69 <- as.numeric(painel_2turno$proporcao_60_69)
painel_2turno$proporcao_70_79 <- as.numeric(painel_2turno$proporcao_70_79)
painel_2turno$proporcao_analfabeto <- as.numeric(painel_2turno$proporcao_analfabeto)
painel_2turno$proporcao_le_e_escreve <- as.numeric(painel_2turno$proporcao_le_e_escreve)
painel_2turno$proporcao_fundamental_incompleto <- as.numeric(painel_2turno$proporcao_fundamental_incompleto)
painel_2turno$proporcao_fundamental_completo <- as.numeric(painel_2turno$proporcao_fundamental_completo)
painel_2turno$proporcao_medio_incompleto <- as.numeric(painel_2turno$proporcao_medio_incompleto)
painel_2turno$proporcao_medio_completo <- as.numeric(painel_2turno$proporcao_medio_completo)
painel_2turno$proporcao_superior_incompleto <- as.numeric(painel_2turno$proporcao_superior_incompleto)
painel_2turno$proporcao_deficientes <- as.numeric(painel_2turno$proporcao_deficientes)
media_proporcao_16 <- mean(painel_2turno$proporcao_16, na.rm = T)
painel_2turno$proporcao_16[is.na(painel_2turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
regressao3 <- lm(proporcao_comparecimento ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                   proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_2turno)
summary(regressao3)
regressao4 <- lm(proporcao_comparecimento ~ proporcao_beneficiarios + factor(ano) + proporcao_mulheres + 
                   proporcao_casados + proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_2turno)
summary(regressao4)
regressao_monetaria_3 <-lm(proporcao_comparecimento ~ log_PARCELA_per_capta + factor(ano) + 
                             factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                             proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                             proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                             proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                             proporcao_analfabeto + proporcao_le_e_escreve + 
                             proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                             proporcao_medio_incompleto + proporcao_medio_completo + 
                             proporcao_superior_incompleto + proporcao_deficientes
                           , data = painel_2turno)
regressao_monetaria_4 <-lm(proporcao_comparecimento ~ log_PARCELA_per_capta + factor(ano) + 
                             proporcao_mulheres + proporcao_casados + proporcao_divorciados + 
                             proporcao_viuvos + proporcao_16 + proporcao_17 + proporcao_18_20 + 
                             proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                             proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                             proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                             proporcao_fundamental_completo + proporcao_medio_incompleto + 
                             proporcao_medio_completo + proporcao_superior_incompleto + proporcao_deficientes
                           , data = painel_2turno)
summary(regressao_monetaria_4)
#Principal
  #Incumbente 
    #1 turno
painel_1turno <- painel %>%
  filter(ano == 2018 | ano == 2022) %>%
  filter(turno == 1)
regressao <- lm(vote_share_direita ~ proporcao_beneficiarios, data = painel_1turno)
stargazer(regressao, type = "text")
regressao_monetaria <- lm(vote_share_direita ~ log_PARCELA_per_capta, 
                          data = painel_1turno)
stargazer(regressao_monetaria, type = "text")
regressao2 <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio)
                 , data = painel_1turno)
summary(regressao2)
regressao_monetaria_2 <- lm(vote_share_direita ~ log_PARCELA_per_capta + factor(ano) + 
                              factor(id_municipio)
                            , data = painel_1turno)
summary(regressao_monetaria_2)
media_proporcao_16 <- mean(painel_1turno$proporcao_16, na.rm = T)
painel_1turno$proporcao_16[is.na(painel_1turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
regressao3 <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                   proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_1turno)
summary(regressao3)
regressao_monetaria_3 <- lm(vote_share_direita ~ log_PARCELA_per_capta + factor(ano) + 
                             factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                             proporcao_divorciados + proporcao_viuvos + proporcao_16 + 
                             proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                             proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                             proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                             proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                             proporcao_fundamental_completo + proporcao_medio_incompleto + 
                             proporcao_medio_completo + proporcao_superior_incompleto + 
                             proporcao_deficientes
                           , data = painel_1turno)
summary(regressao_monetaria_3)
regressao4 <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + proporcao_mulheres + 
                   proporcao_casados + proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_1turno)
summary(regressao4)
regressao_monetaria_4 <-lm(vote_share_direita ~ log_PARCELA_per_capta + factor(ano) + 
                             proporcao_mulheres + proporcao_casados + proporcao_divorciados + 
                             proporcao_viuvos + proporcao_16 + proporcao_17 + proporcao_18_20 + 
                             proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                             proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                             proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                             proporcao_fundamental_completo + proporcao_medio_incompleto + 
                             proporcao_medio_completo + proporcao_superior_incompleto + proporcao_deficientes
                           , data = painel_1turno)
summary(regressao_monetaria_4)
    #2 turno
painel_2turno <- painel %>%
  filter(ano == 2018 | ano == 2022) %>%
  filter(turno == 2)
regressao_2_turno <- lm(vote_share_direita ~ proporcao_beneficiarios, data = painel_2turno)
stargazer(regressao_2_turno, type = "text")
regressao_monetaria_2_turno <- lm(vote_share_direita ~ log_PARCELA_per_capta, 
                          data = painel_2turno)
stargazer(regressao_monetaria_2_turno, type = "text")
regressao2_turno_2 <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio)
                 , data = painel_2turno)
summary(regressao2_turno_2)
regressao_monetaria_2_turno_2 <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                 factor(ano) + factor(id_municipio)
                            , data = painel_2turno)
summary(regressao_monetaria_2_turno_2)
media_proporcao_16 <- mean(painel_2turno$proporcao_16, na.rm = T)
painel_2turno$proporcao_16[is.na(painel_2turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
regressao3_turno_2 <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                   proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_2turno)
summary(regressao3_turno_2)
regressao_monetaria_2_turno_3 <- lm(vote_share_direita ~ log_PARCELA_per_capta + factor(ano) + 
                              factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                              proporcao_divorciados + proporcao_viuvos + proporcao_16 + 
                              proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                              proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                              proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                              proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                              proporcao_fundamental_completo + proporcao_medio_incompleto + 
                              proporcao_medio_completo + proporcao_superior_incompleto + 
                              proporcao_deficientes
                            , data = painel_2turno)
summary(regressao_monetaria_2_turno_3)
regressao4 <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + proporcao_mulheres + 
                   proporcao_casados + proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_2turno)
summary(regressao4)
regressao_monetaria_4 <-lm(vote_share_direita ~ log_PARCELA_per_capta + factor(ano) + 
                             proporcao_mulheres + proporcao_casados + proporcao_divorciados + 
                             proporcao_viuvos + proporcao_16 + proporcao_17 + proporcao_18_20 + 
                             proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                             proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                             proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                             proporcao_fundamental_completo + proporcao_medio_incompleto + 
                             proporcao_medio_completo + proporcao_superior_incompleto + proporcao_deficientes
                           , data = painel_2turno)
summary(regressao_monetaria_4)
painel_lag <- painel_2turno %>%
  filter(ano == 2018)
painel_lag2 <- painel_2turno %>%
  filter(ano == 2022)
painel_lag$deltaproporcao_beneficiarios <- painel_lag2$proporcao_beneficiarios - painel_lag$proporcao_beneficiarios
painel_lag$delta_volume <- painel_lag2$total_value - painel_lag$total_value
painel_lag$volume_per_capta <- painel_lag$total_value/painel_lag$individuals
painel_lag2$volume_per_capta <- painel_lag2$total_value/painel_lag2$individuals
painel_lag$delta_volume_per_capta <- painel_lag2$volume_per_capta - painel_lag$volume_per_capta
painel_lag2 <- painel_lag2[,c(4,10)]
colnames(painel_lag2)[colnames(painel_lag2) == "vote_share_direita"] <- "vote_share_2022"
colnames(painel_lag)[colnames(painel_lag) == "vote_share_direita"] <- "vote_share_2018"
painel_lag <- left_join(painel_lag, painel_lag2, by = "id_municipio")
painel_lag$delta_vote_share <- painel_lag$vote_share_2022 - painel_lag$vote_share_2018
painel_lag <- painel_lag[,c(74, 10, 75, 70, 71, 73, 2:69, 72)]
regressao_lag_pbf <- lm(vote_share_2022~vote_share_2018 + deltaproporcao_beneficiarios, data = painel_lag)
summary(regressao_lag_pbf)
regressao_lag_volume <- lm(vote_share_2022~vote_share_2018 + delta_volume, data = painel_lag)
summary(regressao_lag_volume)
regressao_delta_pbf <- lm(delta_vote_share ~ deltaproporcao_beneficiarios, data = painel_lag)
summary(regressao_delta_pbf)
regressao_delta_volume <- lm(delta_vote_share ~ delta_volume, data = painel_lag)
summary(regressao_delta_volume)
regressao_delta_volume_per_capta <- lm(delta_vote_share ~ delta_volume_per_capta, data = painel_lag)
summary(regressao_delta_volume_per_capta)
painel_lag <- painel_lag[,c(1:9)]
scatterplot <- ggplot(painel_lag, aes(x = deltaproporcao_beneficiarios, y = delta_vote_share)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  labs(x = "Change in Proportion of Beneficiaries",
       y = "Change in Vote Share") +  # Add axis labels
  ggtitle("Scatterplot with Regression Line")  # Add title
print(scatterplot)
scatterplot2 <- ggplot(painel_lag, aes(x = delta_volume, y = delta_vote_share)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  labs(x = "Change in total volume of benefit",
       y = "Change in Vote Share") +  # Add axis labels
  ggtitle("Scatterplot with Regression Line")  # Add title
print(scatterplot2)
scatterplot3 <- ggplot(painel_lag, aes(x = delta_volume_per_capta, y = delta_vote_share)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  labs(x = "Change in total volume of benefit per capta",
       y = "Change in Vote Share") +  # Add axis labels
  ggtitle("Scatterplot with Regression Line")  # Add title
print(scatterplot3)

  #Oposição
    #1 turno
painel_1turno <- painel %>%
  filter(ano == 2018 | ano == 2022) %>%
  filter(turno == 1)
rm(painel)
regressao <- lm(vote_share_esquerda ~ proporcao_beneficiarios, data = painel_1turno)
stargazer(regressao, type = "text")
regressao_monetaria <- lm(vote_share_esquerda ~ log_PARCELA_per_capta, 
                          data = painel_1turno)
stargazer(regressao_monetaria, type = "text")
regressao2 <- lm(vote_share_esquerda ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio)
                 , data = painel_1turno)
summary(regressao2)
regressao_monetaria_2 <- lm(vote_share_esquerda ~ log_PARCELA_per_capta + factor(ano) + 
                              factor(id_municipio)
                            , data = painel_1turno)
summary(regressao_monetaria_2)
media_proporcao_16 <- mean(painel_1turno$proporcao_16, na.rm = T)
painel_1turno$proporcao_16[is.na(painel_1turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
regressao3 <- lm(vote_share_esquerda ~ proporcao_beneficiarios + factor(ano) + 
                   factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                   proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                 , data = painel_1turno)
summary(regressao3)
regressao_monetaria_3 <- lm(vote_share_esquerda ~ log_PARCELA_per_capta + factor(ano) + 
                              factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                              proporcao_divorciados + proporcao_viuvos + proporcao_16 + 
                              proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                              proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                              proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                              proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                              proporcao_fundamental_completo + proporcao_medio_incompleto + 
                              proporcao_medio_completo + proporcao_superior_incompleto + 
                              proporcao_deficientes
                            , data = painel_1turno)
summary(regressao_monetaria_3)
      #2 turno
painel_2turno <- painel %>%
  filter(ano == 2018 | ano == 2022) %>%
  filter(turno == 2)
rm(painel)
regressao_2_turno <- lm(vote_share_esquerda ~ proporcao_beneficiarios, data = painel_2turno)
stargazer(regressao_2_turno, type = "text")
regressao_monetaria_2_turno <- lm(vote_share_esquerda ~ log_PARCELA_per_capta, 
                                  data = painel_2turno)
stargazer(regressao_monetaria_2_turno, type = "text")
regressao2_turno_2 <- lm(vote_share_esquerda ~ proporcao_beneficiarios + factor(ano) + 
                           factor(id_municipio)
                         , data = painel_2turno)
summary(regressao2_turno_2)
regressao_monetaria_2_turno_2 <- lm(vote_share_esquerda ~ log_PARCELA_per_capta + 
                                      factor(ano) + factor(id_municipio)
                                    , data = painel_2turno)
summary(regressao_monetaria_2_turno_2)
media_proporcao_16 <- mean(painel_2turno$proporcao_16, na.rm = T)
painel_2turno$proporcao_16[is.na(painel_2turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
regressao3_turno_2 <- lm(vote_share_esquerda ~ proporcao_beneficiarios + factor(ano) + 
                           factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                           proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                           proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                           proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                           proporcao_analfabeto + proporcao_le_e_escreve + 
                           proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                           proporcao_medio_incompleto + proporcao_medio_completo + 
                           proporcao_superior_incompleto + proporcao_deficientes
                         , data = painel_2turno)
summary(regressao3_turno_2)
regressao_monetaria_2_turno_3 <- lm(vote_share_esquerda ~ log_PARCELA_per_capta + factor(ano) + 
                                      factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                                      proporcao_divorciados + proporcao_viuvos + proporcao_16 + 
                                      proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                                      proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                                      proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                                      proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                                      proporcao_fundamental_completo + proporcao_medio_incompleto + 
                                      proporcao_medio_completo + proporcao_superior_incompleto + 
                                      proporcao_deficientes
                                    , data = painel_2turno)
summary(regressao_monetaria_2_turno_3)
#Regressões robustez
  #Robustez interação uf
    #1 turno
painel_uf <- painel %>%
  filter(ano == 2018 | ano == 2022) %>%
  filter(turno == 1)
rm(painel)
regressao_uf <- lm(vote_share_direita ~ proporcao_beneficiarios + sigla_uf*factor(ano)
                   , data = painel_uf)
stargazer(regressao_uf, type = "text")
regressao_monetaria_uf <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                               sigla_uf*factor(ano), 
                             data = painel_uf)
stargazer(regressao_monetaria_uf, type = "text")
regressao2_uf <- lm(vote_share_direita ~ proporcao_beneficiarios + sigla_uf*factor(ano) + 
                      factor(id_municipio)
                 , data = painel_uf)
summary(regressao2_uf)
regressao_monetaria_2_uf <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                 sigla_uf*factor(ano) + factor(id_municipio)
                            , data = painel_uf)
summary(regressao_monetaria_2_uf)
sum(is.na(painel_uf$proporcao_16))
painel_uf$proporcao_mulheres <- as.numeric(painel_uf$prop)
painel_uf$proporcao_casados <- as.numeric(painel_uf$proporcao_16)
painel_uf$proporcao_divorciados <- as.numeric(painel_uf$proporcao_16)
painel_uf$proporcao_viuvos <- as.numeric(painel_uf$proporcao_16)
painel_uf$proporcao_16 <- as.numeric(painel_uf$proporcao_16)
painel_uf$proporcao_17 <- as.numeric(painel_uf$proporcao_17)
painel_uf$proporcao_18_20 <- as.numeric(painel_uf$proporcao_18_20)
painel_uf$proporcao_21_24 <- as.numeric(painel_uf$proporcao_21_24)
painel_uf$proporcao_25_29 <- as.numeric(painel_uf$proporcao_25_29)
painel_uf$proporcao_35_44 <- as.numeric(painel_uf$proporcao_35_44)
painel_uf$proporcao_45_59 <- as.numeric(painel_uf$proporcao_45_59)
painel_uf$proporcao_60_69 <- as.numeric(painel_uf$proporcao_60_69)
painel_uf$proporcao_70_79 <- as.numeric(painel_uf$proporcao_70_79)
painel_uf$proporcao_analfabeto <- as.numeric(painel_uf$proporcao_analfabeto)
painel_uf$proporcao_le_e_escreve <- as.numeric(painel_uf$proporcao_le_e_escreve)
painel_uf$proporcao_fundamental_incompleto <- as.numeric(painel_uf$proporcao_fundamental_incompleto)
painel_uf$proporcao_fundamental_completo <- as.numeric(painel_uf$proporcao_fundamental_completo)
painel_uf$proporcao_medio_incompleto <- as.numeric(painel_uf$proporcao_medio_incompleto)
painel_uf$proporcao_medio_completo <- as.numeric(painel_uf$proporcao_medio_completo)
painel_uf$proporcao_superior_incompleto <- as.numeric(painel_uf$proporcao_superior_incompleto)
painel_uf$proporcao_deficientes <- as.numeric(painel_uf$proporcao_deficientes)
media_proporcao_16 <- mean(painel_uf$proporcao_16, na.rm = T)
painel_uf$proporcao_16[is.na(painel_uf$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
regressao3_uf <- lm(vote_share_direita ~ proporcao_beneficiarios + sigla_uf*factor(ano) + 
                   factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                   proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                   proporcao_18_20 + proporcao_21_24 + proporcao_25_29 + proporcao_35_44 + 
                   proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                   proporcao_analfabeto + proporcao_le_e_escreve + 
                   proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                   proporcao_medio_incompleto + proporcao_medio_completo + 
                   proporcao_superior_incompleto + proporcao_deficientes
                   , data = painel_uf)
summary(regressao3_uf)
regressao3_monetaria_uf <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                sigla_uf*factor(ano) + factor(id_municipio) + 
                                proporcao_mulheres + proporcao_casados + 
                                proporcao_divorciados + proporcao_viuvos + proporcao_16 + 
                                proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                                proporcao_25_29 + proporcao_35_44 + proporcao_45_59 + 
                                proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                                proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                                proporcao_fundamental_completo + 
                                proporcao_medio_incompleto + proporcao_medio_completo + 
                                proporcao_superior_incompleto + proporcao_deficientes
                              , data = painel_uf)
summary(regressao3_monetaria_uf)
    #2 Turno
painel_uf_2turno <- painel %>%
  filter(ano == 2018 | ano == 2022) %>%
  filter(turno == 2)
regressao_uf_2turno <- lm(vote_share_direita ~ proporcao_beneficiarios + sigla_uf*factor(ano)
                   , data = painel_uf_2turno)
stargazer(regressao_uf_2turno, type = "text")
regressao_monetaria_uf_2turno <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                               sigla_uf*factor(ano), 
                             data = painel_uf_2turno)
stargazer(regressao_monetaria_uf_2turno, type = "text")
regressao2_uf_2turno <- lm(vote_share_direita ~ proporcao_beneficiarios + sigla_uf*factor(ano) + 
                      factor(id_municipio)
                    , data = painel_uf_2turno)
summary(regressao2_uf_2turno)
regressao_monetaria_2_uf_2turno <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                 sigla_uf*factor(ano) + factor(id_municipio)
                               , data = painel_uf_2turno)
summary(regressao_monetaria_2_uf_2turno)
sum(is.na(painel_uf_2turno$proporcao_deficientes))
painel_uf_2turno$proporcao_mulheres <- as.numeric(painel_uf_2turno$proporcao_16)
painel_uf_2turno$proporcao_casados <- as.numeric(painel_uf_2turno$proporcao_16)
painel_uf_2turno$proporcao_divorciados <- as.numeric(painel_uf_2turno$proporcao_16)
painel_uf_2turno$proporcao_viuvos <- as.numeric(painel_uf_2turno$proporcao_16)
painel_uf_2turno$proporcao_16 <- as.numeric(painel_uf_2turno$proporcao_16)
painel_uf_2turno$proporcao_17 <- as.numeric(painel_uf_2turno$proporcao_17)
painel_uf_2turno$proporcao_18_20 <- as.numeric(painel_uf_2turno$proporcao_18_20)
painel_uf_2turno$proporcao_21_24 <- as.numeric(painel_uf_2turno$proporcao_21_24)
painel_uf_2turno$proporcao_25_34 <- as.numeric(painel_uf_2turno$proporcao_25_34)
painel_uf_2turno$proporcao_35_44 <- as.numeric(painel_uf_2turno$proporcao_35_44)
painel_uf_2turno$proporcao_45_59 <- as.numeric(painel_uf_2turno$proporcao_45_59)
painel_uf_2turno$proporcao_60_69 <- as.numeric(painel_uf_2turno$proporcao_60_69)
painel_uf_2turno$proporcao_70_79 <- as.numeric(painel_uf_2turno$proporcao_70_79)
painel_uf_2turno$proporcao_analfabeto <- as.numeric(painel_uf_2turno$proporcao_analfabeto)
painel_uf_2turno$proporcao_le_e_escreve <- as.numeric(painel_uf_2turno$proporcao_le_e_escreve)
painel_uf_2turno$proporcao_fundamental_incompleto <- as.numeric(painel_uf_2turno$proporcao_fundamental_incompleto)
painel_uf_2turno$proporcao_fundamental_completo <- as.numeric(painel_uf_2turno$proporcao_fundamental_completo)
painel_uf_2turno$proporcao_medio_incompleto <- as.numeric(painel_uf_2turno$proporcao_medio_incompleto)
painel_uf_2turno$proporcao_medio_completo <- as.numeric(painel_uf_2turno$proporcao_medio_completo)
painel_uf_2turno$proporcao_superior_incompleto <- as.numeric(painel_uf_2turno$proporcao_superior_incompleto)
painel_uf_2turno$proporcao_deficientes <- as.numeric(painel_uf_2turno$proporcao_deficientes)
media_proporcao_16 <- mean(painel_uf_2turno$proporcao_16, na.rm = T)
painel_uf_2turno$proporcao_16[is.na(painel_uf_2turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
regressao3_uf_2turno <- lm(vote_share_direita ~ proporcao_beneficiarios + 
                        sigla_uf*factor(ano) + factor(id_municipio) + proporcao_mulheres + 
                        proporcao_casados + proporcao_divorciados + proporcao_viuvos + 
                        proporcao_16 + proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                        proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                        proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                        proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                        proporcao_fundamental_completo + proporcao_medio_incompleto + 
                        proporcao_medio_completo + proporcao_superior_incompleto + 
                        proporcao_deficientes
                    , data = painel_uf_2turno)
summary(regressao3_uf_2turno)
regressao3_monetaria_3_uf_2turno <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                sigla_uf*factor(ano) + factor(id_municipio) + 
                                proporcao_mulheres + proporcao_casados + 
                                proporcao_divorciados + proporcao_viuvos + proporcao_16 + 
                                proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                                proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                                proporcao_60_69 + proporcao_70_79 + proporcao_analfabeto + 
                                proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                                proporcao_fundamental_completo + 
                                proporcao_medio_incompleto + proporcao_medio_completo + 
                                proporcao_superior_incompleto + proporcao_deficientes
                              , data = painel_uf_2turno)
summary(regressao3_monetaria_3_uf_2turno)
#Robustez pre-trend
#Direita
#1 turno
painel_robustez_1turno <- painel %>%
  filter(ano == 2014 | ano == 2018 | ano == 2022) %>%
  filter(turno == 1)
painel_2018_2022 <- painel_robustez_1turno[,c(1:27)]
painel_2018_2022 <- painel_2018_2022 %>%
  filter(ano == 2018 | ano == 2022)
colnames(painel_2018_2022)[1] <- "ano_proporcao_beneficiarios"
painel_2014_2018 <- painel_robustez_1turno[,c(1, 4, 28:74)]
painel_2014_2018 <- painel_2014_2018 %>%
  filter(ano == 2014 | ano == 2018)
painel_2014_2018$ano_proporcao_beneficiarios <- ifelse(painel_2014_2018$ano == 2014, 2018, 2022)
painel_2014_2018 <- painel_2014_2018[c(1, 50, 2:49)]
colnames(painel_2014_2018)[1] <- "ano_variaveis_controle"
painel_2014_2018$ano_proporcao_beneficiarios <- as.integer(painel_2014_2018$ano_proporcao_beneficiarios)
painel_robustez_1turno <- left_join(painel_2018_2022, painel_2014_2018, by = c("id_municipio", "ano_proporcao_beneficiarios"))
painel_robustez_1turno <- painel_robustez_1turno[, c(1, 28, 2:27, 29:75)]
regressao2_robustez <- lm(vote_share_direita ~ proporcao_beneficiarios + 
                            factor(ano_variaveis_controle) + factor(id_municipio)
                          , data = painel_robustez_1turno)
summary(regressao2_robustez)
regressao_monetaria_2_robustez <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                factor(ano_variaveis_controle) + factor(id_municipio)
                                , data = painel_robustez_1turno)
summary(regressao_monetaria_2_robustez)
regressao3_robustez <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano_variaveis_controle) + 
                            factor(id_municipio) + proporcao_mulheres + proporcao_16 + 
                            proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                            proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                            proporcao_60_69 + proporcao_70_79  + proporcao_analfabeto + 
                            proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                            proporcao_fundamental_completo + proporcao_medio_incompleto +
                            proporcao_medio_completo + proporcao_superior_incompleto
                          , data = painel_robustez_1turno)
summary(regressao3_robustez)
regressao_monetaria_3_robustez <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                      factor(ano_variaveis_controle) + factor(id_municipio) + 
                                      proporcao_mulheres + proporcao_16 + proporcao_17 + 
                                      proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + 
                                      proporcao_35_44 + proporcao_45_59 + proporcao_60_69 + 
                                      proporcao_70_79 + proporcao_analfabeto + 
                                      proporcao_le_e_escreve + 
                                      proporcao_fundamental_incompleto + 
                                      proporcao_fundamental_completo + 
                                      proporcao_medio_incompleto + 
                                      proporcao_medio_completo + 
                                      proporcao_superior_incompleto
                                    , data = painel_robustez_1turno)
summary(regressao3_monetaria_robustez)
#2 turno
painel_robustez_2turno <- painel %>%
  filter(ano == 2014 | ano == 2018 | ano == 2022) %>%
  filter(turno == 2)
painel_2018_2022 <- painel_robustez_2turno[,c(1:27)]
painel_2018_2022 <- painel_2018_2022 %>%
  filter(ano == 2018 | ano == 2022)
colnames(painel_2018_2022)[1] <- "ano_proporcao_beneficiarios"
painel_2014_2018 <- painel_robustez_2turno[,c(1, 4, 28:74)]
painel_2014_2018 <- painel_2014_2018 %>%
  filter(ano == 2014 | ano == 2018)
painel_2014_2018$ano_proporcao_beneficiarios <- ifelse(painel_2014_2018$ano == 2014, 2018, 2022)
painel_2014_2018 <- painel_2014_2018[c(1, 50, 2:49)]
colnames(painel_2014_2018)[1] <- "ano_variaveis_controle"
painel_2014_2018$ano_proporcao_beneficiarios <- as.integer(painel_2014_2018$ano_proporcao_beneficiarios)
painel_robustez_2turno <- left_join(painel_2018_2022, painel_2014_2018, by = c("id_municipio", "ano_proporcao_beneficiarios"))
painel_robustez_2turno <- painel_robustez_2turno[, c(1, 28, 2:27, 29:75)]
regressao2_pretrend_2_turno <- lm(vote_share_direita ~ proporcao_beneficiarios + 
                                    factor(ano_variaveis_controle) + factor(id_municipio)
                                  , data = painel_robustez_2turno)
summary(regressao2_pretrend_2_turno)
regressao_monetaria_2_pretrend_2turno <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                              factor(ano_variaveis_controle) + factor(id_municipio)
                                            , data = painel_robustez_2turno)
summary(regressao_monetaria_2_pretrend_2turno)
regressao3_pretrend_2turno <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano_variaveis_controle) + 
                                   factor(id_municipio) + proporcao_mulheres + proporcao_16 + 
                                   proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                                   proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                                   proporcao_60_69 + proporcao_70_79  + proporcao_analfabeto + 
                                   proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                                   proporcao_fundamental_completo + proporcao_medio_incompleto +
                                   proporcao_medio_completo + proporcao_superior_incompleto
                                 , data = painel_robustez_2turno)
summary(regressao3_pretrend_2turno)
regressao_monetaria_3_pretrend_2turno <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                              factor(ano_variaveis_controle) + factor(id_municipio) + 
                                              proporcao_mulheres + proporcao_16 + proporcao_17 + 
                                              proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + 
                                              proporcao_35_44 + proporcao_45_59 + proporcao_60_69 + 
                                              proporcao_70_79 + proporcao_analfabeto + 
                                              proporcao_le_e_escreve + 
                                              proporcao_fundamental_incompleto + 
                                              proporcao_fundamental_completo + 
                                              proporcao_medio_incompleto + 
                                              proporcao_medio_completo + 
                                              proporcao_superior_incompleto
                                            , data = painel_robustez_2turno)
summary(regressao_monetaria_3_pretrend_2turno)
# Pre-trend tratamento mediana
  #1 turno
painel_robustez_1turno <- painel %>%
  filter(ano == 2014 | ano == 2018 | ano == 2022) %>%
  filter(turno == 1)
painel_2018_2022 <- painel_robustez_1turno[,c(1:27)]
painel_2018_2022 <- painel_2018_2022 %>%
  filter(ano == 2018 | ano == 2022)
colnames(painel_2018_2022)[1] <- "ano_proporcao_beneficiarios"
painel_2014_2018 <- painel_robustez_1turno[,c(1, 4, 28:74)]
painel_2014_2018 <- painel_2014_2018 %>%
  filter(ano == 2014 | ano == 2018)
painel_2014_2018$ano_proporcao_beneficiarios <- ifelse(painel_2014_2018$ano == 2014, 2018, 2022)
painel_2014_2018 <- painel_2014_2018[c(1, 50, 2:49)]
colnames(painel_2014_2018)[1] <- "ano_variaveis_controle"
painel_2014_2018$ano_proporcao_beneficiarios <- as.integer(painel_2014_2018$ano_proporcao_beneficiarios)
painel_robustez_1turno <- left_join(painel_2018_2022, painel_2014_2018, by = c("id_municipio", "ano_proporcao_beneficiarios"))
painel_robustez_1turno <- painel_robustez_1turno[, c(1, 28, 2:27, 29:75)]
painel_robustez_1turno$acima_mediana <- ifelse(painel_robustez_1turno$proporcao_beneficiarios >
                                                 median(painel_robustez_1turno$proporcao_beneficiarios), 1, 0)
table(painel_robustez_1turno$acima_mediana)
regressao2_robustez <- lm(vote_share_direita ~ acima_mediana + 
                            factor(ano_variaveis_controle) + factor(id_municipio)
                          , data = painel_robustez_1turno)
summary(regressao2_robustez)
regressao3_robustez <- lm(vote_share_direita ~ acima_mediana + factor(ano_variaveis_controle) + 
                            factor(id_municipio) + proporcao_mulheres + proporcao_16 + 
                            proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                            proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                            proporcao_60_69 + proporcao_70_79  + proporcao_analfabeto + 
                            proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                            proporcao_fundamental_completo + proporcao_medio_incompleto +
                            proporcao_medio_completo + proporcao_superior_incompleto
                          , data = painel_robustez_1turno)
summary(regressao3_robustez)
  #2 turno
painel_robustez_2turno <- painel %>%
  filter(ano == 2014 | ano == 2018 | ano == 2022) %>%
  filter(turno == 2)
painel_2018_2022 <- painel_robustez_2turno[,c(1:27)]
painel_2018_2022 <- painel_2018_2022 %>%
  filter(ano == 2018 | ano == 2022)
colnames(painel_2018_2022)[1] <- "ano_proporcao_beneficiarios"
painel_2014_2018 <- painel_robustez_2turno[,c(1, 4, 28:74)]
painel_2014_2018 <- painel_2014_2018 %>%
  filter(ano == 2014 | ano == 2018)
painel_2014_2018$ano_proporcao_beneficiarios <- ifelse(painel_2014_2018$ano == 2014, 2018, 2022)
painel_2014_2018 <- painel_2014_2018[c(1, 50, 2:49)]
colnames(painel_2014_2018)[1] <- "ano_variaveis_controle"
painel_2014_2018$ano_proporcao_beneficiarios <- as.integer(painel_2014_2018$ano_proporcao_beneficiarios)
painel_robustez_2turno <- left_join(painel_2018_2022, painel_2014_2018, by = c("id_municipio", "ano_proporcao_beneficiarios"))
painel_robustez_2turno <- painel_robustez_2turno[, c(1, 28, 2:27, 29:75)]
painel_robustez_2turno$acima_mediana <- ifelse(painel_robustez_2turno$proporcao_beneficiarios >
                                                 median(painel_robustez_2turno$proporcao_beneficiarios), 1, 0)
table(painel_robustez_2turno$acima_mediana)
regressao2_robustez <- lm(vote_share_direita ~ acima_mediana + 
                            factor(ano_variaveis_controle) + factor(id_municipio)
                          , data = painel_robustez_2turno)
summary(regressao2_robustez)
regressao3_robustez <- lm(vote_share_direita ~ acima_mediana + factor(ano_variaveis_controle) + 
                            factor(id_municipio) + proporcao_mulheres + proporcao_16 + 
                            proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                            proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                            proporcao_60_69 + proporcao_70_79  + proporcao_analfabeto + 
                            proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                            proporcao_fundamental_completo + proporcao_medio_incompleto +
                            proporcao_medio_completo + proporcao_superior_incompleto
                          , data = painel_robustez_2turno)
summary(regressao3_robustez)
#Robustez método principal com anos anteriores
  #Direita
    #1 turno
painel_robustez_1turno <- painel %>%
  filter(ano == 2014 | ano == 2018)
rm(painel)
painel_robustez_1turno <- painel_robustez_1turno %>%
  filter(turno == 1)
regressao_robustez <- lm(vote_share_direita ~ proporcao_beneficiarios, data = painel_robustez_1turno)
stargazer(regressao_robustez, type = "text")
regressao_monetaria_robustez <- lm(vote_share_direita ~ log_PARCELA_per_capta, data = painel_robustez_1turno)
stargazer(regressao_monetaria_robustez, type = "text")
regressao2_robustez <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + factor(id_municipio)
                          , data = painel_robustez_1turno)
summary(regressao2_robustez)
regressao_monetaria_2_robustez <- lm(vote_share_direita ~ log_PARCELA_per_capta + factor(ano) + factor(id_municipio)
                                     , data = painel_robustez_1turno)
summary(regressao_monetaria_2_robustez)
painel_robustez_1turno$proporcao_mulheres <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_casados <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_divorciados <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_viuvos <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_16 <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_17 <- as.numeric(painel_robustez_1turno$proporcao_17)
painel_robustez_1turno$proporcao_18_20 <- as.numeric(painel_robustez_1turno$proporcao_18_20)
painel_robustez_1turno$proporcao_21_24 <- as.numeric(painel_robustez_1turno$proporcao_21_24)
painel_robustez_1turno$proporcao_25_29 <- as.numeric(painel_robustez_1turno$proporcao_25_29)
painel_robustez_1turno$proporcao_35_44 <- as.numeric(painel_robustez_1turno$proporcao_35_44)
painel_robustez_1turno$proporcao_45_59 <- as.numeric(painel_robustez_1turno$proporcao_45_59)
painel_robustez_1turno$proporcao_60_69 <- as.numeric(painel_robustez_1turno$proporcao_60_69)
painel_robustez_1turno$proporcao_70_79 <- as.numeric(painel_robustez_1turno$proporcao_70_79)
painel_robustez_1turno$proporcao_analfabeto <- as.numeric(painel_robustez_1turno$proporcao_analfabeto)
painel_robustez_1turno$proporcao_le_e_escreve <- as.numeric(painel_robustez_1turno$proporcao_le_e_escreve)
painel_robustez_1turno$proporcao_fundamental_incompleto <- as.numeric(painel_robustez_1turno$proporcao_fundamental_incompleto)
painel_robustez_1turno$proporcao_fundamental_completo <- as.numeric(painel_robustez_1turno$proporcao_fundamental_completo)
painel_robustez_1turno$proporcao_medio_incompleto <- as.numeric(painel_robustez_1turno$proporcao_medio_incompleto)
painel_robustez_1turno$proporcao_medio_completo <- as.numeric(painel_robustez_1turno$proporcao_medio_completo)
painel_robustez_1turno$proporcao_superior_incompleto <- as.numeric(painel_robustez_1turno$proporcao_superior_incompleto)
painel_robustez_1turno$proporcao_deficientes <- as.numeric(painel_robustez_1turno$proporcao_deficientes)
sum(is.na(painel_robustez_1turno$proporcao_superior_incompleto))
media_proporcao_mulheres <- mean(painel_robustez_1turno$proporcao_mulheres, na.rm = T)
media_proporcao_16 <- mean(painel_robustez_1turno$proporcao_16, na.rm = T)
painel_robustez_1turno$proporcao_mulheres[is.na(painel_robustez_1turno$proporcao_mulheres)] <- media_proporcao_mulheres
painel_robustez_1turno$proporcao_16[is.na(painel_robustez_1turno$proporcao_16)] <- media_proporcao_16
regressao3_robustez <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + 
                            factor(id_municipio) + proporcao_mulheres + proporcao_16 + 
                            proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                            proporcao_25_29 + proporcao_35_44 + proporcao_45_59 + 
                            proporcao_60_69 + proporcao_70_79  + proporcao_analfabeto + 
                            proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                            proporcao_fundamental_completo + proporcao_medio_incompleto +
                            proporcao_medio_completo + proporcao_superior_incompleto
                            , data = painel_robustez_1turno)
summary(regressao3_robustez)
regressao3_monetaria_robustez <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                      factor(ano) + factor(id_municipio) + 
                                      proporcao_mulheres + proporcao_16 + proporcao_17 + 
                                      proporcao_18_20 + proporcao_21_24 + proporcao_25_29 + 
                                      proporcao_35_44 + proporcao_45_59 + proporcao_60_69 + 
                                      proporcao_70_79 + proporcao_analfabeto + 
                                      proporcao_le_e_escreve + 
                                      proporcao_fundamental_incompleto + 
                                      proporcao_fundamental_completo + 
                                      proporcao_medio_incompleto + 
                                      proporcao_medio_completo + 
                                      proporcao_superior_incompleto
                                    , data = painel_robustez_1turno)
summary(regressao3_monetaria_robustez)
#2 turno
painel_robustez_2turno <- painel %>%
  filter(ano == 2014 | ano == 2018) %>%
  filter(turno == 2)
rm(painel)
regressao_pretrend_2turno <- lm(vote_share_direita ~ proporcao_beneficiarios, 
                                data = painel_robustez_2turno)
stargazer(regressao_pretrend_2turno, type = "text")
regressao_monetaria_pretrend_2turno <- lm(vote_share_direita ~ log_PARCELA_per_capta, 
                                   data = painel_robustez_2turno)
stargazer(regressao_monetaria_pretrend_2turno, type = "text")
regressao2_pretrend_2_turno <- lm(vote_share_direita ~ proporcao_beneficiarios + 
                               factor(ano) + factor(id_municipio)
                          , data = painel_robustez_2turno)
summary(regressao2_pretrend_2_turno)
regressao_monetaria_2_pretrend_2turno <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                              factor(ano) + factor(id_municipio)
                                     , data = painel_robustez_2turno)
summary(regressao_monetaria_2_pretrend_2turno)
painel_robustez_2turno$proporcao_mulheres <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_casados <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_divorciados <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_viuvos <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_16 <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_17 <- as.numeric(painel_robustez_2turno$proporcao_17)
painel_robustez_2turno$proporcao_18_20 <- as.numeric(painel_robustez_2turno$proporcao_18_20)
painel_robustez_2turno$proporcao_21_24 <- as.numeric(painel_robustez_2turno$proporcao_21_24)
painel_robustez_2turno$proporcao_25_34 <- as.numeric(painel_robustez_2turno$proporcao_25_34)
painel_robustez_2turno$proporcao_35_44 <- as.numeric(painel_robustez_2turno$proporcao_35_44)
painel_robustez_2turno$proporcao_45_59 <- as.numeric(painel_robustez_2turno$proporcao_45_59)
painel_robustez_2turno$proporcao_60_69 <- as.numeric(painel_robustez_2turno$proporcao_60_69)
painel_robustez_2turno$proporcao_70_79 <- as.numeric(painel_robustez_2turno$proporcao_70_79)
painel_robustez_2turno$proporcao_analfabeto <- as.numeric(painel_robustez_2turno$proporcao_analfabeto)
painel_robustez_2turno$proporcao_le_e_escreve <- as.numeric(painel_robustez_2turno$proporcao_le_e_escreve)
painel_robustez_2turno$proporcao_fundamental_incompleto <- as.numeric(painel_robustez_2turno$proporcao_fundamental_incompleto)
painel_robustez_2turno$proporcao_fundamental_completo <- as.numeric(painel_robustez_2turno$proporcao_fundamental_completo)
painel_robustez_2turno$proporcao_medio_incompleto <- as.numeric(painel_robustez_2turno$proporcao_medio_incompleto)
painel_robustez_2turno$proporcao_medio_completo <- as.numeric(painel_robustez_2turno$proporcao_medio_completo)
painel_robustez_2turno$proporcao_superior_incompleto <- as.numeric(painel_robustez_2turno$proporcao_superior_incompleto)
painel_robustez_2turno$proporcao_deficientes <- as.numeric(painel_robustez_2turno$proporcao_deficientes)
sum(is.na(painel_robustez_2turno$proporcao_deficientes))
media_proporcao_mulheres <- mean(painel_robustez_2turno$proporcao_mulheres, na.rm = T)
media_proporcao_16 <- mean(painel_robustez_2turno$proporcao_16, na.rm = T)
painel_robustez_2turno$proporcao_mulheres[is.na(painel_robustez_2turno$proporcao_mulheres)] <- media_proporcao_mulheres
painel_robustez_2turno$proporcao_16[is.na(painel_robustez_2turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
rm(media_proporcao_mulheres)
regressao3_pretrend_2turno <- lm(vote_share_direita ~ proporcao_beneficiarios + factor(ano) + 
                            factor(id_municipio) + proporcao_mulheres + proporcao_16 + 
                            proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                            proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                            proporcao_60_69 + proporcao_70_79  + proporcao_analfabeto + 
                            proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                            proporcao_fundamental_completo + proporcao_medio_incompleto +
                            proporcao_medio_completo + proporcao_superior_incompleto
                          , data = painel_robustez_2turno)
summary(regressao3_pretrend_2turno)
regressao_monetaria_3_pretrend_2turno <- lm(vote_share_direita ~ log_PARCELA_per_capta + 
                                      factor(ano) + factor(id_municipio) + 
                                      proporcao_mulheres + proporcao_16 + proporcao_17 + 
                                      proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + 
                                      proporcao_35_44 + proporcao_45_59 + proporcao_60_69 + 
                                      proporcao_70_79 + proporcao_analfabeto + 
                                      proporcao_le_e_escreve + 
                                      proporcao_fundamental_incompleto + 
                                      proporcao_fundamental_completo + 
                                      proporcao_medio_incompleto + 
                                      proporcao_medio_completo + 
                                      proporcao_superior_incompleto
                                    , data = painel_robustez_2turno)
summary(regressao_monetaria_3_pretrend_2turno)
  #Esquerda
#1 turno
painel_robustez_1turno <- painel %>%
  filter(ano == 2014 | ano == 2018) %>%
  filter(turno == 1)
  rm(painel)
regressao_robustez <- lm(vote_share_esquerda ~ proporcao_beneficiarios, 
                         data = painel_robustez_1turno)
stargazer(regressao_robustez, type = "text")
regressao_monetaria_robustez <- lm(vote_share_esquerda ~ log_PARCELA_per_capta, 
                                   data = painel_robustez_1turno)
stargazer(regressao_monetaria_robustez, type = "text")
regressao2_robustez <- lm(vote_share_esquerda ~ proporcao_beneficiarios + factor(ano) + 
                            factor(id_municipio), data = painel_robustez_1turno)
summary(regressao2_robustez)
regressao_monetaria_2_robustez <- lm(vote_share_esquerda ~ log_PARCELA_per_capta + 
                        factor(ano) + factor(id_municipio), data = painel_robustez_1turno)
summary(regressao_monetaria_2_robustez)
painel_robustez_1turno$proporcao_mulheres <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_casados <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_divorciados <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_viuvos <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_16 <- as.numeric(painel_robustez_1turno$proporcao_16)
painel_robustez_1turno$proporcao_17 <- as.numeric(painel_robustez_1turno$proporcao_17)
painel_robustez_1turno$proporcao_18_20 <- as.numeric(painel_robustez_1turno$proporcao_18_20)
painel_robustez_1turno$proporcao_21_24 <- as.numeric(painel_robustez_1turno$proporcao_21_24)
painel_robustez_1turno$proporcao_25_34 <- as.numeric(painel_robustez_1turno$proporcao_25_34)
painel_robustez_1turno$proporcao_35_44 <- as.numeric(painel_robustez_1turno$proporcao_35_44)
painel_robustez_1turno$proporcao_45_59 <- as.numeric(painel_robustez_1turno$proporcao_45_59)
painel_robustez_1turno$proporcao_60_69 <- as.numeric(painel_robustez_1turno$proporcao_60_69)
painel_robustez_1turno$proporcao_70_79 <- as.numeric(painel_robustez_1turno$proporcao_70_79)
painel_robustez_1turno$proporcao_analfabeto <- as.numeric(painel_robustez_1turno$proporcao_analfabeto)
painel_robustez_1turno$proporcao_le_e_escreve <- as.numeric(painel_robustez_1turno$proporcao_le_e_escreve)
painel_robustez_1turno$proporcao_fundamental_incompleto <- as.numeric(painel_robustez_1turno$proporcao_fundamental_incompleto)
painel_robustez_1turno$proporcao_fundamental_completo <- as.numeric(painel_robustez_1turno$proporcao_fundamental_completo)
painel_robustez_1turno$proporcao_medio_incompleto <- as.numeric(painel_robustez_1turno$proporcao_medio_incompleto)
painel_robustez_1turno$proporcao_medio_completo <- as.numeric(painel_robustez_1turno$proporcao_medio_completo)
painel_robustez_1turno$proporcao_superior_incompleto <- as.numeric(painel_robustez_1turno$proporcao_superior_incompleto)
painel_robustez_1turno$proporcao_deficientes <- as.numeric(painel_robustez_1turno$proporcao_deficientes)
sum(is.na(painel_robustez_1turno$proporcao_superior_incompleto))
media_proporcao_mulheres <- mean(painel_robustez_1turno$proporcao_mulheres, na.rm = T)
media_proporcao_16 <- mean(painel_robustez_1turno$proporcao_16, na.rm = T)
painel_robustez_1turno$proporcao_mulheres[is.na(painel_robustez_1turno$proporcao_mulheres)] <- media_proporcao_mulheres
painel_robustez_1turno$proporcao_16[is.na(painel_robustez_1turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
rm(media_proporcao_mulheres)
regressao3_robustez <- lm(vote_share_esquerda ~ proporcao_beneficiarios + factor(ano) + 
                            factor(id_municipio) + proporcao_mulheres + proporcao_16 + 
                            proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                            proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                            proporcao_60_69 + proporcao_70_79  + proporcao_analfabeto + 
                            proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                            proporcao_fundamental_completo + proporcao_medio_incompleto +
                            proporcao_medio_completo + proporcao_superior_incompleto
                          , data = painel_robustez_1turno)
summary(regressao3_robustez)
regressao_monetaria3_robustez <- lm(vote_share_esquerda ~ log_PARCELA_per_capta + 
                                      factor(ano) + factor(id_municipio) + 
                                      proporcao_mulheres + proporcao_16 + proporcao_17 + 
                                      proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + 
                                      proporcao_35_44 + proporcao_45_59 + proporcao_60_69 + 
                                      proporcao_70_79 + proporcao_analfabeto + 
                                      proporcao_le_e_escreve + 
                                      proporcao_fundamental_incompleto + 
                                      proporcao_fundamental_completo + 
                                      proporcao_medio_incompleto + 
                                      proporcao_medio_completo + 
                                      proporcao_superior_incompleto
                                    , data = painel_robustez_1turno)
summary(regressao_monetaria3_robustez)
#2 turno
painel_robustez_2turno <- painel %>%
  filter(ano == 2014 | ano == 2018) %>%
  filter(turno == 2)
rm(painel)
regressao_pretrend_2turno <- lm(vote_share_esquerda ~ proporcao_beneficiarios, 
                                data = painel_robustez_2turno)
stargazer(regressao_pretrend_2turno, type = "text")
regressao_monetaria_pretrend_2turno <- lm(vote_share_esquerda ~ log_PARCELA_per_capta, 
                                          data = painel_robustez_2turno)
stargazer(regressao_monetaria_pretrend_2turno, type = "text")
regressao2_pretrend_2_turno <- lm(vote_share_esquerda ~ proporcao_beneficiarios + 
                                    factor(ano) + factor(id_municipio)
                                  , data = painel_robustez_2turno)
summary(regressao2_pretrend_2_turno)
regressao_monetaria_2_pretrend_2turno <- lm(vote_share_esquerda ~ log_PARCELA_per_capta + 
                                              factor(ano) + factor(id_municipio)
                                            , data = painel_robustez_2turno)
summary(regressao_monetaria_2_pretrend_2turno)
painel_robustez_2turno$proporcao_mulheres <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_casados <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_divorciados <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_viuvos <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_16 <- as.numeric(painel_robustez_2turno$proporcao_16)
painel_robustez_2turno$proporcao_17 <- as.numeric(painel_robustez_2turno$proporcao_17)
painel_robustez_2turno$proporcao_18_20 <- as.numeric(painel_robustez_2turno$proporcao_18_20)
painel_robustez_2turno$proporcao_21_24 <- as.numeric(painel_robustez_2turno$proporcao_21_24)
painel_robustez_2turno$proporcao_25_34 <- as.numeric(painel_robustez_2turno$proporcao_25_34)
painel_robustez_2turno$proporcao_35_44 <- as.numeric(painel_robustez_2turno$proporcao_35_44)
painel_robustez_2turno$proporcao_45_59 <- as.numeric(painel_robustez_2turno$proporcao_45_59)
painel_robustez_2turno$proporcao_60_69 <- as.numeric(painel_robustez_2turno$proporcao_60_69)
painel_robustez_2turno$proporcao_70_79 <- as.numeric(painel_robustez_2turno$proporcao_70_79)
painel_robustez_2turno$proporcao_analfabeto <- as.numeric(painel_robustez_2turno$proporcao_analfabeto)
painel_robustez_2turno$proporcao_le_e_escreve <- as.numeric(painel_robustez_2turno$proporcao_le_e_escreve)
painel_robustez_2turno$proporcao_fundamental_incompleto <- as.numeric(painel_robustez_2turno$proporcao_fundamental_incompleto)
painel_robustez_2turno$proporcao_fundamental_completo <- as.numeric(painel_robustez_2turno$proporcao_fundamental_completo)
painel_robustez_2turno$proporcao_medio_incompleto <- as.numeric(painel_robustez_2turno$proporcao_medio_incompleto)
painel_robustez_2turno$proporcao_medio_completo <- as.numeric(painel_robustez_2turno$proporcao_medio_completo)
painel_robustez_2turno$proporcao_superior_incompleto <- as.numeric(painel_robustez_2turno$proporcao_superior_incompleto)
painel_robustez_2turno$proporcao_deficientes <- as.numeric(painel_robustez_2turno$proporcao_deficientes)
sum(is.na(painel_robustez_2turno$proporcao_deficientes))
media_proporcao_mulheres <- mean(painel_robustez_2turno$proporcao_mulheres, na.rm = T)
media_proporcao_16 <- mean(painel_robustez_2turno$proporcao_16, na.rm = T)
painel_robustez_2turno$proporcao_mulheres[is.na(painel_robustez_2turno$proporcao_mulheres)] <- media_proporcao_mulheres
painel_robustez_2turno$proporcao_16[is.na(painel_robustez_2turno$proporcao_16)] <- media_proporcao_16
rm(media_proporcao_16)
rm(media_proporcao_mulheres)
regressao3_pretrend_2turno <- lm(vote_share_esquerda ~ proporcao_beneficiarios + 
                              factor(ano) + factor(id_municipio) + proporcao_mulheres + proporcao_16 + 
                              proporcao_17 + proporcao_18_20 + proporcao_21_24 + 
                              proporcao_25_34 + proporcao_35_44 + proporcao_45_59 + 
                              proporcao_60_69 + proporcao_70_79  + proporcao_analfabeto + 
                              proporcao_le_e_escreve + proporcao_fundamental_incompleto + 
                              proporcao_fundamental_completo + proporcao_medio_incompleto +
                              proporcao_medio_completo + proporcao_superior_incompleto
                                 , data = painel_robustez_2turno)
summary(regressao3_pretrend_2turno)
regressao_monetaria_3_pretrend_2turno <- lm(vote_share_esquerda ~ log_PARCELA_per_capta + 
                                              factor(ano) + factor(id_municipio) + 
                                              proporcao_mulheres + proporcao_16 + proporcao_17 + 
                                              proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + 
                                              proporcao_35_44 + proporcao_45_59 + proporcao_60_69 + 
                                              proporcao_70_79 + proporcao_analfabeto + 
                                              proporcao_le_e_escreve + 
                                              proporcao_fundamental_incompleto + 
                                              proporcao_fundamental_completo + 
                                              proporcao_medio_incompleto + 
                                              proporcao_medio_completo + 
                                              proporcao_superior_incompleto
                                            , data = painel_robustez_2turno)
summary(regressao_monetaria_3_pretrend_2turno)
#Mediana
  #1 turno
painel_1turno <- painel %>%
  filter(turno == 1) %>%
  filter(ano == 2018 | ano == 2022)
painel_1turno$acima_mediana <- ifelse(painel_1turno$proporcao_beneficiarios >
                                      median(painel_1turno$proporcao_beneficiarios), 1, 0)
table(painel_1turno$acima_mediana)
median(painel_1turno$proporcao_beneficiarios)
regressao_mediana_1 <- lm(vote_share_direita ~ acima_mediana, data = painel_1turno)
summary(regressao_mediana_1)
regressao_mediana_2 <- lm(vote_share_direita ~ acima_mediana + factor(ano) + 
                            factor(id_municipio), data = painel_1turno)
summary(regressao_mediana_2)
regressao_mediana_3 <- lm(vote_share_direita ~ acima_mediana + factor(ano) + 
                  factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                  proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                  proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                  proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                  proporcao_analfabeto + proporcao_le_e_escreve + 
                  proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                  proporcao_medio_incompleto + proporcao_medio_completo + 
                  proporcao_superior_incompleto + proporcao_deficientes
                  , data = painel_1turno)
summary(regressao_mediana_3)
#2 turno
painel_2turno <- painel %>%
  filter(turno == 2) %>%
  filter(ano == 2018 | ano == 2022)
painel_2turno$acima_mediana <- ifelse(painel_2turno$proporcao_beneficiarios >
                                      median(painel_2turno$proporcao_beneficiarios), 1, 0)
regressao_mediana_1 <- lm(vote_share_direita ~ acima_mediana, data = painel_2turno)
summary(regressao_mediana_1)
regressao_mediana_2 <- lm(vote_share_direita ~ acima_mediana + factor(ano) + 
                            factor(id_municipio), data = painel_2turno)
summary(regressao_mediana_2)
regressao_mediana_3 <- lm(vote_share_direita ~ acima_mediana + factor(ano) + 
                            factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                            proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                            proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                            proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                            proporcao_analfabeto + proporcao_le_e_escreve + 
                            proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                            proporcao_medio_incompleto + proporcao_medio_completo + 
                            proporcao_superior_incompleto + proporcao_deficientes
                            , data = painel_2turno)
summary(regressao_mediana_3)
#1 quartil
  #1 turno
painel_1turno <- painel %>%
  filter(turno == 1) %>%
  filter(ano == 2018 | ano == 2022)
painel_1turno$acima_1quartil <- ifelse(painel_1turno$proporcao_beneficiarios >
                              quantile(painel_1turno$proporcao_beneficiarios, 0.75), 1, 0)
table(painel_1turno$acima_1quartil)
regressao_1quartil_1 <- lm(vote_share_direita ~ acima_1quartil, data = painel_1turno)
summary(regressao_1quartil_1)
regressao_1quartil_2 <- lm(vote_share_direita ~ acima_1quartil + factor(ano) + 
                            factor(id_municipio), data = painel_1turno)
summary(regressao_1quartil_2)
regressao_1quartil_3 <- lm(vote_share_direita ~ acima_1quartil + factor(ano) + 
                  factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                  proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                  proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                  proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                  proporcao_analfabeto + proporcao_le_e_escreve + 
                  proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                  proporcao_medio_incompleto + proporcao_medio_completo + 
                  proporcao_superior_incompleto + proporcao_deficientes
                  , data = painel_1turno)
summary(regressao_1quartil_3)
  #2 turno
painel_2turno <- painel %>%
  filter(turno == 2) %>%
  filter(ano == 2018 | ano == 2022)
painel_2turno$acima_1quartil <- ifelse(painel_2turno$proporcao_beneficiarios >
                              quantile(painel_2turno$proporcao_beneficiarios, 0.75), 1, 0)
table(painel_2turno$acima_1quartil)
regressao_1quartil_1 <- lm(vote_share_direita ~ acima_1quartil, data = painel_2turno)
summary(regressao_1quartil_1)
regressao_1quartil_2 <- lm(vote_share_direita ~ acima_1quartil + factor(ano) + 
                            factor(id_municipio), data = painel_2turno)
summary(regressao_1quartil_2)
regressao_1quartil_3 <- lm(vote_share_direita ~ acima_1quartil + factor(ano) + 
                  factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                  proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                  proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                  proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                  proporcao_analfabeto + proporcao_le_e_escreve + 
                  proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                  proporcao_medio_incompleto + proporcao_medio_completo + 
                  proporcao_superior_incompleto + proporcao_deficientes
                  , data = painel_2turno)
summary(regressao_1quartil_3)
#1 decil
  #1 turno
painel_1turno <- painel %>%
  filter(turno == 1) %>%
  filter(ano == 2018 | ano == 2022)
painel_1turno$acima_1decil <- ifelse(painel_1turno$proporcao_beneficiarios >
                                quantile(painel_1turno$proporcao_beneficiarios, 0.9), 1, 0)
table(painel_1turno$acima_1decil)
regressao_1decil_1 <- lm(vote_share_direita ~ acima_1decil, data = painel_1turno)
summary(regressao_1decil_1)
regressao_1decil_2 <- lm(vote_share_direita ~ acima_1decil + factor(ano) + 
                             factor(id_municipio), data = painel_1turno)
summary(regressao_1decil_2)
regressao_1decil_3 <- lm(vote_share_direita ~ acima_1decil + factor(ano) + 
                             factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                             proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                             proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                             proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                             proporcao_analfabeto + proporcao_le_e_escreve + 
                             proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                             proporcao_medio_incompleto + proporcao_medio_completo + 
                             proporcao_superior_incompleto + proporcao_deficientes
                           , data = painel_1turno)
summary(regressao_1decil_3)
#2 turno
painel_2turno <- painel %>%
  filter(turno == 2) %>%
  filter(ano == 2018 | ano == 2022)
painel_2turno$acima_1decil <- ifelse(painel_2turno$proporcao_beneficiarios >
                               quantile(painel_2turno$proporcao_beneficiarios, 0.9), 1, 0)
table(painel_2turno$acima_1decil)
regressao_1decil_1 <- lm(vote_share_direita ~ acima_1decil, data = painel_2turno)
summary(regressao_1decil_1)
regressao_1decil_2 <- lm(vote_share_direita ~ acima_1decil + factor(ano) + 
                            factor(id_municipio), data = painel_2turno)
summary(regressao_1decil_2)
regressao_1decil_3 <- lm(vote_share_direita ~ acima_1decil + factor(ano) + 
                             factor(id_municipio) + proporcao_mulheres + proporcao_casados + 
                             proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                             proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                             proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                             proporcao_analfabeto + proporcao_le_e_escreve + 
                             proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                             proporcao_medio_incompleto + proporcao_medio_completo + 
                             proporcao_superior_incompleto + proporcao_deficientes
                           , data = painel_2turno)
summary(regressao_1decil_3)
#Quartis
  #1 turno
painel_1turno <- painel %>%
  filter(turno == 1) %>%
  filter(ano == 2018 | ano == 2022)
painel_1turno$acima_1quartil <- ifelse(painel_1turno$proporcao_beneficiarios >
                               quantile(painel_1turno$proporcao_beneficiarios, 0.25) &
                                 painel_1turno$proporcao_beneficiarios <=
                                 median(painel_1turno$proporcao_beneficiarios) , 1, 0)
painel_1turno$acima_2quartil <- ifelse(painel_1turno$proporcao_beneficiarios >
                              median(painel_1turno$proporcao_beneficiarios) &
                              painel_1turno$proporcao_beneficiarios <=
                              quantile(painel_1turno$proporcao_beneficiarios, 0.75) , 1, 0)
painel_1turno$acima_3quartil <- ifelse(painel_1turno$proporcao_beneficiarios >
                              quantile(painel_1turno$proporcao_beneficiarios, 0.75) , 1, 0)
table(painel_1turno$acima_1quartil)
table(painel_1turno$acima_2quartil)
table(painel_1turno$acima_3quartil)
regressao_quartil_1 <- lm(vote_share_direita ~ acima_1quartil + acima_2quartil + 
                             acima_3quartil, data = painel_1turno)
summary(regressao_quartil_1)
regressao_quartil_2 <- lm(vote_share_direita ~ acima_1quartil + acima_2quartil + 
                             acima_3quartil + factor(ano) + factor(id_municipio),
                           data = painel_1turno)
summary(regressao_quartil_2)
regressao_quartil_3 <- lm(vote_share_direita ~ acima_1quartil + acima_2quartil + 
                             acima_3quartil + factor(ano) + factor(id_municipio) + 
                             proporcao_mulheres + proporcao_casados + 
                             proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                             proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                             proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                             proporcao_analfabeto + proporcao_le_e_escreve + 
                             proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                             proporcao_medio_incompleto + proporcao_medio_completo + 
                             proporcao_superior_incompleto + proporcao_deficientes
                           , data = painel_1turno)
summary(regressao_quartil_3)
#2 turno
painel_2turno <- painel %>%
  filter(turno == 2) %>%
  filter(ano == 2018 | ano == 2022)
painel_2turno$acima_1quartil <- ifelse(painel_2turno$proporcao_beneficiarios >
                                         quantile(painel_2turno$proporcao_beneficiarios, 0.25) &
                                         painel_2turno$proporcao_beneficiarios <=
                                         median(painel_2turno$proporcao_beneficiarios) , 1, 0)
painel_2turno$acima_2quartil <- ifelse(painel_2turno$proporcao_beneficiarios >
                                         median(painel_2turno$proporcao_beneficiarios) &
                                         painel_2turno$proporcao_beneficiarios <=
                                         quantile(painel_2turno$proporcao_beneficiarios, 0.75) , 1, 0)
painel_2turno$acima_3quartil <- ifelse(painel_2turno$proporcao_beneficiarios >
                                         quantile(painel_2turno$proporcao_beneficiarios, 0.75) , 1, 0)
table(painel_2turno$acima_1quartil)
table(painel_2turno$acima_2quartil)
table(painel_2turno$acima_3quartil)
regressao_quartil_1 <- lm(vote_share_direita ~ acima_1quartil + acima_2quartil + 
                            acima_3quartil, data = painel_2turno)
summary(regressao_quartil_1)
regressao_quartil_2 <- lm(vote_share_direita ~ acima_1quartil + acima_2quartil + 
                            acima_3quartil + factor(ano) + factor(id_municipio),
                          data = painel_2turno)
summary(regressao_quartil_2)
regressao_quartil_3 <- lm(vote_share_direita ~ acima_1quartil + acima_2quartil + 
                            acima_3quartil + factor(ano) + factor(id_municipio) + 
                            proporcao_mulheres + proporcao_casados + 
                            proporcao_divorciados + proporcao_viuvos + proporcao_16 + proporcao_17 + 
                            proporcao_18_20 + proporcao_21_24 + proporcao_25_34 + proporcao_35_44 + 
                            proporcao_45_59 + proporcao_60_69 + proporcao_70_79 + 
                            proporcao_analfabeto + proporcao_le_e_escreve + 
                            proporcao_fundamental_incompleto + proporcao_fundamental_completo + 
                            proporcao_medio_incompleto + proporcao_medio_completo + 
                            proporcao_superior_incompleto + proporcao_deficientes
                          , data = painel_2turno)
summary(regressao_quartil_3)





setwd("C:\\Users\\lucas\\OneDrive\\Documentos\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Cadastro Único\\Famílias")
familias_2012 <- read.csv("cad201212_familias.csv", sep = ";", fileEncoding = "latin1")
setwd("C:\\Users\\lucas\\OneDrive\\Documentos\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Cadastro Único\\Pessoas1")
setwd("C:\\Users\\lucas\\OneDrive\\Documentos\\Profissional\\Faculdade\\Mestrado\\Dissertação\\Dados\\Cadastro Único\\Pessoas2")
pessoas_2012 <- read.csv("cad201212_pessoas.csv", sep = ";", fileEncoding = "latin1")
dados_2012 <- left_join(pessoas_2012, familias_2012, by = "cod_familiar_fam")
dados_2017 <- read.csv("cad201712_familias.csv", sep = ";", fileEncoding = "latin1", nrows = 1000)
dados_2018 <- read.csv("cad201812_pessoas.csv", sep = ";", fileEncoding = "latin1", nrows = 1000)
dados_2022 <- read.csv("cad202212_familias.csv", sep = ";", fileEncoding = "latin1", nrows = 1000)
dados_2022 <- read.csv("cad202212_pessoas.csv", sep = ";", fileEncoding = "latin1", nrows = 1000)


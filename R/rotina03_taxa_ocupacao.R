library(tidyverse)
library(ggforce) #PACOTE PARA COLOCAR FACET_WRAP EM VARIAS PAGINAS (FACET_WRAP_PAGINATE)
library(factoextra) #PACOTE PARA TECNICA DE CLUSTER TIPO K-MEANS
library(cluster)
library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)
library(maptools)
#library(tmap)
#library(sf) #PACOTE PARA CONSTRUCAO DE MAPAS UTLIZANDO O TIDYVERSE
library(geobr)
library(pdftables)
#library(ggspatial)
#library(raster) #PACOTE PARA MAPAS - IMAGENS DE SATELITE COM INFORMACOES TABELADAS
#library(rlang)
set.seed(123)

#setwd("C:/Users/lucas.eneas/OneDrive - MINISTERIO DA JUSTIÇA/analise_dados/bases")
setwd("C:/Users/55619/OneDrive - MINISTERIO DA JUSTIÇA/analise_dados/bases")
base_sisdepen <- read.csv2("base_sisdepen.csv")

cores <- c("#294661","#7F312F","#808080","#B8860B","#5E3B56","#5F9EA0","#808000", 
           "#A0522D","#F5DEB3","#FF9900","#8B008B","#5F6B6D","#FB8281","#F4D25A",
           "#7F898A","#A4DDEE","#FDAB89","#B687AC","#28738A","#A78F8F","#168980") #VETOR DE CORES PARA OS GRAFICOS

base_taxa <- base_sisdepen %>%
  filter(
    A.Situação.de.Preenchimento == "Validado",
    A.Ano == "2020",
    A.Referencia == "Dezembro"
  )%>%
  mutate(
    `Ano` = A.Ano,
    `Referência` = A.Referencia,
    `Situação de Preenchimento` = A.Situação.de.Preenchimento,
    `UF` = A.UF,
    `Município` = A.Município,
    `Código Mun.IBGE` = A.Código.IBGE,
    `Nome do Estabelecimento` = A.Nome.do.Estabelecimento,
    `Atividades Educacionais-Responde` = A.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Existem.pessoas.privadas.de.liberdade.neste.estabelecimento.em.atividades.educacionais.,
   
    `Tipo de Custódia`=ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime semiaberto)"), ignore_case = TRUE)))==TRUE,"Semiaberto",
                        ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime fechado)"), ignore_case = TRUE)))==TRUE,"Fechado",
                         ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime aberto)"), ignore_case = TRUE)))==TRUE,"Aberto",
                          ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(diversos tipos de regime)"), ignore_case = TRUE)))==TRUE,"Diversos",
                           ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(presos provisórios)"), ignore_case = TRUE)))==TRUE,"Provisórios",
                            ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(medida de segurança)"), ignore_case = TRUE)))==TRUE,"Medida de Segurança",
                             ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(patronato)"), ignore_case = TRUE)))==TRUE,"Patronato",
                              "Outros"))))))),
    
    `Unidade Monitoramento` = ifelse((str_detect(A.Nome.do.Estabelecimento,regex(c("(monitoramento)|(monitora(ç|c)(ã|a)o)|(eletr(ô|o)nic(o|a))"),ignore_case=TRUE)))==TRUE,"Monitoramento Eletrônico","Não"),
    `Unidade Domiciliar` = ifelse((str_detect(A.Nome.do.Estabelecimento,regex(c("(Domiciliar)"),ignore_case=TRUE)))==TRUE,"Domiciliar","Não"),
    `Unidade Monitoramento - Domiciliar` = ifelse(`Unidade Monitoramento` == "Monitoramento Eletrônico" & `Unidade Domiciliar` == "Domiciliar", "Mon.Eletrônico e Domiciliar","Não"),
    `Tipo de Unidade` = ifelse(`Unidade Monitoramento - Domiciliar` ==  "Mon.Eletrônico e Domiciliar", "Mon.Eletrônico e Domiciliar",
                         ifelse((str_detect(A.Nome.do.Estabelecimento,regex(c("(monitoramento)|(monitora(ç|c)(ã|a)o)|(eletr(ô|o)nic(o|a))"),ignore_case=TRUE)))==TRUE,"Monitoramento Eletrônico",
                          ifelse((str_detect(A.Nome.do.Estabelecimento,regex(c("(Domiciliar)"),ignore_case=TRUE)))==TRUE,"Domiciliar",  
                           "Comum")))
  )%>%
  group_by(
    `Ano`,
    `Referência`,
    `Situação de Preenchimento`,
    `UF`,
    `Município`,
    `Código Mun.IBGE`,
    `Nome do Estabelecimento`,
    `Tipo de Custódia`,
    `Unidade Monitoramento`,
    `Unidade Domiciliar`,
    `Unidade Monitoramento - Domiciliar`,
    `Tipo de Unidade`,
    `Atividades Educacionais-Responde`
  )%>%
  summarise(
    `Capacidade-Provisório-Masculino`   = sum(M.1.3.Capacidade.do.estabelecimento...Presos.provisórios...Masculino,na.rm = TRUE),
    `Capacidade-Provisório-Feminino`    = sum(M.1.3.Capacidade.do.estabelecimento...Presos.provisórios...Feminino,na.rm = TRUE),
    `Capacidade-Provisório-Total`       = sum(`Capacidade-Provisório-Masculino`,na.rm = TRUE)+sum(`Capacidade-Provisório-Feminino`,na.rm = TRUE),
    
    `Capacidade-Fechado-Masculino`      = sum(M.1.3.Capacidade.do.estabelecimento...Regime.fechado...Masculino,na.rm = TRUE),
    `Capacidade-Fechado-Feminino`       = sum(M.1.3.Capacidade.do.estabelecimento...Regime.fechado...Feminino,na.rm = TRUE),
    `Capacidade-Fechado-Total`          = sum(`Capacidade-Fechado-Masculino`,na.rm = TRUE)+sum(`Capacidade-Fechado-Feminino`,na.rm = TRUE),
    
    `Capacidade-Semiaberto-Masculino`   = sum(M.1.3.Capacidade.do.estabelecimento...Regime.semiaberto...Masculino,na.rm = TRUE),
    `Capacidade-Semiaberto-Feminino`    = sum(M.1.3.Capacidade.do.estabelecimento...Regime.semiaberto...Feminino,na.rm = TRUE),
    `Capacidade-Semiaberto-Total`       = sum(`Capacidade-Semiaberto-Masculino`,na.rm = TRUE)+sum(`Capacidade-Semiaberto-Feminino`,na.rm = TRUE),
    
    `Capacidade-Aberto-Masculino`       = sum(M.1.3.Capacidade.do.estabelecimento...Regime.aberto...Masculino,na.rm = TRUE),
    `Capacidade-Aberto-Feminino`        = sum(M.1.3.Capacidade.do.estabelecimento...Regime.aberto...Feminino,na.rm = TRUE),
    `Capacidade-Aberto-Total`           = sum(`Capacidade-Aberto-Masculino`,na.rm = TRUE)+sum(`Capacidade-Aberto-Feminino`,na.rm = TRUE),
    
    `Capacidade-Med.Segurança-Masculino`= sum(M.1.3.Capacidade.do.estabelecimento...Medidas.de.segurança.de.internação...Masculino,na.rm = TRUE),
    `Capacidade-Med.Segurança-Feminino` = sum(M.1.3.Capacidade.do.estabelecimento...Medidas.de.segurança.de.internação...Feminino,na.rm = TRUE),
    `Capacidade-Med.Segurança-Total`    = sum(`Capacidade-Med.Segurança-Masculino`,na.rm = TRUE)+sum(`Capacidade-Med.Segurança-Feminino`,na.rm = TRUE),
    
    `Capacidade-Outros-Masculino`       = sum(M.1.3.Capacidade.do.estabelecimento...Outro.s...Qual.is.....Masculino,na.rm = TRUE),
    `Capacidade-Outros-Feminino`        = sum(M.1.3.Capacidade.do.estabelecimento...Outro.s...Qual.is.....Feminino,na.rm = TRUE),
    `Capacidade-Outros-Total`           = sum(`Capacidade-Outros-Masculino`,na.rm = TRUE)+sum(`Capacidade-Outros-Feminino`,na.rm = TRUE),
    
    `Capacidade Total-Masculino`        = sum(M.1.3.Capacidade.do.estabelecimento...Masculino...Total,na.rm = TRUE), 
    `Capacidade Total-Feminino`         = sum(M.1.3.Capacidade.do.estabelecimento...Feminino...Total,na.rm = TRUE),
    `Capacidade Total`                  = sum(`Capacidade Total-Masculino`,na.rm=TRUE) + sum(`Capacidade Total-Feminino`,na.rm=TRUE),
    
    
    `População Prisional-Provisório-Masculino` = sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Justiça.Federal.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Provisório-Feminino`  = sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Justiça.Federal.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Provisório-Total`     = sum(`População Prisional-Provisório-Masculino`,na.rm=TRUE) + sum(`População Prisional-Provisório-Feminino`,na.rm=TRUE),
    
    `População Prisional-Fechado-Masculino`    = sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Fechado-Feminino`     = sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Fechado-Total`        = sum(`População Prisional-Fechado-Masculino`,na.rm = TRUE)+sum(`População Prisional-Fechado-Feminino`,na.rm = TRUE),
    
    `População Prisional-Semiaberto-Masculino` = sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Semiaberto-Feminino`  = sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Semiaberto-Total`     = sum(`População Prisional-Semiaberto-Masculino`,na.rm = TRUE)+sum(`População Prisional-Semiaberto-Feminino`,na.rm = TRUE),
    
    `População Prisional-Aberto-Masculino`     = sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Aberto-Feminino`      = sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Aberto-Total`         = sum(`População Prisional-Aberto-Masculino`,na.rm = TRUE)+sum(`População Prisional-Aberto-Feminino`,na.rm = TRUE),
    
    `População Prisional-Med.Segurança-Masculino`= sum(M.4.1.População.prisional...Medida.de.segurança...internação...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                   sum(M.4.1.População.prisional...Medida.de.segurança...internação...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                   sum(M.4.1.População.prisional...Medida.de.segurança...internação...Outros.Just..Trab...cível..Masculino, na.rm = TRUE)+
                                                   sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                   sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                   sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Med.Segurança-Feminino`= sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                  sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                  sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Outros.Just..Trab...cível..Feminino, na.rm = TRUE)+
                                                  sum(M.4.1.População.prisional...Medida.de.segurança...internação...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                  sum(M.4.1.População.prisional...Medida.de.segurança...internação...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                  sum(M.4.1.População.prisional...Medida.de.segurança...internação...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Med.Segurança-Total`   = sum(`População Prisional-Med.Segurança-Masculino`,na.rm = TRUE)+sum(`População Prisional-Med.Segurança-Feminino`,na.rm = TRUE),
    
    `População Prisional Total` = sum(M.4.1.População.prisional...Total),
    
    `População Movimentada` = sum(`População Prisional Total`,na.rm = TRUE)+
                              sum(M.4.5.Movimentação.no.Sistema.Prisional..total.do.período.de.referência....Entradas...Número.de.inclusões.originárias...Total,na.rm=TRUE)+
                              sum(M.4.5.Movimentação.no.Sistema.Prisional..total.do.período.de.referência....Transferências.remoções...Número.de.inclusões.por.transferências.ou.remoções...Total,na.rm=TRUE),
                              
    
    `Agravos Transmissíveis` = sum(M.6.7.Quantidade.de.pessoas.com.agravos.transmissíveis.na.data.de.fim.do.período.de.referência...Hepatite...Total,na.rm=TRUE)+
                               sum(M.6.7.Quantidade.de.pessoas.com.agravos.transmissíveis.na.data.de.fim.do.período.de.referência...HIV...Total,na.rm=TRUE)+
                               sum(M.6.7.Quantidade.de.pessoas.com.agravos.transmissíveis.na.data.de.fim.do.período.de.referência...Outros...Total,na.rm=TRUE)+
                               sum(M.6.7.Quantidade.de.pessoas.com.agravos.transmissíveis.na.data.de.fim.do.período.de.referência...Sífilis...Total,na.rm=TRUE)+
                               sum(M.6.7.Quantidade.de.pessoas.com.agravos.transmissíveis.na.data.de.fim.do.período.de.referência...Tuberculose...Total,na.rm=TRUE),
    
    `Consultas Médicas Interna e Externas`= sum(M.6.6.Informações.da.área.de.saúde...total.do.período...Consultas.médicas.realizadas.externamente...Total,na.rm=TRUE)+
                                            sum(M.6.6.Informações.da.área.de.saúde...total.do.período...Consultas.médicas.realizadas.no.estabelecimento...Total),
    
    `Atividades Educacionais` = sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Alfabetização...Total,na.rm=TRUE)+
                                sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Ensino.Fundamental...Total,na.rm=TRUE)+
                                sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Ensino.Médio...Total,na.rm=TRUE)+
                                sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Ensino.Superior...Total,na.rm=TRUE)+
                                sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Curso.Técnico..acima.de.800.horas.de.aula....Total,na.rm=TRUE)+
                                sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Curso.de.Formação.Inicial.e.Continuada..Capacitação.Profissional..acima.de.160.horas.de.aula....Total,na.rm=TRUE)+
                                sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Pessoas.matriculadas.em.programa.de.remição.pelo.estudo.através.da.leitura...Total,na.rm=TRUE)+
                                sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Pessoas.matriculadas.em.programa.de.remição.pelo.estudo.através.do.esporte...Total,na.rm=TRUE)+
                                sum(M.6.3.Quantidade.de.pessoas.privadas.de.liberdade.em.atividade.educacional...Pessoas.envolvidas.em.atividades.educacionais.complementares..videoteca..atividades.de.lazer..cultura....Total),
    
    
    `Consultas Médicas Interna e Externas-Responde` = ifelse(`Consultas Médicas Interna e Externas`>0, "Sim", "Não"),
    
    `Agravos Transmissíveis-Responde`=ifelse(`Agravos Transmissíveis`>0, "Sim","Não"),
    
    `Trabalho externo` = sum(M.6.1.Quantidade.de.pessoas.privadas.de.liberdade.em.programas.de.laborterapia...Trabalho.externo.Masculino, na.rm = TRUE)+
                         sum(M.6.1.Quantidade.de.pessoas.privadas.de.liberdade.em.programas.de.laborterapia...Trabalho.externo.Feminino, na.rm = TRUE),
    
    `Trabalho interno`=  sum(M.6.1.Quantidade.de.pessoas.privadas.de.liberdade.em.programas.de.laborterapia...Trabalho.interno.Masculino, na.rm = TRUE)+
                         sum(M.6.1.Quantidade.de.pessoas.privadas.de.liberdade.em.programas.de.laborterapia...Trabalho.interno.Feminino,na.rm = TRUE),
    
    `Trabalho`= `Trabalho externo` + `Trabalho interno`        
    
  )

#######################################################################################################################
#RELATORIO 01 - TRATA DA TAXA DE OCUPACAO EM TODOS OS ESTABELECIMENTOS PRISIONAIS INDIVIDUALMENTE
rel1_ocupacaoGeral <- 
  base_taxa %>%
  group_by(
    Ano,
    Referência,
    UF,
    `Nome do Estabelecimento`,
    `Tipo de Custódia`,
    `Tipo de Unidade`,
    `População Prisional Total`,
    `Capacidade Total`
  )%>%
  summarise(
    `Taxa de Ocupação Geral` = round(sum(`População Prisional Total`) / sum(`Capacidade Total`) * 100, digits = 2),
    `Taxa Agravos Transmissíveis` = round(sum(`Agravos Transmissíveis`,na.rm = TRUE) / sum(`População Movimentada`,na.rm = TRUE)*100, digits = 2),
    `Taxa Consultas Médicas`=round(sum(`Consultas Médicas Interna e Externas`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Atividades Educacionais`= round(sum(`Atividades Educacionais`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho externo` = round(sum(`Trabalho externo`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho interno` = round(sum(`Trabalho interno`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho` = round(sum(`Trabalho`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2)
  )

#######################################################################################################################  
# ANALISE DA CAPACIDADE VERSUS POPULACAO PRISIONAL DESCONSIDERANDO O MONITORAMENTO E PRISAO DOMICILIAR
# AJUSTADAS AS UNIDADES DE SEMIABERTO E ABERTO COM PRESOS FORA DO ESTABELECIMENTO
# NESSE CASO, CONSIDERAMOS APENAS AS UNIDADES QUE POSSUEM PRESOS FISICAMENTE

rel2_ocupacaoComum <- 
  base_taxa %>%
  filter(
    `Tipo de Unidade` == "Comum"
  )%>%
  group_by(
    Ano,
    Referência,
    UF,
    `Nome do Estabelecimento`,
    `Tipo de Custódia`,
    `Tipo de Unidade`,
    `População Prisional Total`,
    `Capacidade Total`
  )%>%
  summarise(
    `Taxa de Ocupação Geral` = round(sum(`População Prisional Total`) / sum(`Capacidade Total`) * 100, digits = 2),
    `Taxa de Ocupação-Fechado e Provisório` = round((sum(`População Prisional-Provisório-Total`)+sum(`População Prisional-Fechado-Total`))/
                                                   (sum(`Capacidade-Provisório-Total`)+sum(`Capacidade-Fechado-Total`))*100, digits = 2),
    `Taxa Agravos Transmissíveis` = round(sum(`Agravos Transmissíveis`,na.rm = TRUE) / sum(`População Movimentada`,na.rm = TRUE)*100, digits = 2),
    `Taxa Consultas Médicas` = round(sum(`Consultas Médicas Interna e Externas`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Atividades Educacionais`= round(sum(`Atividades Educacionais`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho externo` = round(sum(`Trabalho externo`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho interno` = round(sum(`Trabalho interno`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho` = round(sum(`Trabalho`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2)
  )

rel3_ocupacaoRegime <-
  base_taxa %>%
  group_by(
    Ano,
    Referência,
    #UF,
    #`Nome do Estabelecimento`,
    `Tipo de Custódia`
    #`Tipo de Unidade`,
    #`População Prisional Total`,
    #`Capacidade Total`
  )%>%
  summarise(
    `Taxa de Ocupação por Regime` = round(sum(`População Prisional Total`) / sum(`Capacidade Total`) * 100, digits = 2),
    `Taxa Agravos Transmissíveis` = round(sum(`Agravos Transmissíveis`,na.rm = TRUE) / sum(`População Prisional Total`,na.rm = TRUE)*100, digits = 2),
    `Taxa Consultas Médicas`=round(sum(`Consultas Médicas Interna e Externas`,na.rm = TRUE)/sum(`População Prisional Total`,na.rm = TRUE)*100,digits = 2),
    `Taxa Atividades Educacionais`=round(sum(`Atividades Educacionais`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho externo` = round(sum(`Trabalho externo`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho interno` = round(sum(`Trabalho interno`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho` = round(sum(`Trabalho`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2)
  )

rel4_ocupacaoFechadoDiversosProvisorios <-
  base_taxa %>%
  filter(
    `Tipo de Custódia` %in% c("Fechado","Diversos","Provisórios")
  )%>%
  group_by(
    Ano,
    Referência
    #UF,
    #`Nome do Estabelecimento`,
    #`Tipo de Custódia`
    #`Tipo de Unidade`,
    #`População Prisional Total`,
    #`Capacidade Total`
  )%>%
  summarise(
    `Taxa de Ocupação por Regimes Fechado Diversos Provisórios` = round(sum(`População Prisional Total`) / sum(`Capacidade Total`) * 100, digits = 2),
    `Taxa Agravos Transmissíveis` = round(sum(`Agravos Transmissíveis`,na.rm = TRUE) / sum(`População Prisional Total`,na.rm = TRUE)*100, digits = 2),
    `Taxa Consultas Médicas`=round(sum(`Consultas Médicas Interna e Externas`,na.rm = TRUE)/sum(`População Prisional Total`,na.rm = TRUE)*100,digits = 2),
    `Taxa Atividades Educacionais`=round(sum(`Atividades Educacionais`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho externo` = round(sum(`Trabalho externo`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho interno` = round(sum(`Trabalho interno`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho` = round(sum(`Trabalho`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2)
  )

rel5_ocupacaoDiversos <-
  base_taxa %>%
  filter(
    `Tipo de Custódia` %in% c("Fechado","Diversos","Provisórios")
  )%>%
  group_by(
    Ano,
    Referência
  )%>%
  summarise(
    `Taxa Ocupação Fechado`  = round(sum(`População Prisional-Fechado-Total`,na.rm = TRUE) / sum(`Capacidade-Fechado-Total`,na.rm = TRUE)*100,digits = 2),
    `Taxa Ocupação Provisórios` = round(sum(`População Prisional-Provisório-Total`,na.rm = TRUE) / sum(`Capacidade-Provisório-Total`,na.rm = TRUE)*100,digits = 2),
    `Taxa Ocupação Medida de Segurança` = round(sum(`População Prisional-Med.Segurança-Total`,na.rm = TRUE)/sum(`Capacidade-Med.Segurança-Total`,na.rm = TRUE)*100, digits = 2),
    `Taxa Agravos Transmissíveis` = round(sum(`Agravos Transmissíveis`,na.rm = TRUE) / sum(`População Prisional Total`,na.rm = TRUE)*100, digits = 2),
    `Taxa Consultas Médicas`=round(sum(`Consultas Médicas Interna e Externas`,na.rm = TRUE)/sum(`População Prisional Total`,na.rm = TRUE)*100,digits = 2),
    `Taxa Atividades Educacionais`=round(sum(`Atividades Educacionais`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho externo` = round(sum(`Trabalho externo`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho interno` = round(sum(`Trabalho interno`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho` = round(sum(`Trabalho`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2)
  )

rel6_ocupacaoFechadoProvisoriosMedida <- 
  base_taxa %>%
  filter(
    `Tipo de Unidade` == "Comum",
    `Tipo de Custódia` %in% c("Fechado")
  )%>%
  group_by(
    Ano,
    Referência,
    UF,
    `Nome do Estabelecimento`,
    `Tipo de Custódia`,
    `Tipo de Unidade`,
    `População Prisional Total`,
    `Capacidade Total`
  )%>%
  summarise(
    `Taxa de Ocupação Geral` = round(sum(`População Prisional Total`) / sum(`Capacidade Total`) * 100, digits = 2),
    `Taxa de Ocupação-Fechado e Provisório`= round((sum(`População Prisional-Provisório-Total`)+sum(`População Prisional-Fechado-Total`))/
                                                   (sum(`Capacidade-Provisório-Total`)+sum(`Capacidade-Fechado-Total`))*100, digits = 2),
    `Taxa Agravos Transmissíveis` = round(sum(`Agravos Transmissíveis`,na.rm = TRUE) / sum(`População Prisional Total`,na.rm = TRUE)*100, digits = 2),
    `Taxa Consultas Médicas`=round(sum(`Consultas Médicas Interna e Externas`,na.rm = TRUE)/sum(`População Prisional Total`,na.rm = TRUE)*100,digits = 2),
    `Taxa Atividades Educacionais`=round(sum(`Atividades Educacionais`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho externo` = round(sum(`Trabalho externo`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho interno` = round(sum(`Trabalho interno`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho` = round(sum(`Trabalho`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2)
  )

graf6.1_dispersao <-
  ggplot(data = rel6_ocupacaoFechadoProvisoriosMedida)+
  geom_point(mapping = aes(x=`Capacidade Total`,y=`População Prisional Total`, color = `Tipo de Custódia`))+
  geom_line(mapping = aes(x=`Capacidade Total`,y=`Capacidade Total`), color = "black", linetype = 2, alpha = 0.5 )

cor(rel6_ocupacaoFechadoProvisoriosMedida$`Capacidade Total`,rel6_ocupacaoFechadoProvisoriosMedida$`População Prisional Total`)
plot(rel6_ocupacaoFechadoProvisoriosMedida$`Taxa de Ocupação Geral`, rel6_ocupacaoFechadoProvisoriosMedida$`Taxa Trabalho`)
cor(rel6_ocupacaoFechadoProvisoriosMedida$`Taxa de Ocupação Geral`, rel6_ocupacaoFechadoProvisoriosMedida$`Taxa Trabalho`, method = "spearman")


############################################################################################################################################

#ESTE RELATORIO FILTRA SOMENTE OS ESTABELECIMENTO QUE CONTEM PRESOS E VAGAS EM REGIME FECHADO E PROVISORIO 

rel7_ocupacaoFechadoProvisorio <- 
  base_taxa %>%
  group_by(
    Ano,
    Referência,
    UF,
    Município,
    `Código Mun.IBGE`,
    `Nome do Estabelecimento`,
    `Tipo de Custódia`,
    `Tipo de Unidade`,
    `População Prisional Total`,
    `Capacidade Total`
  )%>%
  summarise(
    `Taxa de Ocupação Geral` = round(sum(`População Prisional Total`, na.rm = TRUE) / sum(`Capacidade Total`, na.rm = TRUE) * 100, digits = 2),
    `Taxa de Ocupação-Fechado e Provisório`= round((sum(`População Prisional-Provisório-Total`, na.rm = TRUE)+sum(`População Prisional-Fechado-Total`, na.rm = TRUE))/
                                                   (sum(`Capacidade-Provisório-Total`, na.rm = TRUE)+sum(`Capacidade-Fechado-Total`, na.rm = TRUE))*100, digits = 2),
    `Taxa Agravos Transmissíveis` = round(sum(`Agravos Transmissíveis`,na.rm = TRUE) / sum(`População Movimentada`,na.rm = TRUE)*100, digits = 2),
    `Taxa Consultas Médicas`=round(sum(`Consultas Médicas Interna e Externas`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Atividades Educacionais`=round(sum(`Atividades Educacionais`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho externo` = round(sum(`Trabalho externo`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho interno` = round(sum(`Trabalho interno`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho` = round(sum(`Trabalho`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `População Fechados e Provisórios` = sum(`População Prisional-Provisório-Total`) + sum(`População Prisional-Fechado-Total`, na.rm = TRUE),
    `Capacidade Fechados e Provisórios` = sum(`Capacidade-Provisório-Total`, na.rm = TRUE)+sum(`Capacidade-Fechado-Total`, na.rm = TRUE)
    )%>%
   filter(
     `Tipo de Custódia` %in% c("Fechado","Provisórios","Diversos"),
     `Tipo de Unidade` == "Comum"
     )

quartil1 <- quantile(rel7_ocupacaoFechadoProvisorio$`Taxa de Ocupação-Fechado e Provisório`,0.25, na.rm = TRUE)
quartil3 <- quantile(rel7_ocupacaoFechadoProvisorio$`Taxa de Ocupação-Fechado e Provisório`,0.75, na.rm = TRUE)

rel7_ocupacaoFechadoProvisorio_quartil <-
  rel7_ocupacaoFechadoProvisorio %>%
  filter(
    `Taxa de Ocupação-Fechado e Provisório` >= quartil1,
    `Taxa de Ocupação-Fechado e Provisório` <= quartil3
  )

rel7_amostra_ocupacao_fechado_provisorio <- 
  rel7_ocupacaoFechadoProvisorio %>%
 mutate(
   `Amostra` = ifelse(`Taxa de Ocupação-Fechado e Provisório` == 0,"zero","um")
 )%>%
  group_by(
    Ano,
    Referência,
    `Amostra`
  )%>%
  summarise(
    `Variável` = n()  
  )
  
graf7.1_dispersao <-
  ggplot(data = rel7_ocupacaoFechadoProvisorio)+
  geom_point(mapping = aes(x=`Capacidade Total`, y = `População Prisional Total`, color = `Tipo de Custódia`))+
  geom_line(mapping = aes(x=`Capacidade Total`, y = `Capacidade Total`), color = "black", linetype = 2, alpha = 0.5 )

graf7.1.1_dispersao_quartil <-
  ggplot(data = rel7_ocupacaoFechadoProvisorio_quartil)+
  geom_point(mapping = aes(x=`Capacidade Total`,y=`População Prisional Total`, color = `Tipo de Custódia`))+
  geom_line(mapping = aes(x=`Capacidade Total`,y=`Capacidade Total`), color = "black", linetype = 2, alpha = 0.5 )

graf7.2_dispersao_doencas <-
  rel7_ocupacaoFechadoProvisorio_quartil %>%
  filter(
    `Taxa Agravos Transmissíveis` > 0
  )%>%
  ggplot()+
  geom_point(mapping = aes(x=`Taxa de Ocupação-Fechado e Provisório`,y=`Taxa Agravos Transmissíveis`, color = `Tipo de Custódia`))
  
graf7.3_dispersao_medicos <-
  rel7_ocupacaoFechadoProvisorio_quartil %>%
  filter(
    `Taxa Consultas Médicas` > 0
  )%>%
  ggplot()+
  geom_point(mapping = aes(x=`Taxa de Ocupação-Fechado e Provisório`,y=`Taxa Consultas Médicas`, color = `Tipo de Custódia`))

graf7.4_dispersao_educacao <-
  rel7_ocupacaoFechadoProvisorio_quartil %>%
  filter(
    `Taxa Atividades Educacionais` > 0
  )%>%
  ggplot()+
  geom_point(mapping = aes(x=`Taxa de Ocupação-Fechado e Provisório`,y=`Taxa Atividades Educacionais`, color = `Tipo de Custódia`))

graf7.5_dispersao_trabalho <-
  rel7_ocupacaoFechadoProvisorio_quartil %>%
  filter(
    `Taxa Trabalho` > 0,
    `Taxa Trabalho` < 100
  )%>%
  ggplot()+
  geom_point(mapping = aes(x=`Taxa de Ocupação-Fechado e Provisório`,y = `Taxa Trabalho`, color = `Tipo de Custódia`))

graf7.6_dispersao_trabalho_interno <-
  rel7_ocupacaoFechadoProvisorio_quartil %>%
  filter(
    `Taxa Trabalho interno` > 0,
    `Taxa Trabalho interno` < 100
  )%>%
  ggplot()+
  geom_point(mapping = aes(x=`Taxa de Ocupação-Fechado e Provisório`,y = `Taxa Trabalho interno`, color = `Tipo de Custódia`))

graf7.7_dispersao_trabalho_externo <-
  rel7_ocupacaoFechadoProvisorio_quartil %>%
  filter(
    `Taxa Trabalho externo` > 0,
    `Taxa Trabalho externo` < 100
  )%>%
  ggplot()+
  geom_point(mapping = aes(x=`Taxa de Ocupação-Fechado e Provisório`,y = `Taxa Trabalho externo`, color = `Tipo de Custódia`))

rel7_amostra_trabalho <- 
  rel7_ocupacaoFechadoProvisorio %>%
  mutate(
    `Amostra` = ifelse(`Taxa Trabalho` == 0,"Zero","um")
  )%>%
  group_by(
    Ano,
    Referência,
    `Amostra`
  )%>%
  summarise(
    `Variável` = n()  
  )

graf7_histograma <- 
  rel7_ocupacaoFechadoProvisorio %>%
  ungroup()%>%
   dplyr::select(#ALGUM PACOTE TEM O COMANDO SELECT, DESSA FORMA, TEMOS QUE ESTABELECER O PACOTE A SER UTILIZADO ANTES DE USAR A FUNCAO
    `Nome do Estabelecimento`,
    `Taxa de Ocupação-Fechado e Provisório`,
    `Taxa de Ocupação Geral`
  )%>%
  filter(
    `Taxa de Ocupação Geral` < 400,
    `Taxa de Ocupação-Fechado e Provisório` < 400
  )%>%
  pivot_longer( #SUBSTITUI O GATHER
    cols = !`Nome do Estabelecimento`,
    names_to = "Variavel",
    values_to = "Taxa"
  )%>%
  ggplot()+
  geom_histogram(aes(`Taxa`, fill = Variavel), binwidth = 5, alpha = 0.5 )

####################################################################################################################

rel8_ocupacaoFechado <- 
  base_taxa %>%
  group_by(
    Ano,
    Referência,
    UF,
    Município,
    `Código Mun.IBGE`,
    `Nome do Estabelecimento`,
    `Tipo de Custódia`,
    `Tipo de Unidade`,
    `População Prisional Total`,
    `Capacidade Total`
  )%>%
  summarise(
    `Taxa de Ocupação Geral` = round(sum(`População Prisional Total`, na.rm = TRUE) / sum(`Capacidade Total`, na.rm = TRUE) * 100, digits = 2),
    `Taxa de Ocupação-Fechado e Provisório`= round((sum(`População Prisional-Provisório-Total`, na.rm = TRUE)+sum(`População Prisional-Fechado-Total`, na.rm = TRUE))/
                                                     (sum(`Capacidade-Provisório-Total`, na.rm = TRUE)+sum(`Capacidade-Fechado-Total`, na.rm = TRUE))*100, digits = 2),
    `Taxa Agravos Transmissíveis` = round(sum(`Agravos Transmissíveis`,na.rm = TRUE) / sum(`População Movimentada`,na.rm = TRUE)*100, digits = 2),
    `Taxa Consultas Médicas`=round(sum(`Consultas Médicas Interna e Externas`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Atividades Educacionais`=round(sum(`Atividades Educacionais`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho externo` = round(sum(`Trabalho externo`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho interno` = round(sum(`Trabalho interno`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `Taxa Trabalho` = round(sum(`Trabalho`,na.rm = TRUE)/sum(`População Movimentada`,na.rm = TRUE)*100,digits = 2),
    `População Fechados e Provisórios` = sum(`População Prisional-Provisório-Total`) + sum(`População Prisional-Fechado-Total`, na.rm = TRUE),
    `Capacidade Fechados e Provisórios` = sum(`Capacidade-Provisório-Total`, na.rm = TRUE)+sum(`Capacidade-Fechado-Total`, na.rm = TRUE)
  )%>%
  filter(
    `Tipo de Custódia` %in% c("Fechado"),
    `Tipo de Unidade` == "Comum",
    `População Prisional Total` > 100,
    `Taxa de Ocupação-Fechado e Provisório` < 600,
    `Taxa Atividades Educacionais` > 0
  )
  
plot(rel8_ocupacaoFechado$`Capacidade Fechados e Provisórios`,rel8_ocupacaoFechado$`População Fechados e Provisórios`)
plot(rel8_ocupacaoFechado$`Taxa de Ocupação-Fechado e Provisório`,rel8_ocupacaoFechado$`Taxa Atividades Educacionais`)


cor(rel8_ocupacaoFechado$`Taxa de Ocupação-Fechado e Provisório`,rel8_ocupacaoFechado$`Taxa Atividades Educacionais`, method = "spearman")

##############################################################################################################
# #MAPA COM AS UNIDADES SUPERLOTADAS
# 
# setwd("C:/Users/lucas.eneas/OneDrive - MINISTERIO DA JUSTIÇA/analise_dados/shapes/BR_Municipios_2020")
# shapeMunicipios <-read_municipality(year = 2020)
# 
# ggplot() +
#   geom_sf(data=shapeMunicipios, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
#   labs(subtitle="States", size=8) +
#   theme_minimal() +
#   theme(axis.title=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank())
# 
# mapa <- inner_join(rel7_ocupacaoFechadoProvisorio_quartil,shapeMunicipios, by = c("Código Mun.IBGE"="code_muni"))
# mapa <- as_data_frame(mapa)
# 
# ggplot() +
#   geom_sf()+
#   geom_sf(data=mapa, aes(geometry = geom, size=`Taxa de Ocupação-Fechado e Provisório`), shape="circle", color = "blue", size = 0.1) +
#   labs(subtitle="Classes de Ouvidorias - abril de 2021", size=8) +
#   #geom_text(mapping = aes(label = Estado), position = position_dodge2(width = 1.0), vjust = -0.5, size = 1 ) +
#   #scale_fill_distiller(palette = "Blues", name="Classe de Ouvidoria", limits = c(1,2)) + #UTILIZADO PARA ESCALAS NUMERICAS
#   #theme_minimal() +
#   #theme(axis.title=element_blank(),
#   #axis.text=element_blank(),
#   #axis.ticks=element_blank())
#   #labs(x = "Ciclo" , y = "Máximo de Pessoas Visitadas", title = "Número Máximo de Pessoas Visitadas por período ", fill = "")+ 
#   scale_fill_manual(values = cores)+ #CORES DAS BARRAS DO GRAFICO - AZUL DEPEN E VERMELHO ESCURO 
#   theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
#         axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
#         axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
#         #legend.title = element_text(color = "Black", size = 7, face = "plain",angle = 0),
#         legend.title = element_blank(),
#         legend.text  = element_text(color = "Black", size = 7, face = "plain",angle = 0, hjust = -0.5),
#         #axis.text.x  = element_text(color = "Black", size = 4, face = "plain",angle = 45), #TEXTO DA VARIAVEL EM X
#         axis.text.x  = element_blank(),
#         axis.text.y  = element_blank(), #SEM TEXTO DA VARIAVEL EM Y
#         legend.position = "bottom", #POSICAO DA LEGENDA
#         legend.direction = "vertical",
#         legend.text.align = 0,
#         legend.key.size = unit(0.7,"cm") #TAMANHO DA CAIXA DA LEGENDA
#   )

###########################################################################################################################
#CLUSTERIZACAO

# Para determinar automaticamente o número ótimo de clusters da classificação, antes de rodar a função kmeans(), 
# é possível utilizar a função fviz_nbclust() do pacote factoextra. Desta forma, utilizando a noção da soma dos 
# quadrados intra cluster é possível verificar que o número ótimo de clusters para a amostra é 4. Isto porque 
# novos clusters acima de 4 possuem baixo ganho para aumentar a diferenciação dos demais.

df <- 
  rel7_ocupacaoFechadoProvisorio %>%
  ungroup()%>%
  dplyr::select(
    `Capacidade Fechados e Provisórios`,
    `População Fechados e Provisórios`
  )%>%
  scale() #CRIA O VETOR PARA PARA GRAFICO DE DISPERSAO

fviz_nbclust(df, kmeans, method = c("wss"), print.summary = TRUE, verbose = TRUE, linecolor = "red", k.max = 5, iter.max = 20) + 
  geom_vline(xintercept = 2, linetype = 2) #PLOTA O GRAFICO DE OTIMIZACAO DO NUMERO DE CLUSTERS

fviz_nbclust(df, kmeans, method = c("silhouette"), print.summary = TRUE, verbose = TRUE, linecolor = "red", k.max = 5, iter.max = 20)
 
fviz_nbclust(df, kmeans, method = c("gap_stat"), nstart = 25, nboot = 50 , k.max = 5, iter.max = 20)



# A função kmeans() é utilizada para o calculo, sendo que x representa a base de dados a ser analisada; 
# centers será substituído pelo número de clusters desejados; iter.max representa o número de iterações 
# para a constituição dos objetos dentro dos clusters, sendo que o padrão é 10; nstart é o número inicial 
# de partições, sendo que o recomendado é superior a 1.

taxaCluster <- kmeans(x =df,centers = 2, nstart = 25) #CRIA OS CLUSTERS

rel_cluster <- cbind(rel7_ocupacaoFechadoProvisorio, cluster= taxaCluster$cluster)
rel_cluster <- rel_cluster %>% ungroup %>% dplyr::select( `População Prisional Total`:`cluster`, -`Taxa de Ocupação-Fechado e Provisório`)

analise_cluster <- 
  rel_cluster %>%
  group_by(
    #`Nome do Estabelecimento`,
    `cluster`
  )%>%
  summarise(
    `Quantidade de Clusters` = n(),
    #`Media Taxa de Ocupação-Fechado Provisórios` = mean(`Taxa de Ocupação-Fechado e Provisório`, na.rm = TRUE),
    #`Media Taxa de Ocupação Geral` = mean(`Taxa de Ocupação Geral`, na.rm = TRUE),
    `Soma Capacidade` = sum(`Capacidade Total`, na.rm = TRUE),
    `Soma População` = sum(`População Prisional Total`, na.rm = TRUE),
    `Taxa de Ocupação por Cluster-Fechados e Provisórios` =  sum(`População Fechados e Provisórios`, na.rm = TRUE) / sum(`Capacidade Fechados e Provisórios`, na.rm = TRUE),
    `Taxa de Ocupação por Cluster-Geral` = `Soma População` / `Soma Capacidade`
    )
  

fviz_cluster(taxaCluster, data = rel_cluster,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)


graf_cluster<-
  rel_cluster %>%
  ungroup()%>%
  dplyr::select(
    `cluster`,
    `Capacidade Fechados e Provisórios`,
    `População Fechados e Provisórios`
  )%>%
  gather(-cluster, key = "Variavel", value = "Quantidade") %>%
  ggplot(aes(x= Variavel, y= Quantidade )) +
  geom_line(aes(group = `Variavel`,  color= factor(`cluster`))) +
  scale_y_log10()+
  #geom_text(mapping = aes(label = `Quantidade`), hjust = 1.0, vjust = -0.5, size = 2.0) +
  labs(x = "Variáveis" , y = "Quantidade", title = "Separação em Cluster's", fill = "")+ 
  scale_fill_manual(values = c("#294661","darkred"))+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm") #TAMANHO DA CAIXA DA LEGENDA
  )


graf_cluster2<-
  rel_cluster %>%
  ggplot(aes(x= `Capacidade Fechados e Provisórios`, y= `População Fechados e Provisórios`  )) +
  geom_line(aes(color= factor(`cluster`))) +
  #scale_y_log10()+
  #geom_text(mapping = aes(label = `Quantidade`), hjust = 1.0, vjust = -0.5, size = 2.0) +
  #labs(x = "Variáveis" , y = "Quantidade", title = "Separação em Cluster's", fill = "")+ 
  scale_fill_manual(values = c("#294661","darkred"))+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm") #TAMANHO DA CAIXA DA LEGENDA
  )


graf_cluster3<-
  rel_cluster %>%
  ggplot(aes(x= `Capacidade Fechados e Provisórios`, y= `População Fechados e Provisórios`)) +
  geom_point(aes(color = factor(`cluster`))) +
  geom_line(aes(x=`Capacidade Fechados e Provisórios`,y=`Capacidade Fechados e Provisórios`), linetype = 2)
  #scale_y_log10()+
  #geom_text(mapping = aes(label = `Quantidade`), hjust = 1.0, vjust = -0.5, size = 2.0) +
  labs(x = "Capacidade" , y = "População", title = "Separação em Cluster's", fill = "" )+ 
  scale_fill_manual(values = c("#294661","darkred"))+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm") #TAMANHO DA CAIXA DA LEGENDA
       )
  




###########################################################################################################################
#REMOVE OUTLIERS TAXA DE OCUPACAO
x <- rel6_ocupacaoFechadoProvisoriosMedida$`Taxa de Ocupação Geral`
ocupacao_sem_outlier <- x[!x %in% boxplot(x)$out]
length(x)
length(ocupacao_sem_outlier)
boxplot(ocupacao_sem_outlier)
hist(ocupacao_sem_outlier)
shapiro.test(ocupacao_sem_outlier)
summary(ocupacao_sem_outlier)


valores <- summary(rel6_ocupacaoFechadoProvisoriosMedida)
valores

cor(rel6_ocupacaoFechadoProvisoriosMedida[,9:12], method = "spearman")
correlacao <- cor(rel7_ocupacaoFechadoProvisorio_quartil[,12:18], method = "spearman")

####################################################################################################################################################

#MAPA DA TAXA DE OCUPACAO POR ESTADO

setwd("C:/Users/55619/OneDrive - MINISTERIO DA JUSTIÇA/analise_dados/shapes/BR_UF_2020")
shapeEstados <-read_country(year = 2020)

ggplot() +
  geom_sf(data=shapeEstados, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

base_mapa <- 
  base_taxa %>%
  group_by(
    UF
  )%>%
  summarise(
    `População Prisional` = sum(`População Prisional-Provisório-Total`,na.rm = TRUE),
    `Taxa de Ocupação geral` = round(sum(`População Prisional Total`, na.rm = TRUE) / sum(`Capacidade Total`, na.rm = TRUE)*100,digits=2),
    `Taxa de Ocupação-Fechados e Provisórios` = round((sum(`População Prisional-Fechado-Total`,na.rm = TRUE)+sum(`População Prisional-Provisório-Total`))/
                                                      (sum(`Capacidade-Fechado-Total`,na.rm = TRUE)+sum(`Capacidade-Provisório-Total`,na.rm = TRUE))*100,digits = 2)        
    )


#MODELANDO / JUNTANDO AS TABELAS DE DADOS E SHAPE
mapa <- inner_join(base_mapa,shapeEstados, by = c("UF"="abbrev_state"))
 mapa <- 
   mapa %>%
   mutate(
     `Taxa de Ocupação` = ifelse(`Taxa de Ocupação-Fechados e Provisórios`<=90, "Menor que 90%",
                           ifelse(`Taxa de Ocupação-Fechados e Provisórios`<=110,  "Entre 90% e 110%",
                            ifelse(`Taxa de Ocupação-Fechados e Provisórios`<=130,  "Entre 111% e 130%",  
                             ifelse(`Taxa de Ocupação-Fechados e Provisórios`<=150,  "Entre 131% e 150%",
                              ifelse(`Taxa de Ocupação-Fechados e Provisórios`<=170,  "Entre 151% e 170%",
                               ifelse(`Taxa de Ocupação-Fechados e Provisórios`<=190,  "Entre 171% e 190%",
                                ifelse(`Taxa de Ocupação-Fechados e Provisórios`<=210,  "Entre 191% e 210%", "Maior que 210%")))))))
   )

mapa <- as_data_frame(mapa)


#hist(x=mapa$`Taxa de Ocupação-Fechados e Provisórios`)

graf_barras <- 
 rel7_ocupacaoFechadoProvisorio %>%
  ungroup()%>%
  dplyr::select(
    `Taxa de Ocupação-Fechado e Provisório`,
    `População Fechados e Provisórios`
   )%>%
  mutate(
    `Taxa de Ocupação` = ifelse(`Taxa de Ocupação-Fechado e Provisório`<=90, "Menor que 90%",
                          ifelse(`Taxa de Ocupação-Fechado e Provisório`<=110,  "Entre 90% e 110%",
                           ifelse(`Taxa de Ocupação-Fechado e Provisório`<=130,  "Entre 111% e 130%",  
                            ifelse(`Taxa de Ocupação-Fechado e Provisório`<=150,  "Entre 131% e 150%",
                             ifelse(`Taxa de Ocupação-Fechado e Provisório`<=170,  "Entre 151% e 170%",
                              ifelse(`Taxa de Ocupação-Fechado e Provisório`<=190,  "Entre 171% e 190%",
                               ifelse(`Taxa de Ocupação-Fechado e Provisório`<=210,  "Entre 191% e 210%", 
                                ifelse(`Taxa de Ocupação-Fechado e Provisório`<=230,  "Entre 211% e 230%",
                                 ifelse(`Taxa de Ocupação-Fechado e Provisório`<=250,  "Entre 231% e 250%",
                                  ifelse(`Taxa de Ocupação-Fechado e Provisório`<=270,  "Entre 251% e 270%","Maior que 270%")))))))))),
    `Taxa de Ocupação` = factor(`Taxa de Ocupação`, levels = c("Menor que 90%","Entre 90% e 110%","Entre 111% e 130%",
                                                               "Entre 131% e 150%","Entre 151% e 170%","Entre 171% e 190%",
                                                               "Entre 191% e 210%","Entre 211% e 230%","Entre 231% e 250%",
                                                               "Entre 251% e 270%","Maior que 270%")) #COLOCA O VETOR EM ORDEM
  )%>%
  group_by(
    `Taxa de Ocupação`
  )%>%
  summarise(
  `População Prisional` = sum(`População Fechados e Provisórios`,na.rm = TRUE)
  )%>%
ggplot()+
  geom_bar(mapping = aes(x=`Taxa de Ocupação`,y=`População Prisional`), fill = "#294661", colour = "White", alpha = 0.7, stat = "identity")+
  geom_text(mapping = aes(x=`Taxa de Ocupação`,y=`População Prisional`, label = `População Prisional`), position = position_dodge2(width = 1.0), vjust = -0.5, size = 3 )+
  theme(#plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_text(color = "Black", size = 5, face = "plain",angle = 0),
        legend.text  = element_text(color = "Black", size = 4, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 7, face = "plain",angle = 45), #TEXTO DA VARIAVEL EM X
        axis.text.y  = element_blank(), #SEM TEXTO DA VARIAVEL EM Y
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm") #TAMANHO DA CAIXA DA LEGENDA
  )
  

ggplot() +
  geom_sf(data=mapa, aes(geometry = geom, fill=`Taxa de Ocupação`), color = "black", size = 0.1) +
  labs(subtitle="Taxa de Ocupação nos Regimes Fechado e Provisório - Infopen", size=8) +
  #geom_text(mapping = aes(label = Estado), position = position_dodge2(width = 1.0), vjust = -0.5, size = 1 ) +
  #scale_fill_distiller(palette = "Blues", name="Classe de Ouvidoria", limits = c(1,2)) + #UTILIZADO PARA ESCALAS NUMERICAS
  #theme_minimal() +
  #theme(axis.title=element_blank(),
  #axis.text=element_blank(),
  #axis.ticks=element_blank())+
  scale_fill_manual(values = cores) #CORES DAS BARRAS DO GRAFICO - AZUL DEPEN E VERMELHO ESCURO 
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        #legend.title = element_text(color = "Black", size = 7, face = "plain",angle = 0),
        legend.title = element_blank(),
        legend.text  = element_text(color = "Black", size = 7, face = "plain",angle = 0, hjust = -0.5),
        #axis.text.x  = element_text(color = "Black", size = 4, face = "plain",angle = 45), #TEXTO DA VARIAVEL EM X
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(), #SEM TEXTO DA VARIAVEL EM Y
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.direction = "vertical",
        legend.text.align = 0,
        legend.key.size = unit(0.7,"cm") #TAMANHO DA CAIXA DA LEGENDA
  )




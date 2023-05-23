library(tidyverse)
library(ggforce) #PACOTE PARA COLOCAR FACET_WRAP EM VARIAS PAGINAS (FACET_WRAP_PAGINATE)
library(factoextra) #PACOTE PARA TECNICA DE CLUSTER TIPO K-MEANS
library(cluster)
set.seed(1)

setwd("C:/Users/lucas.eneas/OneDrive - MINISTERIO DA JUSTIÇA/analise_dados/bases")
#setwd("C:/Users/lucas.eneas/OneDrive - MINISTERIO DA JUSTIÇA/alteracoesValidacaoDadosCosisdepen")
base_sisdepen <- read.csv2("base_sisdepen.csv")

cores <- c("#294661","#7F312F","#808080","#B8860B","#5E3B56","#5F9EA0","#808000", 
           "#A0522D","#F5DEB3","#FF9900","#8B008B","#5F6B6D","#FB8281","#F4D25A",
           "#7F898A","#A4DDEE","#FDAB89","#B687AC","#28738A","#A78F8F","#168980") #VETOR DE CORES PARA OS GRAFICOS


base <- base_sisdepen %>%
  filter(
    A.Situação.de.Preenchimento == "Validado",
    A.Ano == 2020,
    A.Referencia == "Junho",
  )%>%
  mutate(
    `Ano` = A.Ano,
    `Referência` = A.Referencia,
    `Situação de Preenchimento` = A.Situação.de.Preenchimento,
    `UF` = A.UF,
    `Nome do Estabelecimento` = A.Nome.do.Estabelecimento,
  
    
    `Tipo de Custódia`=ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime semiaberto)"), ignore_case = TRUE)))==TRUE,"Semiaberto",
                        ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime fechado)"), ignore_case = TRUE)))==TRUE,"Fechado",
                         ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime aberto)"), ignore_case = TRUE)))==TRUE,"Aberto",
                          ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(diversos tipos de regime)"), ignore_case = TRUE)))==TRUE,"Diversos",
                           ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(presos provisórios)"), ignore_case = TRUE)))==TRUE,"Provisórios",
                            ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(medida de segurança)"), ignore_case = TRUE)))==TRUE,"Medida de Segurança",
                             ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(patronato)"), ignore_case = TRUE)))==TRUE,"Patronato",
                              "Diversos"))))))),
    `Unidade Monitoramento` = ifelse((str_detect(A.Nome.do.Estabelecimento,regex(c("(monitoramento)|(monitora(ç|c)(ã|a)o)|(eletr(ô|o)nic(o|a))"),ignore_case=TRUE)))==TRUE,"Monitoramento Eletrônico","Não"),
    `Unidade Domiciliar` = ifelse((str_detect(A.Nome.do.Estabelecimento,regex(c("(Domiciliar)"),ignore_case=TRUE)))==TRUE,"Domiciliar","Não"),
    `Unidade Monitoramento - Domiciliar` = ifelse(`Unidade Monitoramento` == "Monitoramento Eletrônico" & `Unidade Domiciliar` == "Domiciliar", "Mon.Eletrônico e Domiciliar","Não")
  )%>%
  group_by(
    `Ano`,
    `Referência`,
    `Situação de Preenchimento`,
    `UF`,
    `Nome do Estabelecimento`,
    `Tipo de Custódia`,
    `Unidade Monitoramento`,
    `Unidade Domiciliar`,
    `Unidade Monitoramento - Domiciliar`
  )%>%
  summarise(
    `Capacidade-Provisório-Masculino`  = sum(M.1.3.Capacidade.do.estabelecimento...Presos.provisórios...Masculino,na.rm = TRUE),
    `Capacidade-Provisório-Feminino`   = sum(M.1.3.Capacidade.do.estabelecimento...Presos.provisórios...Feminino,na.rm = TRUE),
    `Capacidade-Fechado-Masculino`      = sum(M.1.3.Capacidade.do.estabelecimento...Regime.fechado...Masculino,na.rm = TRUE),
    `Capacidade-Fechado-Feminino`       = sum(M.1.3.Capacidade.do.estabelecimento...Regime.fechado...Feminino,na.rm = TRUE),
    `Capacidade-Semiaberto-Masculino`   = sum(M.1.3.Capacidade.do.estabelecimento...Regime.semiaberto...Masculino,na.rm = TRUE),
    `Capacidade-Semiaberto-Masculino`   = sum(M.1.3.Capacidade.do.estabelecimento...Regime.semiaberto...Feminino,na.rm = TRUE),
    `Capacidade-Aberto-Masculino`       = sum(M.1.3.Capacidade.do.estabelecimento...Regime.aberto...Masculino,na.rm = TRUE),
    `Capacidade-Aberto-Feminino`        = sum(M.1.3.Capacidade.do.estabelecimento...Regime.aberto...Feminino,na.rm = TRUE),
    `Capacidade-Med.Segurança-Masculino`= sum(M.1.3.Capacidade.do.estabelecimento...Medidas.de.segurança.de.internação...Masculino,na.rm = TRUE),
    `Capacidade-Med.Segurança-Feminino` = sum(M.1.3.Capacidade.do.estabelecimento...Medidas.de.segurança.de.internação...Feminino,na.rm = TRUE),
    `Capacidade-Outros-Masculino`       = sum(M.1.3.Capacidade.do.estabelecimento...Outro.s...Qual.is.....Masculino,na.rm = TRUE),
    `Capacidade-Outros-Feminino`        = sum(M.1.3.Capacidade.do.estabelecimento...Outro.s...Qual.is.....Feminino,na.rm = TRUE),
    
                   
    `População Prisional-Provisório-Masculino` = sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Justiça.Federal.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Provisório-Feminino`  = sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Justiça.Federal.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.provisórios..sem.condenação....Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Fechado-Masculino`    = sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Fechado-Feminino`     = sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.fechado...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Semiaberto-Masculino` = sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Semiaberto-Feminino`  = sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.semiaberto...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Aberto-Masculino`     = sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Aberto-Feminino`      = sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                 sum(M.4.1.População.prisional...Presos.sentenciados...regime.aberto...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Med.Segurança Internação-Masculino`= sum(M.4.1.População.prisional...Medida.de.segurança...internação...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                              sum(M.4.1.População.prisional...Medida.de.segurança...internação...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                              sum(M.4.1.População.prisional...Medida.de.segurança...internação...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Med.Segurança Internação-Feminino`= sum(M.4.1.População.prisional...Medida.de.segurança...internação...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                             sum(M.4.1.População.prisional...Medida.de.segurança...internação...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                             sum(M.4.1.População.prisional...Medida.de.segurança...internação...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    `População Prisional-Med.Segurança Ambulatorial-Masculino`= sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Justiça.Estadual.Masculino, na.rm = TRUE)+
                                                                sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Justiça.Federal.Masculino, na.rm = TRUE)+
                                                                sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Outros.Just..Trab...cível..Masculino, na.rm = TRUE),
    
    `População Prisional-Med.Segurança Ambulatorial-Feminino`= sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Justiça.Estadual.Feminino, na.rm = TRUE)+
                                                               sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Justiça.Federal.Feminino, na.rm = TRUE)+
                                                               sum(M.4.1.População.prisional...Medida.de.segurança...tratamento.ambulatorial...Outros.Just..Trab...cível..Feminino, na.rm = TRUE),
    
    )


rel_capacidade <- base %>%
  select(`Ano`:`Capacidade-Outros-Feminino`)%>%
  gather(
    -`Ano`,
    -`Referência`,
    -`Situação de Preenchimento`,
    -`UF`,
    -`Nome do Estabelecimento`,
    -`Tipo de Custódia`,
    -`Unidade Monitoramento`,
    -`Unidade Domiciliar`,
    -`Unidade Monitoramento - Domiciliar`,
    key = "Tipo de Vaga",
    value = "Quantidade"
    )%>%
  separate(`Tipo de Vaga`, c("Tipo de Variável","Tipo de Regime", "Sexo"), sep = "-", remove = TRUE)

rel_populacao <- base %>%
  select(`Ano`:`Unidade Monitoramento - Domiciliar`,
         `População Prisional-Provisório-Masculino`:`População Prisional-Med.Segurança Ambulatorial-Feminino`)%>%
  gather(
    -`Ano`,
    -`Referência`,
    -`Situação de Preenchimento`,
    -`UF`,
    -`Nome do Estabelecimento`,
    -`Tipo de Custódia`,
    -`Unidade Monitoramento`,
    -`Unidade Domiciliar`,
    -`Unidade Monitoramento - Domiciliar`,
    key = "População",
    value = "Quantidade"
  )%>%
  separate(População, c("Tipo de Variável","Tipo de Regime", "Sexo"), sep = "-", remove = TRUE)
  
  
 rel_geral <- full_join(rel_populacao,rel_capacidade)
 
 rel_geral2 <- rel_geral %>%
   spread(
     key = "Tipo de Variável",
     value = "Quantidade"
   )
     
   
 setwd("C:/Users/lucas.eneas/OneDrive - MINISTERIO DA JUSTIÇA/analise_dados/taxa_ocupacao/tabelas/tabelas2")
 #setwd("C:/Users/lucas.eneas/OneDrive - MINISTERIO DA JUSTIÇA/alteracoesValidacaoDadosCosisdepen")
 write.csv2(rel_geral, file = "rel_geral.csv", row.names = FALSE)
 
 rel1 <- rel_geral %>%
   select(
     -`Unidade Monitoramento - Domiciliar`
   )%>%
   gather(
     -`Ano`,
     -`Referência`,
     -`Situação de Preenchimento`,
     -`UF`,
     -`Nome do Estabelecimento`,
     -`Tipo de Custódia`,
     - `Tipo de Variável`,
     -`Tipo de Regime`,
     -`Sexo`,
     -`Quantidade`,
     key = "Monitoramento ou Domiciliar",
     value = "Sim ou Não"
   )%>%
   filter(
     `Sim ou Não` != "Não"
   )
    

 rel2 <- rel_geral %>%
   filter(
     `Unidade Domiciliar` == "Não",
     `Unidade Monitoramento` == "Não",
     `Unidade Monitoramento - Domiciliar` == "Não"
   )%>%
   group_by(
     #`Nome do Estabelecimento`,
     #`Tipo de Custódia`,
     `Tipo de Variável`,
     `Tipo de Regime`
   )%>%
   summarise(
     Quantidade = sum(`Quantidade`, na.rm = TRUE)
   )%>%
   ggplot()+
   geom_bar(aes(x = `Tipo de Regime`, y = `Quantidade`,fill = `Tipo de Variável`), stat = "identity", show.legend = TRUE,position = position_dodge2(width = 1.0))+
   geom_text(mapping = aes(x = `Tipo de Variável`, y = Quantidade, label = Quantidade), position = position_dodge2(width = 1.0), vjust = -0.5, size = 2 )+ 
   labs(x = "Intervalo" , y = "Quantidade de Unidades", title = "Taxa de Ocupação por Classe", fill = "")+ 
   scale_fill_manual(values = cores)+
   theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
         axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
         axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
         legend.title = element_text(color = "Black", size = 5, face = "plain",angle = 0),
         legend.text  = element_text(color = "Black", size = 4, face = "plain",angle = 0),
         axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 45), #TEXTO DA VARIAVEL EM X
         axis.text.y  = element_blank(), #SEM TEXTO DA VARIAVEL EM Y
         legend.position = "bottom", #POSICAO DA LEGENDA
         legend.key.size = unit(0.5,"cm") #TAMANHO DA CAIXA DA LEGENDA
   )
   


graf1 <-
  rel_geral2 %>%
  filter(
    `Unidade Monitoramento` == "Não"
  )%>%
  group_by(
    #`Unidade Monitoramento`,
    `Tipo de Regime`
  )%>%
  summarise(
    `População Prisional` = sum(`População Prisional`, na.rm = TRUE),
    `Capacidade` = sum(`Capacidade`, na.rm = TRUE)
  )%>%
  ggplot()+
  geom_point(aes(x = `Capacidade`, y=`População Prisional`, shape = `Unidade Monitoramento`, color = `Tipo de Regime` ))+
  geom_line(aes(x = `Capacidade`, y =`Capacidade`), color = "darkred")+
  #geom_text(mapping = aes( x=`Capacidade`,y=`População Prisional`), hjust = 1.0, vjust = -0.5, size = 3.0) +
  labs(x = "Capacidade" , y = "População Prisional", title = "Taxa de Ocupação por Unidade", fill = "")+ 
  scale_fill_manual(values = c("#294661","darkred"))+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_blank(),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm"), #TAMANHO DA CAIXA DA LEGENDA,
        legend.key.height =unit(0.5,"cm"),
        legend.margin = margin(t=2,r = 3,l = 4,unit = "pt")
  )

graf1.1 <-
  ggplot(rel1)+
  geom_point(aes(x = `A.UF`, y=log10(`Taxa de Ocupação`), color = `A.UF`), show.legend = FALSE)+
  #geom_text(mapping = aes( x=`Capacidade`,y=`População Prisional`), hjust = 1.0, vjust = -0.5, size = 3.0) +
  labs(x = "Unidade Federativa" , y = "Taxa de Ocupação", title = "Taxa de Ocupação por Estado", fill = "")+ 
  scale_fill_manual(values = cores)+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_blank(),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm"), #TAMANHO DA CAIXA DA LEGENDA,
        legend.key.height =unit(0.5,"cm"),
        legend.margin = margin(t=2,r = 3,l = 4,unit = "pt")
  )

graf1.2 <-
  ggplot(rel1)+
  geom_boxplot(aes(x = `A.UF`, y=log10(`Taxa de Ocupação`), color= `A.UF`), show.legend = FALSE)+
  #geom_text(mapping = aes( x=`Capacidade`,y=`População Prisional`), hjust = 1.0, vjust = -0.5, size = 3.0) +
  labs(x = "Unidade Federativa" , y = "Taxa de Ocupação", title = "Taxa de Ocupação por Estado", fill = "")+ 
  scale_fill_manual(values = cores)+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_blank(),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm"), #TAMANHO DA CAIXA DA LEGENDA,
        legend.key.height =unit(0.5,"cm"),
        legend.margin = margin(t=2,r = 3,l = 4,unit = "pt")
  )



#################################################################################################

rel2 <- base_sisdepen %>%
  filter(
    A.Situação.de.Preenchimento == "Validado",
    A.Ano == 2020
  )%>%
  mutate(
    `Tipo de Custódia`=ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime semiaberto)"), ignore_case = TRUE)))==TRUE,"Semiaberto",
                              ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime fechado)"), ignore_case = TRUE)))==TRUE,"Fechado",
                                     ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime aberto)"), ignore_case = TRUE)))==TRUE,"Aberto",
                                            ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(diversos tipos de regime)"), ignore_case = TRUE)))==TRUE,"Diversos",
                                                   ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(presos provisórios)"), ignore_case = TRUE)))==TRUE,"Provisórios",
                                                          ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(medida de segurança)"), ignore_case = TRUE)))==TRUE,"Medida de Segurança",
                                                                 ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(patronato)"), ignore_case = TRUE)))==TRUE,"Patronato",
                                                                        "Diversos"))))))),
    `Unidade Monitoramento` = ifelse((str_detect(A.Nome.do.Estabelecimento,regex(c("(monitoramento)|(monitora(ç|c)(ã|a)o)|(eletr(ô|o)nic(o|a))"),ignore_case=TRUE)))==TRUE,"Sim","Não"),
  )%>%
  group_by(
    A.Ano,
    A.Referencia,
    A.Situação.de.Preenchimento,
    A.UF,
    A.Nome.do.Estabelecimento,
    `Tipo de Custódia`,
    `Unidade Monitoramento`
  )%>%
  summarise(
    `Capacidade` = sum(M.1.3.Capacidade.do.estabelecimento...Masculino...Total,na.rm = TRUE) + 
                   sum(M.1.3.Capacidade.do.estabelecimento...Feminino...Total,na.rm = TRUE),
    `População Prisional` = M.4.1.População.prisional...Total,
    `Taxa de Ocupação` = round(M.4.1.População.prisional...Total / `Capacidade`,digits = 2),
    `Intervalo de Taxa` = ifelse(`Taxa de Ocupação`<=0.5, "Até 80%",
                             ifelse(`Taxa de Ocupação`<=0.9,"Entre 80% e 90%",
                              ifelse(`Taxa de Ocupação`<=1.0,"Entre 90% e 100%",
                               ifelse(`Taxa de Ocupação`<=1.1,"Entre 100% e 110%",
                                ifelse(`Taxa de Ocupação`<=1.2,"Entre 110% e 120%",
                                 ifelse(`Taxa de Ocupação`<=1.3,"Entre 120% e 130%",
                                  ifelse(`Taxa de Ocupação`<=1.4,"Entre 130% e 140%",
                                   ifelse(`Taxa de Ocupação`<=1.5,"Entre 140% e 150%",
                                    ifelse(`Taxa de Ocupação`<=1.6,"Entre 150% e 160%",
                                     ifelse(`Taxa de Ocupação`<=1.7,"Entre 160% e 170%",
                                      ifelse(`Taxa de Ocupação`<=1.8,"Entre 170% e 180%",
                                       ifelse(`Taxa de Ocupação`>1.8,"Maior que 180%",
                                        "Erro"))))))))))))
  )%>%
  filter(
    #A.Situação.de.Preenchimento %in% c("Validado")
    `Tipo de Custódia` %in% c("Diversos","Fechado","Medida de Segurança","Provisórios","Semiaberto"),
    `Unidade Monitoramento` == "Não"
  )

graf2 <-
  ggplot(rel2)+
  geom_point(aes(x = `Capacidade`, y=`População Prisional`, color = `Tipo de Custódia` ))+
  geom_line(aes(x = `Capacidade`, y =`Capacidade`), color = "darkred")+
  labs(x = "Capacidade" , y = "População Prisional", title = "Taxa de Ocupação por Unidade", fill = "")+
  scale_fill_manual(values = cores)+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_blank(),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm"), #TAMANHO DA CAIXA DA LEGENDA,
        legend.key.height =unit(0.5,"cm"),
        legend.margin = margin(t=2,r = 3,l = 4,unit = "pt")
  )

graf2.1 <-
  ggplot(rel2)+
  geom_point(aes(x = `A.UF`, y=log10(`Taxa de Ocupação`), color = `A.UF`), show.legend = FALSE)+
  labs(x = "Unidade Federativa" , y = "Taxa de Ocupação", title = "Taxa de Ocupação por Estado", fill = "")+ 
  scale_fill_manual(values = cores)+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_blank(),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm"), #TAMANHO DA CAIXA DA LEGENDA,
        legend.key.height =unit(0.5,"cm"),
        legend.margin = margin(t=2,r = 3,l = 4,unit = "pt")
  )

graf2.2 <-
  ggplot(rel2)+
  geom_boxplot(aes(x = `A.UF`, y=log10(`Taxa de Ocupação`),color = `A.UF`),show.legend = FALSE)+
  labs(x = "Unidade Federativa" , y = "Taxa de Ocupação", title = "Taxa de Ocupação por Estado", fill = "")+ 
  scale_fill_manual(values = cores)+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_blank(),
        legend.text  = element_text(color = "Black", size = 8, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        #axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm"), #TAMANHO DA CAIXA DA LEGENDA,
        legend.key.height =unit(0.5,"cm"),
        legend.margin = margin(t=2,r = 3,l = 4,unit = "pt")
  )



nivel_taxa <- c("Até 80%","Entre 80% e 90%","Entre 90% e 100%","Entre 100% e 110%","Entre 110% e 120%","Entre 120% e 130%"
                ,"Entre 130% e 140%","Entre 140% e 150%","Entre 150% e 160%","Entre 160% e 170%","Entre 170% e 180%","Maior que 180%")

graf2.3 <- rel2 %>%
  group_by(`Intervalo de Taxa`)%>%
  summarise(`Quantidade` = n())%>%
  mutate(`Intervalo de Taxa` = factor(`Intervalo de Taxa`, levels = nivel_taxa ))%>%
  ggplot()+
  geom_bar(aes(x = `Intervalo de Taxa`, y = `Quantidade`), stat = "identity", fill = "#294661", show.legend = FALSE)+
  geom_text(mapping = aes(x = `Intervalo de Taxa`, y = Quantidade, label = Quantidade), position = position_dodge2(width = 1.0), vjust = -0.5, size = 2 ) + 
  labs(x = "Intervalo" , y = "Quantidade de Unidades", title = "Taxa de Ocupação por Classe", fill = "")+ 
  scale_fill_manual(values = cores)+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_text(color = "Black", size = 5, face = "plain",angle = 0),
        legend.text  = element_text(color = "Black", size = 4, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 45), #TEXTO DA VARIAVEL EM X
        axis.text.y  = element_blank(), #SEM TEXTO DA VARIAVEL EM Y
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm") #TAMANHO DA CAIXA DA LEGENDA
  )

graf2.4 <- rel2 %>%
  filter(`Taxa de Ocupação` < 10)%>%
  ggplot()+
  geom_histogram(aes(x=`Taxa de Ocupação`),binwidth = 0.1,fill = "#294661", show.legend = FALSE)+
  #geom_density(aes(x=`Taxa de Ocupação`))+
  #geom_text(mapping = aes(x = `Taxa de Ocupação`), position = position_dodge2(width = 1.0), vjust = -0.5, size = 2 ) + 
  labs(x = "Intervalo" , y = "Quantidade de Unidades", title = "Taxa de Ocupação por Classe", fill = "")+ 
  scale_fill_manual(values = cores)+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_text(color = "Black", size = 5, face = "plain",angle = 0),
        legend.text  = element_text(color = "Black", size = 4, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 0), #TEXTO DA VARIAVEL EM X
        axis.text.y  = element_blank(), #SEM TEXTO DA VARIAVEL EM Y
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm") #TAMANHO DA CAIXA DA LEGENDA
  )

teste_normal <- rel2%>%
  filter(`Taxa de Ocupação` < 10)
shapiro.test(teste_normal$`Taxa de Ocupação`)


graf2.5 <- rel2 %>%
  group_by(`Intervalo de Taxa`)%>%
  summarise(`Quantidade` = n())%>%
  mutate(
    `Soma` = nrow(rel2),
    `Quantidade relativa` = round((`Quantidade` / `Soma`)*100, digits = 2),
    `Intervalo de Taxa` = factor(`Intervalo de Taxa`, levels = nivel_taxa )
    )%>%
  ggplot(aes(x=`Intervalo de Taxa`,y = log10(`Quantidade relativa`) , fill = `Quantidade relativa`, label = `Quantidade relativa` )) + 
  geom_bar(stat = "identity",show.legend = FALSE) + 
  geom_text(mapping = aes(label = `Quantidade relativa`), hjust = 1.0 ,vjust = 1.0, size = 1.5, nudge_x = 0.0) + 
  labs(x = "" , y = "", title = "Quantidade de Unidades por Classe de Taxa de Ocupação (%)", fill = "")+ 
  scale_fill_gradient(low = "#294661", high = "darkred")+  
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 0, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 0, face = "bold", angle = 90),
        legend.title = element_text(color = "Black", size = 5, face = "plain",angle = 0),
        legend.text  = element_text(color = "Black", size = 5, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 5, face = "plain",angle = 25, hjust = 0.5, vjust = 0.5 ), #TEXTO DA VARIAVEL EM X
        #axis.text.x = element_blank(),
        axis.text.y  = element_blank(), #SEM TEXTO DA VARIAVEL EM Y
        legend.position = "right", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm"), #TAMANHO DA CAIXA DA LEGENDA
        strip.text.x = element_text(size = 4, colour = "black", angle = 0) #ALTERA AS CARACTERISTICAS DO TITULO DAS VARIAVEIS
  )+
  coord_polar()

graf2.6 <- rel2 %>%
  group_by(`Intervalo de Taxa`)%>%
  summarise(`Quantidade de Unidades` = n(),
            `População Prisional` = sum(`População Prisional`))%>%
  gather(
    -`Intervalo de Taxa`,
    key = "Tipo",
    value = "Quantidade"
    )%>%
  mutate(`Intervalo de Taxa` = factor(`Intervalo de Taxa`, levels = nivel_taxa, ordered = TRUE))%>%
  ggplot()+
  geom_bar(aes(x=`Intervalo de Taxa`, y=log10(`Quantidade`), fill=`Tipo`), position = position_dodge2(width = 1.0),stat = "identity", show.legend = TRUE)+
  geom_text(mapping = aes(label = `Quantidade`, x=`Intervalo de Taxa`, y=log10(`Quantidade`)), hjust = 1.0, vjust = -0.5, size = 2.0, nudge_x = 0.0) +
  labs(x = "Intervalo" , y = "Quantidade de Unidades / População Prisional", title = "Comparação entre Quantidade de Unidades e População Prisional", fill = "")+ 
  scale_fill_manual(values = c("#294661","darkred"))+
  theme(plot.title   = element_text(color = "Black", size = 8, face = "bold", angle = 0, hjust = 0.5), #hjust CENTRALIZA O TEXTO
        axis.title.x = element_text(color = "Black", size = 8, face = "bold", angle = 0),
        axis.title.y = element_text(color = "Black", size = 8, face = "bold", angle = 90),
        legend.title = element_text(color = "Black", size = 5, face = "plain",angle = 0),
        legend.text  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        axis.text.x  = element_text(color = "Black", size = 6, face = "plain",angle = 45), #TEXTO DA VARIAVEL EM X
        axis.text.y  = element_text(color = "Black", size = 6, face = "plain",angle = 0),
        legend.position = "bottom", #POSICAO DA LEGENDA
        legend.key.size = unit(0.5,"cm") #TAMANHO DA CAIXA DA LEGENDA
  )
  
###########################################################################################################################
  
rel3 <- base_sisdepen %>%
  filter(
    A.Situação.de.Preenchimento == "Validado",
    A.Ano == 2020,
    #!=is.na(M.1.3.Capacidade.do.estabelecimento...Feminino...Total),
  )%>%
  mutate(
    `Tipo de Custódia`=ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime semiaberto)"), ignore_case = TRUE)))==TRUE,"Semiaberto",
                              ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime fechado)"), ignore_case = TRUE)))==TRUE,"Fechado",
                                     ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(regime aberto)"), ignore_case = TRUE)))==TRUE,"Aberto",
                                            ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(diversos tipos de regime)"), ignore_case = TRUE)))==TRUE,"Diversos",
                                                   ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(presos provisórios)"), ignore_case = TRUE)))==TRUE,"Provisórios",
                                                          ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(medida de segurança)"), ignore_case = TRUE)))==TRUE,"Medida de Segurança",
                                                                 ifelse((str_detect(A.1.2.Tipo.de.estabelecimento...originalmente.destinado,regex(c("(patronato)"), ignore_case = TRUE)))==TRUE,"Patronato",
                                                                        "Diversos"))))))),
    `Unidade Monitoramento` = ifelse((str_detect(A.Nome.do.Estabelecimento,regex(c("(monitoramento)|(monitora(ç|c)(ã|a)o)|(eletr(ô|o)nic(o|a))"),ignore_case=TRUE)))==TRUE,"Sim","Não"),
  )%>%
  group_by(
    A.Ano,
    A.Referencia,
    A.Situação.de.Preenchimento,
    A.UF,
    A.Nome.do.Estabelecimento,
    `Tipo de Custódia`,
    `Unidade Monitoramento`
  )%>%
  summarise(
    `Capacidade` = sum(M.1.3.Capacidade.do.estabelecimento...Masculino...Total,na.rm = TRUE) + 
                   sum(M.1.3.Capacidade.do.estabelecimento...Feminino...Total,na.rm = TRUE),
    `População Prisional` = M.4.1.População.prisional...Total,
    `Taxa de Ocupação` = round(M.4.1.População.prisional...Total / `Capacidade`,digits = 2),
    `Intervalo de Taxa` = ifelse(`Taxa de Ocupação`<=0.5, "Até 80%",
                           ifelse(`Taxa de Ocupação`<=0.9,"Entre 80% e 90%",
                            ifelse(`Taxa de Ocupação`<=1.0,"Entre 90% e 100%",
                             ifelse(`Taxa de Ocupação`<=1.1,"Entre 100% e 110%",
                              ifelse(`Taxa de Ocupação`<=1.2,"Entre 110% e 120%",
                               ifelse(`Taxa de Ocupação`<=1.3,"Entre 120% e 130%",
                                ifelse(`Taxa de Ocupação`<=1.4,"Entre 130% e 140%",
                                 ifelse(`Taxa de Ocupação`<=1.5,"Entre 140% e 150%",
                                  ifelse(`Taxa de Ocupação`<=1.6,"Entre 150% e 160%",
                                   ifelse(`Taxa de Ocupação`<=1.7,"Entre 160% e 170%",
                                    ifelse(`Taxa de Ocupação`<=1.8,"Entre 170% e 180%",
                                     ifelse(`Taxa de Ocupação`>1.8,"Maior que 180%",
                                         "Erro"))))))))))))
  )%>%
  filter(
    #A.Situação.de.Preenchimento %in% c("Validado")
    `Tipo de Custódia` %in% c("Diversos","Fechado","Medida de Segurança","Provisórios","Semiaberto"),
    `Unidade Monitoramento` == "Não"
  )
  
timestamp()
#busca identificar qual o número ideal de grupos
sil_width <- map_dbl(2:4,  function(k){
  model<- pam(x = rel3[,8:9], k = k)
  model$silinfo$avg.width
})
timestamp()

# MELHOR INDICE COM DOIS CLUSTER's
model<- pam(x = rel3[,8:9], k = 2)

#valor da width média
model$silinfo$avg.width  

#cria uma tabela 
rel3_cluster<-
  cbind(rel3,model$clustering)

names(rel3_cluster)[12] <- "Cluster"

#Faz o gráfico com escala logarítimica. É interessante em seguida ver o efeito do gráfico com escala linear

graf3.1_cluster<-
rel3_cluster %>%
  gather(key = "Variavel", value = "Quantidade", -c(1:7,10:12)) %>%
  ggplot(aes(x= Variavel, y= Quantidade )) +
  geom_line(aes(group = `A.Nome.do.Estabelecimento`,  color= factor(`Cluster`))) +
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

#Informa o número de elementos por agrupamento formado

graf3.2_cluster<-
rel3_cluster %>%  
  group_by(`Cluster`) %>%
  summarise(
    quantidade= n()
  ) %>%
  ggplot()  +
  geom_col(aes(x= quantidade, y=factor(`Cluster`),  fill= factor(`Cluster`))) +
  geom_text(mapping = aes(label = `quantidade`, x=`quantidade`,y=`Cluster`), hjust = 1.0, vjust = -0.5, size = 3.0) +
  labs(x = "Cluster's" , y = "Quantidade de Unidades por Cluster", title = "Quantidade de Unidades por Cluster", fill = "")+ 
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

#Informa a populaçao prisional por cluster

graf3.3_cluster<-
  rel3_cluster%>%
  gather(key = "Variavel", value = "Quantidade", -c(1:8,10:12)) %>%  
  group_by(`Cluster`) %>%
  summarise(
    `População por Cluster`= sum(`Quantidade`, na.rm = TRUE)
  )%>%
  ggplot()+
  geom_col(aes(x=factor(`Cluster`), y= `População por Cluster`, fill= factor(`Cluster`)))+
  geom_text(mapping = aes(label = `População por Cluster`, x=`Cluster`,y=`População por Cluster`), hjust = 1.0, vjust = -0.5, size = 3.0) +
  labs(x = "Cluster's" , y = "Quantidade de Presos por Cluster", title = "População Prisonal por Cluster", fill = "")+ 
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

setwd("C:/Users/lucas.eneas/OneDrive - MINISTERIO DA JUSTIÇA/analise_dados/taxa_ocupacao/graficos/graficos2")
ggsave(graf1, filename = "graf1.pdf", width = 18, height = 12, units = "cm")
ggsave(graf1.1, filename = "graf1_1.pdf", width = 18, height = 14, units = "cm")
ggsave(graf1.2, filename = "graf1_2.pdf", width = 18, height = 12, units = "cm")
ggsave(graf2, filename = "graf2.pdf", width = 18, height = 14, units = "cm")
ggsave(graf2.1, filename = "graf2_1.pdf", width = 18, height = 14, units = "cm")
ggsave(graf2.2, filename = "graf2_2.pdf", width = 18, height = 14, units = "cm")
ggsave(graf2.3, filename = "graf2_3.pdf", width = 18, height = 14, units = "cm")
ggsave(graf2.4, filename = "graf2_4.pdf", width = 18, height = 14, units = "cm")
ggsave(graf2.5, filename = "graf2_5.pdf", width = 18, height = 14, units = "cm")
ggsave(graf2.6, filename = "graf2_6.pdf", width = 18, height = 14, units = "cm")
ggsave(graf3.1_cluster, filename = "graf3.1_cluster.pdf", width = 18, height = 14, units = "cm")
ggsave(graf3.2_cluster, filename = "graf3.2_cluster.pdf", width = 18, height = 14, units = "cm")
ggsave(graf3.3_cluster, filename = "graf3.3_cluster.pdf", width = 18, height = 14, units = "cm")

ggsave(graf1, filename = "graf1.png", width = 18, height = 12, units = "cm")
ggsave(graf1.1, filename = "graf1_1.png", width = 18, height = 14, units = "cm")
ggsave(graf1.2, filename = "graf1_2.png", width = 18, height = 12, units = "cm")
ggsave(graf2, filename = "graf2.png", width = 18, height = 14, units = "cm")
ggsave(graf2.1, filename = "graf2_1.png", width = 18, height = 14, units = "cm")
ggsave(graf2.2, filename = "graf2_2.png", width = 18, height = 14, units = "cm")
ggsave(graf2.3, filename = "graf2_3.png", width = 18, height = 14, units = "cm")
ggsave(graf2.4, filename = "graf2_4.png", width = 18, height = 14, units = "cm")
ggsave(graf2.5, filename = "graf2_5.png", width = 18, height = 14, units = "cm")
ggsave(graf2.6, filename = "graf2_6.png", width = 18, height = 14, units = "cm")
ggsave(graf3.1_cluster, filename = "graf3.1_cluster.png", width = 18, height = 14, units = "cm")
ggsave(graf3.2_cluster, filename = "graf3.2_cluster.png", width = 18, height = 14, units = "cm")
ggsave(graf3.3_cluster, filename = "graf3.3_cluster.png", width = 18, height = 14, units = "cm")


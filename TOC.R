dadosTOC <- read.csv("ocd_patient_dataset.csv")
library(ggplot2)

#DEPRESSÂO
table(dadosTOC$Depression.Diagnosis)
  ggplot(dadosTOC, aes(x=Depression.Diagnosis)) + 
    geom_bar(fill = "#69b3a2", width = 0.20)+
    scale_y_continuous(limits = c(0, 800) , breaks = seq(0, 800, 100))+
    labs(title = "Diagnóstico de Depressão", y = "Quantidade", x = "Indivíduos com depressão")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_text(aes(label = scales::percent(..count../sum(..count..))), 
              stat = "count", vjust = -0.5)
  
#ANSIEDADE
table(dadosTOC$Anxiety.Diagnosis)
ggplot(dadosTOC, aes(x=Anxiety.Diagnosis)) + 
  geom_bar(fill = "#6973a2", width = 0.20)+
  scale_y_continuous(limits = c(0, 800) , breaks = seq(0, 800, 100))+
  labs(title = "Diagnóstico de Ansiedade", y = "Quantidade", x = "Indivíduos com ansiedade")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = scales::percent(..count../sum(..count..))), 
            stat = "count", vjust = -0.5)

#Com depressão e ansiedade e com nenhum 
dadosTOC$comb_diagnostico <- ifelse(dadosTOC$Anxiety.Diagnosis == "Yes" & dadosTOC$Depression.Diagnosis == "Yes", "Ambos",
                                    ifelse(dadosTOC$Anxiety.Diagnosis == "No" & dadosTOC$Depression.Diagnosis == "No", "Nenhum", NA))
dados_filtrados <- dadosTOC[dadosTOC$comb_diagnostico %in% c("Ambos", "Nenhum"), ]
ggplot(dados_filtrados, aes(x = comb_diagnostico, fill = comb_diagnostico)) +
  geom_bar(width = 0.20) +
  scale_y_continuous(limits = c(0, 500) , breaks = seq(0, 500, 100))+
  labs(title = "Combinação de Diagnósticos",
       x = "Combinação",
       y = "Contagem") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") +
  geom_text(aes(label = scales::percent(..count../sum(..count..))), 
            stat = "count", vjust = -0.5)+
  scale_fill_manual(values = c("Ambos" = "#69b3a2", "Nenhum"="#6973a2"))


tabe <- table(dadosTOC$Depression.Diagnosis, dadosTOC$Anxiety.Diagnosis)

print(tabe)

resultado_teste <- chisq.test(tabe)
print(resultado_teste)












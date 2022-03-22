# script para estimar a volatilidade usando o GARCH(1,1)
library(readxl) # para ler Excel
library(rugarch) # rodar GARCH
library(ggplot2) # fazer grafico
library(xts) # para converter o dataframe em xts
# inserir a library(tseries) para fazer testes na serie temporal de retornos
# ler os dados da planilha Excel

df = read_xlsx('garchibovespa.xlsx')

# calcular os retornos simples e criar variavel de serie de tempo xts
df2 = as.data.frame(df$Data[2:length(df$Data)])
# df2 tem as DATAS defasadas de 1 observacao, e df3 tem os retornos simples
df3 = as.data.frame((df$`IBOVESPA (Pt)`[2:length(df$`IBOVESPA (Pt)`)]/df$`IBOVESPA (Pt)`[1:(length(df$`IBOVESPA (Pt)`)-1)])-1)
# juntar os 2 dataframes no df2
df4=cbind(df2,df3)
colnames(df4)=c('data','retornos')
# criar variavel xts com retornos
rets = xts(df4[,-1], order.by = df4[,1])
colnames(rets)='retornos'

# rodar o GARCH

garch.model = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), 
                         mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                         distribution.model = "std")

fit.garch = ugarchfit(spec = garch.model, data = rets)

omega = fit.garch@fit$coef['omega']
alfa = fit.garch@fit$coef['alpha1']
beta = fit.garch@fit$coef['beta1']
gama = 1 - alfa - beta
VL = omega/gama
VLanualizado = sqrt(VL*252)

# fazer o grafico
dev.off()
ggplot(rets,aes(x = df4$data)) +
  geom_line(aes(y = (fit.garch@fit[["sigma"]]*sqrt(252)*100)),
            color = "steelblue") +
  geom_line(aes(y = VLanualizado*100), color = "darkred") +
  labs(y = "Volatilidade Anualizada (% a.a.)", x = "Data")









  
  

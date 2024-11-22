#Teste-Estágio-Analytics-QUOD
#Estevam Coelho Lopes

#############################################PARTE 1: Programação em R

############################ 1°: LIMPEZA E ANÁLISE DE DADOS DE VENDAS

#bibliotecas
library(tidyverse)
library(scales)
library(lubridate)

#Diretório
setwd("C:/Users/estevam1/Downloads")


############### 1.1 Criando dados simulados
#semente
set.seed(100)

#DATA
seq_datas = seq.Date(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")
data = sample(seq_datas,size = 120,replace = T)

#ID
#sequencia de 120 valores de 1 até 120.
ID = seq(1,120)

#Produto
tipos_produtos = c("A","B","C","D")#4 produtos.
#Criando amostra com 120 valores com os 4 produtos.
produtos = sample(tipos_produtos,size = 120,replace = T)

#OBS: Vamos manipular as quantidades para adicionar uma tendência que irá aumentar com o passar dos meses!

#Quantidade
#A função format em datas pode extrair meses, por exemplo!
#criamos essa variável para adicionar a tendência.
meses <- as.numeric(format(data, "%m"))

#A função round irá arredondar para um número inteiro.
#A função rnorm irá gerar dados aleatórios de uma distribuição Normal com média 60 e variância 30.
#Além disso, adicionamos uma tendência que aumenta a cada mês!!
quantidade <- round(rnorm(length(data), mean = 60, sd = 30) + (meses * 10)) 
quantidade[quantidade < 0] <- 0

#Criando base de dados com as variáveis: Data, Produto, Categoria e Quantidade.
base = tibble("Data" = data,"Produto" = produtos,
              "Quantidade" = quantidade)

#Adicionando a variável "ID".
base$ID = seq(1,120)

#Usando a função "mutate" para criar a variável "Preco", baseado em cada produto!
base = base |> 
  mutate(Preco = ifelse(Produto == "A",100,
                 ifelse(Produto == "B",150,
                 ifelse(Produto == "C",600,
                 ifelse(Produto == "D",200, 0)))))

#Usando a função "mutate" para criar a variável "Categoria", baseado em cada produto!
base = base |> 
  mutate(Categoria = ifelse(Produto == "A" | Produto == "D",1,
                     ifelse(Produto == "B" | Produto == "C",2, 0)))

#Ordenando as colunas!
base = base |> 
  select(ID,Data,Produto,Categoria,Quantidade,Preco)

#Ordenando a base pela variável "Data"
base = base |> 
  arrange(Data)
########### 1.1.2 Criando problemas para corrigir depois!

#Criando valores faltantes
#:Vamos criar alguns valores faltantes em "Quantidade" e/ou em "Produto/Preco".
#Obs: precisamos criar NA's igualmente em Produto/Preço.
base$Preco[1] = NA; base$Produto[1] = NA
base$Preco[9] = NA; base$Produto[9] = NA
base$Preco[37] = NA; base$Produto[37] = NA
base$Quantidade[14] = NA; base$Quantidade[27] = NA

#Criando duplicatas
base = rbind(base,base[c(2,7,32),])


############### 1.2 Tratamento de valores faltantes

###Como eu trataria valores faltantes?
#Podemos remover linhas ou substituir pela média/mediana.A solução depende de cada caso em específico.
#Minha recomendação: Remover Linhas com dados faltantes.  Como temos menos de 5% de dados faltantes,
#isto não prejudicaria muito o conjunto de dados.

#a função complete.cases seleciona linhas sem NA's.
base2 =  base[complete.cases(base[,c("Preco","Quantidade","Produto")]),]

############### 1.3 Remoção de Duplicatas
base3 = unique(base2)#A função "unique" seleciona as linhas únicas.

#Após isso, vamos reorganizar a variável ID para ir de 1 até 115.
base3$ID = seq(1,115)

############### 1.4 Conversão de tipo de dados
#Vamos transformar as variáveis "Produto" e "Categoria" em factors.
#Poderiamos trocar os nomes dos produtos e categorias também!
base3$Produto <- as.factor(base3$Produto)
base3$Categoria <- as.factor(base3$Categoria)

############### 1.5 Salvando como data_clean.csv
#write.csv(base3,"dados_clean.csv")

############### 1.6 Abrindo a nova base de dados:
#Como eu iria ler a base de dados:
dados = read_csv("dados_clean.csv")#Lendo a base dados_clean
dados = dados[,-1]#Tirando a 1° coluna, que tem valores errados.

#transformando produto e categoria da base dados em Factor.
dados$Produto <- as.factor(dados$Produto)
dados$Categoria <- as.factor(dados$Categoria)

#Criando a variável total_vendas = qt*preço.
dados = dados |> 
  mutate(Total_Vendas = Quantidade*Preco)

#Vamos retirar os dados em que temos vendas = 0!!
dados = dados |> 
  filter(Total_Vendas > 0)

############### 1.8  Identifique o produto com o maior número de vendas totais.

#Obs: Não entendi se é pedido o produto que teve maior número de quantidade de vendas
#Ou maior número de valores de vendas totais. Portanto, farei para ambos!

#Vamos criar uma tabela para total de vendas e total de quantidades de cada Produto!
#group_by agrupa os dados por cada produto.
#summarise vai criar funções para cada variável!
Vendas_T = dados |> 
  group_by(Produto) |> 
  summarise(Total_Vendas = sum(Total_Vendas),
            Total_Quantidades = sum(Quantidade))
#RESULTADOS:
Vendas_T


#O Produto C teve o maior total de vendas = 2.046.000
#O produto D vendeu a maior quantidade = 4.689
unique(dados$Preco)

############################ 2°: Visualização de Dados!

#1° Gráfico de barras para vendas totais por produto
ggplot(dados, aes(x = Produto, y = Total_Vendas, fill = Produto)) +
  geom_bar(stat = "identity") + #gráfico de barras
  labs(title = "Vendas Totais por Produto",x = "Produto", y = "Total de Vendas (em R$)") + #legendas
  scale_y_continuous(labels = comma)+ #botando vírgula, com pacote scales, no Y.
  theme_minimal()+
  theme(legend.position = "none")#tirando a legenda!!
  

#2° Gráfico de barras de vendas totais por categoria!
ggplot(dados, aes(x = Categoria, y = Total_Vendas, fill = Categoria)) +
  geom_bar(stat = "identity")+ #gráfico de barras
  theme_minimal()+              
  labs(title = "Vendas Totais por Categoria",x = "Categoria",y = "Total de Vendas (em R$)") +#legendas!!
   scale_y_continuous(labels = comma,limits = c(0,3000000))+#botando vírgula, com pacote scales, no Y.
  theme(legend.position = "none")#tirando a legenda!!


#3° gráfico de séries temporais para quantidade ao longo do tempo.
dados |> 
  ggplot(mapping = aes(x = Data, y = Quantidade)) +
  geom_line()+#gráfico de linhas
  labs(title = "Serie Temporal de Quantidade de Vendas ao Longo do Tempo(2023)",x = "Data",y = "Quantidade")+#legendas
  scale_x_date(date_breaks = "3 months",date_labels = "%b")#ajustando legenda do "X".


dados
############## 2.1: Identificando padrões e insights!
#A categoria 2 e o Produto C tem as maiores vendas totais.
#Em relação a quantidade de vendas ao longo do tempo, observa-se uma tendência que aumenta ao longo dos meses.

#Salvando arquivo para sql:
#write.csv(dados,"dados_sql.csv")

install.packages('knitr')

#filtrando para o mês de junho:
Vendas_T_Jun = dados |> 
  filter(Data >= as.Date("2023-06-01") & Data <= as.Date("2023-06-30")) |> 
  group_by(Produto) |> 
  summarise(Total_Vendas = sum(Total_Vendas),
            Total_Quantidades = sum(Quantidade)) |> 
  arrange(Total_Vendas)
#RESULTADOS:
Vendas_T_Jun

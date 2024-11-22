--Vamos examinar as primeiras 10 linhas da base de dados.
select top(10) *
  from dados_sql


 ---------------Tarefa 1:
 --listar o nome do produto, categoria e soma total de vendas, para cada produto. Ordenar pelo total de vendas em ordem decrescente.
select
    Produto,
    Categoria,
	sum(Total_Vendas) as Soma_Total_Vendas--Somei o total das vendas para cada produto.
from	
	dados_sql
group by
	Produto, Categoria--Agrupei por produto e categoria.
order by
    Soma_Total_Vendas desc;--ordenei de maneira decrescente pela soma_total_vendas

---------------Tarefa 2:
--Identificar os produtos que venderam menos em jun/2023
select
	Produto, 
	Categoria,
	sum(Total_Vendas) as Soma_Total_Vendas_jun_2023--Somei o total das vendas para cada produto.
from
	dados_sql
where
	month(Data) = 6 and year(Data) = 2023--Filtrei para jun/2023
group by
	Produto, Categoria--Agrupei por produto e categoria.

-- O Produto "A" Foi o que vendeu menos em jun/2023!!
---
title: "TranferBR"
author: Lucas E. O. Silva
output: 
  html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
require(knitr)
```
# TranferBR
#### Autor: Lucas E. O. Silva

### Objeto selecionado:

As transferências de recursos da União para os municípios brasileiros disponíveis no [Portal Transparência](http://www.portaltransparencia.gov.br/). 

### O que foi feito:

Foi criada a função `coletarTransparenciaAno(ano, uf)` para extrair as informações referentes a todas referências realizadas pela União aos municípios.

O parâmetro `ano` se refere ao exercício fiscal o qual as transferências foram realizadas, enquanto `uf` se refere ao estado de interesse do pesquisador.

### Explicando o passo-a-passo...

#### Carregamento das bibliotecas:
O trecho abaixo carrega as bibliotecas necessárias para a execução do código. Caso o usuário não as tenha, ela será carregada automaticamente.

O pacote *XML* contém a função `readHTMLTable()`, necessária para a extração das informações propriamente ditas. Por sua vez, o pacote *httr* contém a função `GET()`, usada para avaliar a requisição das páginas .html e apontar possíveis erros. O pacote *tidyr* contém a função `separate()` para a quebra das strings. Por fim, *beepr* avisa quando o procedimento é finalizado.
```{r eval=F}
## Carrregamento automático das bibliotecas
if(require(XML) == F){
  install.packages("XML")
}
library(XML)

if(require(httr) == F){
  install.packages("httr")
}
library(httr)

if(require(tidyr) == F){
  install.packages("tidyr")
}
library(tidyr)

if(require(beepr) == F){
  install.packages("beepr")
}
library(beepr)
```


#### Objeto siafi:

O objeto siafi é um data.frame que contém o nome, estado e código de todos os municípios brasileiros. O Sistema Integrado de Administração Financeira do Governo Federal (SIAFI) é um sistema contábil do Tesouro Nacional que realiza todo o processamento, controle e execução financeira do governo brasileiro. Todos os munícipios possuem um código dentro desse sistema que sempre é utilizado nas transferências de recursos. A Tabela `r tabs(name="tab01",display="num")` exibe a codificação de alguns casos. 

```{r}
# carregamento da base que contem os codigos siafi dos municipios
  arquivo <- "https://raw.githubusercontent.com/lucaseosilva/transfBR/master/siafi.csv"
  siafi <- read.table(arquivo, header = T, sep = ",", stringsAsFactors = F)
```

```{r echo=F, message=F}
kable(as.data.frame(head(siafi,n=10)), align = 'c', caption = tabs(name="tab01","Códigos SIAFI"))
```

##### Seleção do estado

Em seguida, `uf_cidades` irá armazenar as informações do SIAFI referentes aos municípios do estado informado pelo usuário. Todas essas as transferências ficarão armazenadas no objeto `agregado`, que será aparecerá novamente depois. 
```{r eval=F}
# selecionando os municipios da uf selecionada pelo usuario
  uf_cidades <- siafi[siafi$uf==uf, ]
  
# frame em branco
  agregado <- data.frame()
```

#### Estruturas de repetição: 

As estruturas de repetição servem para automatizar a extração das informações. O `for` produzirá interações referentes à quantidade de municípios no estado solicitado. Se `uf = "AC"`, por exemplo, essa estrutura de repetição interagirá 22 vezes, já que o estado do Acre possui 22 municípios.

O `while` é refente à quantidade de páginas de exibição que um município possui. Em função da quantidade de transferências ser um número razoavelmente elevado, ao invés de serem exibidas em uma só página, elas são quebradas em várias outras. A estrutura não possui um limite fixo e em cada novo município analisado seu valor será sempre 1. O ponto de quebra dessa interação será a partir do momento em que ela solicitar uma página que não existe, isso quer dizer que a quantidade de transferências para um determinado município acabou e, em seguida, um novo município será analisado.

```{r eval=F}
# percorrendo pela quantidade de municipios presentes naquela uf
  for(i in 1:nrow(uf_cidades)){
    # percorrendo pela quantidade de paginas referentes às transferências naquele mun.
      pagina <- 1
      while(pagina>=1){
        ...
      
        # verificando se aquela uma determinada pag. existe
        # caso não haja, a estrutura de repetição eh quebrada e algoritmo avanca para o prox. mun
        response <- GET(url)
        if (response$status_code!=200){
          break
        }
      
        ...
        
        # contador 
        pagina <- pagina + 1
    }
}
```


#### Parametrização das URLs:

O próximo passo é imputar o valor dos parâmetros dentro da url do Portal Transferência que se refere ao repasse dos recursos. A função `gettextf()` é utilizada com o intuito de substituir os *placesholder* (%s) presentes no link pelo respectivo valor dos parâmetros.

```{r eval=F}
# estrutura de decisão para incrementar um zero à esquerda nos municipios com o cod <1000
      if(nchar(as.character(uf_cidades$codigo[i]))>=4){
        url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=%s&Pagina=%s", 
                        ano, uf, uf_cidades$codigo[i], pagina)      
      }
      else{
        url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=0%s&Pagina=%s", 
                        ano, uf, uf_cidades$codigo[i], pagina)      
      }
```

Detalhe: essa estrutura de decisão foi utilizada em função da distribuição dos valores do código SIAFI. Um 0 à esquerda é adicionado em municípios cujo seu valor é < 1000. Isso faz com que todos eles tenham 4 dígitos e se adequem ao formato utilizado no Portal. 


#### Extração e seleção:

A função `readHTMLTable()` realiza a captura de todas as tabelas presentes na url informada. É importante ressaltar que `stringsAsFactors = F` é utilizado para considerar todas as strings como texto - e não como levels pelo default - e a codificação `UTF-8` serve para que o software interprete as possíveis acentuações existentes no texto.

A variável `lista.tabelas` contém uma lista com todas as tabelas existentes na página. Contudo, as informações de interesse estão presentes estão presentes somente no segundo elemento dessa lista.

```{r eval=F}
# lendo as tabelas da pag
      lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")
      
# selecionando a tabela de interesse
      tabela <- lista.tabelas[[2]]
```


#### Adicionando variáveis:

A tabela de interesse na página, originalmente, possui 4 variáveis:

- *Função*: refere-se a destinação social da verba (Saúde, Assistência social, Educação, etc. );
- *Ação governamental*: qual programa do governo federal estará recebendo o recurso;
- *Linguagem cidadã*: descreve como o programa é socialmente conhecido;
- *Total ano*: o valor total transferido para um determinado programa durante o ano.

Em seguida, outras 4 variáveis são adicionadas para facilitar e padronizar a visualização das observações no `data.frame` de retorno:

- *Ano*: o ano informado pelo usuário;
- *Uf*: o estado informado pelo usuário;
- *Código SIAFI*: o código siafi dos municípios analisados;
- *Nome município*: o nome dos municípios de um determinado estado.

```{r eval=F}
# acrescentando algumas infos
    tabela$ano <- ano
    tabela$uf <- uf
    tabela$codSiafi <- uf_cidades$codigo[i]
    tabela$nomeMunicipio <- uf_cidades$cidade[i]
```

#### Concatenando informações:

O objeto `tabela` contém todas as transferências para um determinado município. O código abaixo está inserido dentro de uma estrutura de repetição onde, a cada interação, o data.frame `agregado` irá concatenar as informações de todos os municípios. 

```{r eval=F}
# agregando no frame final
      agregado <- rbind(agregado, tabela)
```


#### Finalizando data frame:
Por fim, alguns retoques estéticos são feitos no data frame. A ordem das colunas é invertida e as variáveis são renomeadas antes do objeto ser retornado na função.

Além disso, uma nova variável é inserida `COD_ACAO_GOV`, que identifica uma determinada ação governamental. Originalmente, esse atributo vem junto com a descrição da ação na tabela. Em função disso, a variável é "quebrada" para fazer essa separação.

A variável `TOTAL_VALOR_REAIS` foi convertida de `character` para `numeric`. Para isso, os indicadores de milhar foram removidos e os separadores decimais foram transformados em ponto. 
```{r eval=F}
# modificando a ordem das variaveis
  agregado <- agregado[,c(5:8,1:4)]
  
# renomeando as vars
  names(agregado) <- c("ANO", "UF", "COD_SIAFI", "MUNICIPIO", "FUNCAO","ACAO_GOV","LINGUAGEM_CIDADA","TOTAL_VALOR_REAIS")
  
# quebrando ACAO_GOV em COD_ACAO_GOV e DESC_ACAO_GOV
  agregado <- separate(agregado, ACAO_GOV, c('COD_ACAO_GOV', 'DESC_ACAO_GOV'), sep=' - ', remove=TRUE)
  
# transformando TOTAL_VALOR_REAIS em numero
  agregado$TOTAL_VALOR_REAIS <- gsub("\\.","", agregado$TOTAL_VALOR_REAIS)
  agregado$TOTAL_VALOR_REAIS <- as.numeric(gsub(",", ".", agregado$TOTAL_VALOR_REAIS))
  
### avisa que está pronto!
  beep(4)
  
#retornando o frame resultante
  return (agregado)
```

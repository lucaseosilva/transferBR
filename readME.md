Desafio 2 - Webscraping
================

### Informações do aluno:

-   **Nome**: Lucas E. O. Silva
-   **Programa**: Mestrado em Ciência Política
-   **Número USP**: 10236603
-   **Data de entrega**: 27/04/2017

### Objeto selecionado:

As transferências de recursos da União para os municípios brasileiros disponíveis no [Portal Transparência](http://www.portaltransparencia.gov.br/). A Figura 1 ilustra como os dados estão disponibilizados na página.

**Figura 1: Diagramação das informações no site do Transparência** <img src="captura.jpg" align="center" height="300" width="500">

### O que foi feito:

Foi criada a função `coletarTransparenciaAno(ano, uf)` para extrair as informações referentes a todas referências realizadas pela União aos municípios.

O parâmetro `ano` se refere ao exercício fiscal o qual as transferências foram realizadas, enquanto `uf` se refere ao estado de interesse do pesquisador.

### Código:

``` r
if(require(XML) == F){
  install.packages("XML")
}
```

    ## Loading required package: XML

``` r
library(XML)

if(require(httr) == F){
  install.packages("httr")
}
```

    ## Loading required package: httr

    ## Warning: package 'httr' was built under R version 3.4.0

``` r
library(httr)

if(require(tidyr) == F){
  install.packages("tidyr")
}
```

    ## Loading required package: tidyr

``` r
library(tidyr)


coletarTransparenciaAno <- function(ano, uf) {

  siafi <- read.csv("C://Users//user03//Desktop//markdown//siafi.csv", header = T, sep=";")

  uf_cidades <- siafi[siafi$uf==uf, ]

  agregado <- data.frame()

  for(i in 1:length(uf_cidades)){
    pagina <- 1
    while(pagina>=1){
      
      if(nchar(as.character(uf_cidades$codigo[i]))>=4){
        url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=%s&Pagina=%s", 
                        ano, uf, uf_cidades$codigo[i], pagina)      
      }
      else{
        url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=0%s&Pagina=%s", 
                        ano, uf, uf_cidades$codigo[i], pagina)      
      }
      
      response <- GET(url)
      if (response$status_code!=200){
        break
      }
      
      lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")
      
      tabela <- lista.tabelas[[2]]
      
      tabela$ano <- ano
      tabela$uf <- uf
      tabela$codSiafi <- uf_cidades$codigo[i]
      tabela$nomeMunicipio <- uf_cidades$cidade[i]
      
      agregado <- rbind(agregado, tabela)
      
      pagina <- pagina+1
    }
  }
  
  agregado <- agregado[,c(5:8,1:4)]
  names(agregado) <- c("ANO", "UF", "COD_SIAFI", "MUNICIPIO", "FUNCAO","ACAO_GOV","LINGUAGEM_CIDADA","TOTAL_VALOR_REAIS")
  agregado <- separate(agregado, ACAO_GOV, c('COD_ACAO_GOV', 'DESC_ACAO_GOV'), sep=' - ', remove=TRUE)
  
  agregado$TOTAL_VALOR_REAIS <- gsub("\\.","", agregado$TOTAL_VALOR_REAIS)
  agregado$TOTAL_VALOR_REAIS <- as.numeric(gsub(",", ".", agregado$TOTAL_VALOR_REAIS))

  return (agregado)

}
```

### Explicando...

#### 1. Carregamento das bibliotecas:

O trecho abaixo carrega as bibliotecas necessárias para a execução do código. Caso o usuário não as tenha, ela será carregada automaticamente.

O pacote *XML* contém a função `readHTMLTable()`, necessária para a extração das informações propriamente ditas. Por sua vez, o pacote *httr* contém a função `GET()`, usada para avaliar a requisição das páginas .html e apontar possíveis erros. O pacote *tidyr* contém a função `separate()` para a quebra das strings.

``` r
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
```

#### 2. Objeto siafi:

O objeto siafi é um data.frame que contém o nome, estado e código de todos os municípios brasileiros. O Sistema Integrado de Administração Financeira do Governo Federal (SIAFI) é um sistema contábil do Tesouro Nacional que realiza todo o processamento, controle e execução financeira do governo brasileiro. Todos os munícipios possuem um código dentro desse sistema que sempre é utilizado nas transferências de recursos. A Tabela 1 exibe a codificação de alguns casos.

``` r
siafi <- read.csv("C://Users//user03//Desktop//markdown//siafi.csv", header = T, sep=";")
```

**Tabela 1: Códigos SIAFI**

|  codigo| cidade          | uf  |
|-------:|:----------------|:----|
|     643| ACRELANDIA      | AC  |
|     157| ASSIS BRASIL    | AC  |
|     105| BRASILEIA       | AC  |
|     645| BUJARI          | AC  |
|     647| CAPIXABA        | AC  |
|     107| CRUZEIRO DO SUL | AC  |
|     651| EPITACIOLANDIA  | AC  |
|     113| FEIJO           | AC  |
|     653| JORDAO          | AC  |
|     109| MANCIO LIMA     | AC  |

##### 2.1 Seleção do estado

Em seguida, `uf_cidades` irá armazenar as informações do SIAFI referentes aos municípios do estado informado pelo usuário. Todas essas as transferências ficarão armazenadas no objeto `agregado`, que será aparecerá novamente depois.

``` r
uf_cidades <- siafi[siafi$uf==uf, ]

agregado <- data.frame()
```

#### 3. Estruturas de repetição:

As estruturas de repetição servem para automatizar a extração das informações. O `for` produzirá interações referentes à quantidade de municípios no estado solicitado. Se `uf = "AC"`, por exemplo, essa estrutura de repetição interagirá 22 vezes, já que o estado do Acre possui 22 municípios.

O `while` é refente à quantidade de páginas de exibição que um município possui. Em função da quantidade de transferências ser um número razoavelmente elevado, ao invés de serem exibidas em uma só página, elas são quebradas em várias outras. A estrutura não possui um limite fixo e em cada novo município analisado seu valor será sempre 1. O ponto de quebra dessa interação será a partir do momento em que ela solicitar uma página que não existe, isso quer dizer que a quantidade de transferências para um determinado município acabou e, em seguida, um novo município será analisado.

``` r
for(i in 1:length(uf_cidades)){
    pagina <- 1
    while(pagina>=1){
      ...
      
      response <- GET(url)
      if (response$status_code!=200){
        break
      }
      
      ...
      
      pagina <- pagina + 1
    }
}
```

#### 4. Parametrização das URLs:

O próximo passo é imputar o valor dos parâmetros dentro da url do Portal Transferência que se refere ao repasse dos recursos. A função `gettextf()` é utilizada com o intuito de substituir os *placesholder* (%s) presentes no link pelo respectivo valor dos parâmetros.

``` r
if(nchar(as.character(uf_cidades$codigo[i]))>=4){
        url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=%s&Pagina=%s", 
                        ano, uf, uf_cidades$codigo[i], pagina)      
      }
      else{
        url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=0%s&Pagina=%s", 
                        ano, uf, uf_cidades$codigo[i], pagina)      
      }
```

Detalhe: essa estrutura de decisão foi utilizada em função da distribuição dos valores do código SIAFI. Um 0 à esquerda é adicionado em municípios cujo seu valor é &lt; 1000. Isso faz com que todos eles tenham 4 dígitos e se adequem ao formato utilizado no Portal.

#### 5. Extração e seleção:

A função `readHTMLTable()` realiza a captura de todas as tabelas presentes na url informada. É importante ressaltar que `stringsAsFactors = F` é utilizado para considerar todas as strings como texto - e não como levels pelo default - e a codificação `UTF-8` serve para que o software interprete as possíveis acentuações existentes no texto.

A variável `lista.tabelas` contém uma lista com todas as tabelas existentes na página. Contudo, as informações de interesse estão presentes estão presentes somente no segundo elemento dessa lista.

``` r
lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")

tabela <- lista.tabelas[[2]]
```

#### 6. Adicionando variáveis:

A tabela de interesse na página, originalmente, possui 4 variáveis:

-   *Função*: refere-se a destinação social da verba (Saúde, Assistência social, Educação, etc. );
-   *Ação governamental*: qual programa do governo federal estará recebendo o recurso;
-   *Linguagem cidadã*: descreve como o programa é socialmente conhecido;
-   *Total ano*: o valor total transferido para um determinado programa durante o ano.

Em seguida, outras 4 variáveis são adicionadas para facilitar e padronizar a visualização das observações no `data.frame` de retorno:

-   *Ano*: o ano informado pelo usuário;
-   *Uf*: o estado informado pelo usuário;
-   *Código SIAFI*: o código siafi dos municípios analisados;
-   *Nome município*: o nome dos municípios de um determinado estado.

``` r
  tabela$ano <- ano
  tabela$uf <- uf
  tabela$codSiafi <- uf_cidades$codigo[i]
  tabela$nomeMunicipio <- uf_cidades$cidade[i]
```

#### 7. Concatenando informações:

O objeto `tabela` contém todas as transferências para um determinado município. O código abaixo está inserido dentro de uma estrutura de repetição onde, a cada interação, o data.frame `agregado` irá concatenar as informações de todos os municípios.

``` r
agregado <- rbind(agregado, tabela)
```

#### 8. Finalizando data frame:

Por fim, alguns retoques estéticos são feitos no data frame. A ordem das colunas é invertida e as variáveis são renomeadas antes do objeto ser retornado na função.

Além disso, uma nova variável é inserida `COD_ACAO_GOV`, que identifica uma determinada ação governamental. Originalmente, esse atributo vem junto com a descrição da ação na tabela. Em função disso, a variável é "quebrada" para fazer essa separação.

A variável `TOTAL_VALOR_REAIS` foi convertida de `character` para `numeric`. Para isso, os indicadores de milhar foram removidos e os separadores decimais foram transformados em ponto.

``` r
  agregado <- agregado[,c(5:8,1:4)]
  names(agregado) <- c("ANO", "UF", "COD_SIAFI", "MUNICIPIO", "FUNCAO","ACAO_GOV","LINGUAGEM_CIDADA","TOTAL_VALOR_REAIS")
  agregado <- separate(agregado, ACAO_GOV, c('COD_ACAO_GOV', 'DESC_ACAO_GOV'), sep=' - ', remove=TRUE)
  
  agregado$TOTAL_VALOR_REAIS <- gsub("\\.","", agregado$TOTAL_VALOR_REAIS)
  agregado$TOTAL_VALOR_REAIS <- as.numeric(gsub(",", ".", agregado$TOTAL_VALOR_REAIS))
  
  return (agregado)
```

### Execução

O usuário executa todo o código através dos comandos `Ctrl + A`, em seguida `Crtl + Enter`. Ela ficará armazenada no Global Environment.

Ao fim, o usuário pode executar a função dentro do console ou em outro código que esteja aberto informando o seguinte comando: `coletarTransparenciaAno(2016, "AC")`. A Tabela ilustra os resultados obtidos no data frame.

**Tabela 2: Transferências realizadas para municípios do Acre em 2016 **

|   ANO| UF  |  COD\_SIAFI| MUNICIPIO  | FUNCAO              | COD\_ACAO\_GOV | DESC\_ACAO\_GOV                                                                                                                             | LINGUAGEM\_CIDADA               |  TOTAL\_VALOR\_REAIS|
|-----:|:----|-----------:|:-----------|:--------------------|:---------------|:--------------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------|--------------------:|
|  2016| AC  |         643| ACRELANDIA | Assistência Social  | 20V5           | Ações Complementares de Proteção Social Básica                                                                                              |                                 |             19628.00|
|  2016| AC  |         643| ACRELANDIA | Educação            | 00PI           | Apoio à Alimentação Escolar na Educação Básica (PNAE)                                                                                       |                                 |            230832.00|
|  2016| AC  |         643| ACRELANDIA | Saúde               | 4525           | Apoio à Manutenção de Unidades de Saúde                                                                                                     | EMENDAS                         |            590518.00|
|  2016| AC  |         643| ACRELANDIA | Assistência Social  | 8893           | Apoio à Organização, à Gestão e à Vigilância Social no Território, no âmbito do Sistema Único de Assistência Social                         |                                 |             20927.10|
|  2016| AC  |         643| ACRELANDIA | Organização Agrária | 210X           | Apoio ao Desenvolvimento Sustentável de Territórios Rurais                                                                                  |                                 |            150000.00|
|  2016| AC  |         643| ACRELANDIA | Educação            | 0969           | Apoio ao Transporte Escolar na Educação Básica                                                                                              | PNATE                           |             92837.74|
|  2016| AC  |         643| ACRELANDIA | Assistência Social  | 20TR           | Apoio Financeiro Suplementar à Manutenção da Educação Infantil                                                                              |                                 |            141091.77|
|  2016| AC  |         643| ACRELANDIA | Saúde               | 8585           | Atenção à Saúde da População para Procedimentos em Média e Alta Complexidade                                                                | TETO MAC                        |             27818.95|
|  2016| AC  |         643| ACRELANDIA | Encargos Especiais  | 099E           | Auxilio Financeiro aos Entes Federados Exportadores                                                                                         | Compensação de Exportação - CEX |              9581.03|
|  2016| AC  |         643| ACRELANDIA | Assistência Social  | 2589           | Avaliação e Operacionalização do Benefício de Prestação Continuada da Assistência Social (BPC) e Manutenção da Renda Mensal Vitalícia (RMV) | BPC                             |              1000.00|

#' Transferência de recursos para os municípios
#'
#' Essa função extrai todas as transferências constitucionais para os municípios
#' presentes no Portal Transparência
#' @param ano A partir de 2004.
#' @param uf Sigla dos estados.
#' @import XML RCurl httr utils
#' @export
TransferenciaMunicipio <- function(ano, uf) {


  #carregando o arquivo com o código siafi de todos os municípios
  siafi <- read.csv("https://raw.githubusercontent.com/lucaseosilva/transferBR/master/data/siafi.csv", header = T, sep=";")

  #criado o frame vazio
  agregado <- data.frame()

  #iniciando as variáveis índice
  i <- 1
  j <- 1
  k <- 1
  z <- 1
  uf_ind <- ""

  #vetor de uf e ano
  if(is.vector(ano)==T){
    if(is.vector(uf)==T){
      for(z in 1:length(uf)){
        uf_ind <- uf[z]
        #selecionando apenas o estado selecionado pelo usuário
        uf_cidades <- siafi[siafi$uf==uf_ind, ]
        for(k in 1:length(ano)){
          #vai repetir de acordo com a quantidade de munícios no estado selecionado
          for(i in 1:nrow(uf_cidades)){
            j <- 1
            #loop infito referente às páginas
            while(j >= 1){
              #essa estrutura de decisão acrescentará um digito 0 à esquerda dos códigos que possuírem apenas 3 algarismos
              #o portal transparência diferencia 630 x 0630
              if(nchar(as.character(uf_cidades$codigo[i]))>=4){
                url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=%s&Pagina=%s",ano[k], uf_ind, uf_cidades$codigo[i], j);
              }
              else{
                url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=0%s&Pagina=%s",ano[k], uf_ind, uf_cidades$codigo[i], j);
              }

              #verificando se a página existe. Caso não exista, interrompe-se o while e já pula para o próximo município
              response <- GET(url)
              if (response$status_code!=200){
                break
              }

              #capturando aspenas as tabelas da página
              lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")

              #selecionando apenas a tabela que possui as informações desejadas
              tabela <- lista.tabelas[[2]]

              #adicionando algumas informações importantes
              tabela$ano <- ano[k]
              tabela$uf <- uf_ind
              tabela$codSiafi <- uf_cidades$codigo[i]
              tabela$nomeMunicipio <- uf_cidades$cidade[i]

              #agregando a tabela com as outras já coletadas
              agregado <- rbind(agregado, tabela)

              j <- j+1
            }
          }
        }
      }
    }
    #caso não seja vector de uf, mas vetor de ano
    else{
      #selecionando apenas o estado selecionado pelo usuário
      uf_cidades <- siafi[siafi$uf==uf, ]
      for(k in 1:length(ano)){
        #vai repetir de acordo com a quantidade de munícios no estado selecionado
        for(i in 1:nrow(uf_cidades)){
          j <- 1
          #loop infito referente às páginas
          while(j >= 1){
            #essa estrutura de decisão acrescentará um digito 0 à esquerda dos códigos que possuírem apenas 3 algarismos
            #o portal transparência diferencia 630 x 0630
            if(nchar(as.character(uf_cidades$codigo[i]))>=4){
              url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=%s&Pagina=%s",ano[k], uf, uf_cidades$codigo[i], j);
            }
            else{
              url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=0%s&Pagina=%s",ano[k], uf, uf_cidades$codigo[i], j);
            }

            #verificando se a página existe. Caso não exista, interrompe-se o while e já pula para o próximo município
            response <- GET(url)
            if (response$status_code!=200){
              break
            }

            #capturando aspenas as tabelas da página
            lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")

            #selecionando apenas a tabela que possui as informações desejadas
            tabela <- lista.tabelas[[2]]

            #adicionando algumas informações importantes
            tabela$ano <- ano[k]
            tabela$uf <- uf
            tabela$codSiafi <- uf_cidades$codigo[i]
            tabela$nomeMunicipio <- uf_cidades$cidade[i]

            #agregando a tabela com as outras já coletadas
            agregado <- rbind(agregado, tabela)

            j <- j+1
          }
        }
      }
    }
  }
  else{
    #não é vetor de ano, mas é de uf
    if(is.vector(uf)==T){
      for(z in length(uf)){
        uf_ind <- uf[z];
        #selecionando apenas o estado selecionado pelo usuário
        uf_cidades <- siafi[siafi$uf==uf_ind, ]
        #vai repetir de acordo com a quantidade de munícios no estado selecionado
        for(i in 1:nrow(uf_cidades)){
          j <- 1
          #loop infito referente às páginas
          while(j >= 1){

            #essa estrutura de decisão acrescentará um digito 0 à esquerda dos códigos que possuírem apenas 3 algarismos
            #o portal transparência diferencia 630 x 0630
            if(nchar(as.character(uf_cidades$codigo[i]))>=4){
              url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=%s&Pagina=%s",ano, uf_ind, uf_cidades$codigo[i], j);
            }
            else{
              url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=0%s&Pagina=%s",ano, uf_ind, uf_cidades$codigo[i], j);
            }

            #verificando se a página existe. Caso não exista, interrompe-se o while e já pula para o próximo município
            response <- GET(url)
            if (response$status_code!=200){
              break
            }

            #capturando aspenas as tabelas da página
            lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")

            #selecionando apenas a tabela que possui as informações desejadas
            tabela <- lista.tabelas[[2]]

            #adicionando algumas informações importantes
            tabela$ano <- ano[k]
            tabela$uf <- uf_ind
            tabela$codSiafi <- uf_cidades$codigo[i]
            tabela$nomeMunicipio <- uf_cidades$cidade[i]

            #agregando a tabela com as outras já coletadas
            agregado <- rbind(agregado, tabela)

            j <- j+1
          }
        }
      }
    }

    #se não for nem vetor de uf e nem de ano
    else{
      #vai repetir de acordo com a quantidade de munícios no estado selecionado
      for(i in 1:nrow(uf_cidades)){
        j <- 1
        #loop infito referente às páginas
        while(j >= 1){

          #essa estrutura de decisão acrescentará um digito 0 à esquerda dos códigos que possuírem apenas 3 algarismos
          #o portal transparência diferencia 630 x 0630
          if(nchar(as.character(uf_cidades$codigo[i]))>=4){
            url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=%s&Pagina=%s",ano, uf, uf_cidades$codigo[i], j);
          }
          else{
            url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaAcoes.asp?Exercicio=%s&SelecaoUF=1&SiglaUF=%s&CodMun=0%s&Pagina=%s",ano, uf, uf_cidades$codigo[i], j);
          }

          #verificando se a página existe. Caso não exista, interrompe-se o while e já pula para o próximo município
          response <- GET(url)
          if (response$status_code!=200){
            break
          }

          #capturando aspenas as tabelas da página
          lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")

          #selecionando apenas a tabela que possui as informações desejadas
          tabela <- lista.tabelas[[2]]

          #adicionando algumas informações importantes
          tabela$ano <- ano[k]
          tabela$uf <- uf
          tabela$codSiafi <- uf_cidades$codigo[i]
          tabela$nomeMunicipio <- uf_cidades$cidade[i]

          #agregando a tabela com as outras já coletadas
          agregado <- rbind(agregado, tabela)

          j <- j+1
        }
      }
    }
  }
  agregado <- agregado[,c(5:8,1:4)]

  agregado[,8] <- gsub("\\.","", agregado[,8])
  agregado[,8] <- as.numeric(gsub(",", ".", agregado[,8]))
  agregado[,8] <- as.numeric(agregado[,8])

  names(agregado) <- c("ANO", "UF", "COD_SIAFI", "MUNICIPIO", "FUNCAO","ACAO_GOVERNAMENTAL","LINGUAGEM_CIDADA","TOTAL_VALOR_REAIS")

  agregado <- separate(agregado, ACAO_GOVERNAMENTAL, c('COD', 'DESC_ACAO_GOV'), sep=' - ', remove=TRUE)

  names(agregado) <- c("ANO", "UF", "COD_SIAFI", "MUNICIPIO", "FUNÇÃO","CÓDIGO AÇÃO", "DESCRIÇÃO AÇÃO GOVERNAMENTAL","LINGUAGEM CIDADÃ","TOTAL ANO (R$)")
  return (agregado)
}

#' Transferência de recursos para os estados
#'
#' Essa função extrai todas as transferências constitucionais para todos os estados
#' presentes no Portal Transparência
#' @param ano A partir de 2004.
#' @import XML RCurl httr utils
#' @export
TransferenciaEstado <- function (ano2){
  #criado o frame vazio
  #agregado <- data.frame()

  #é vetor
  if(is.vector(ano2)){
    agregado <- data.frame()
    #iniciando as variáveis índice
    i <- 1
    j <- 1

    for(i in i:length(ano2)){
      j <- 1
      while(j>=1){
        url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaUFs.asp?Exercicio=%s&Pagina=%s",ano2[i], j);


        #verificando se a página existe. Caso não exista, interrompe-se o while e já pula para o próximo município
        response <- GET(url)
        if (response$status_code!=200){
          break
        }

        #capturando aspenas as tabelas da página
        lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")

        #selecionando apenas a tabela que possui as informações desejadas
        tabela <- lista.tabelas[[2]]

        #adicionando algumas informações importantes
        tabela$ano <- ano2[i]
        #tabela$uf <- uf_ind
        #tabela$codSiafi <- uf_cidades$codigo[i]
        #tabela$nomeMunicipio <- uf_cidades$cidade[i]

        #agregando a tabela com as outras já coletadas
        agregado <- rbind(agregado, tabela)

        j <- j+1
      }
    }
  }

  #não é vetor
  else{
    agregado <- data.frame()
    #iniciando as variáveis índice
    i <- 1
    j <- 1
    while(j>=1){
      url <- gettextf("http://transparencia.gov.br/PortalTransparenciaListaUFs.asp?Exercicio=%s&Pagina=%s",ano2, j);
      print(url)

      #verificando se a página existe. Caso não exista, interrompe-se o while e já pula para o próximo município
      response <- GET(url)
      if (response$status_code!=200){
        break
      }

      #capturando aspenas as tabelas da página
      lista.tabelas <- readHTMLTable(url, stringsAsFactors = F, encoding = "UTF-8")

      #selecionando apenas a tabela que possui as informações desejadas
      tabela <- lista.tabelas[[2]]

      #adicionando algumas informações importantes
      tabela$ano <- ano2
      #tabela$uf <- uf_ind
      #tabela$codSiafi <- uf_cidades$codigo[i]
      #tabela$nomeMunicipio <- uf_cidades$cidade[i]

      #agregando a tabela com as outras já coletadas
      agregado <- rbind(agregado, tabela)

      j <- j+1
    }
  }
  agregado <- agregado[,c(5,1:4)]
  agregado[,3] <- gsub("\\.","", agregado[,3])
  agregado[,3] <- as.numeric(gsub(",", ".", agregado[,3]))
  agregado[,3] <- as.numeric(agregado[,3])

  agregado[,4] <- gsub("\\.","", agregado[,4])
  agregado[,4] <- as.numeric(gsub(",", ".", agregado[,4]))
  agregado[,4] <- as.numeric(agregado[,4])

  agregado[,5] <- gsub("\\.","", agregado[,5])
  agregado[,5] <- as.numeric(gsub(",", ".", agregado[,5]))
  agregado[,5] <- as.numeric(agregado[,5])

  agregado[,3] <- as.numeric(agregado[,3])
  agregado[,4] <- as.numeric(agregado[,4])
  agregado[,5] <- as.numeric(agregado[,5])
  names(agregado) <- c("ANO", "UF", "TRANSFERENCIA_ESTADO (R$)", "TRANSFERÊNCIA MUNICÍPIO (R$)", "VALOR TOTAL (R$)")

  return (agregado)
}

#' Transferência de recursos para os municípios
#'
#' Essa função extrai os valores dos convênios transferidos aos estados
#' @import XML RCurl httr utils
#' @export
ColetaConvenioGeral <- function(){
  uf <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MT", "MS", "PE", "PI", "PB", "PR", "PA",
          "RS", "RR", "RO", "RN", "SP", "SE", "SC", "TO")

  i <- 1;
  j <- 1;

  agregado <- data.frame()
  url_base <- "http://www.portaltransparencia.gov.br/convenios/convenioslistamunicipios.asp?uf=ESTADO&codorgao=%20&tipoconsulta=0&periodo=&Pagina="
  i <- 1

  for(i in 1:length(uf)){
    url2 <- gsub("ESTADO", uf[i], url_base)
    #print(url2)
    j <- 1
    while(j >= 1){
      url3 <- paste(url2, j, collapse = "", sep = "")
      lista.tabelas <- readHTMLTable(url3, stringsAsFactors = F)
      tabela <- lista.tabelas[[2]]

      if(is.null(tabela)){
        break
      }
      else{
        tabela <- tabela[,-5]
        tabela$uf <- uf[i]
        agregado <- rbind(agregado, tabela)

      }
      j <- j+1
    }

  }

  agregado <- agregado[, c(5,1:4)]
  names(agregado) <- c("UF", "MUNICIPIO", "QUANT_CONVENIOS", "VALOR CONVENIADO (R$)", "VALOR LIBERADO (R$)")
  return (agregado)

}

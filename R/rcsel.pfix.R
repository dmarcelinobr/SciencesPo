### trabalhando com microdados


### funcao rcsel.pfix() conecta com um arquivo texto de posicao fixa
### extrai colunas especificadas e seleciona linhas utilizando
### funcao de selecao de linhas passada pelo usuario
rcsel.pfix <- function(file.inp, file.out, first, last, frsel,
                       buffer=1000, nmax=1e10, verbose=TRUE, ...) {
  ## file.inp e' o arquivo de origem dos dados
  ## file.inp e' o arquivo de destino dos dados selecionados
  ## first e' a posicao de inicio da variavel
  ## last e'a posicao de fim da variavel
  ## frsel e' uma funcao de selecao das linhas do arquivo de dados
  ## buffer e' o numero de linhas lidas a cada vez
  ## nmax e' o numero maximo de linhas a ler do arquivo de origem
  ## verbose e' uma flag para opcao de imprimir mensagens de progresso
  ## ... parametros adicionais passados para a funcao frsel()
  ns <- nr <- 0
  con.inp <- file(file.inp, "r")
  repeat {
    if (nr>=nmax)
      break
    else {
      temp <- readLines(con.inp, buffer)
      nr <- nr + length(temp)
      if (length(temp)>0) {
        temp2 <- frsel(temp, ...)
        nsi <- length(temp2)
        if (verbose)
          cat("Lidos:", length(temp), "registros. Selecionados",
              nsi, "linhas.\n")
        if (nsi>0) {
          ns <- ns + nsi
          temp2 <- t(sapply(temp2, substring,
                            first, last))
          write.table(temp2, file=file.out, append=TRUE,
                      row.names=FALSE, col.names=FALSE)
        }
      }
      else break
    }
  }
  if (verbose)
    cat("TOTAL: Lidos:", nr, "registros. Selecionados",
        ns, "linhas.\n")
  close(con.inp)
}


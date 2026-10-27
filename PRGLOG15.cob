programa
{
    inclua biblioteca Texto    --> txt
    inclua biblioteca Arquivos --> arq

    funcao inicio()
    {
        cadeia caminho = "C:/CURSOLOGICA/dados/funcionarios.txt"
        cadeia linha
        inteiro qt = 0, acum_lidos=0
        inteiro func

        se (arq.arquivo_existe(caminho))
        {
            escreva("Arquivo encontrado.\n")
            func = arq.abrir_arquivo(caminho, arq.MODO_LEITURA)
            //inteiro total = 0
		
		  inteiro total = 0
            enquanto (nao arq.fim_arquivo(func))
		{
		    linha = arq.ler_linha(func)
	
		    se (linha != "")
		    {
		        qt = txt.numero_caracteres(linha)
		        escreva(linha, " - ", qt, "\n")
		        total = total + 1
		    }
		}

		  escreva("---------------------Estatistica-------------------\n")
		  escreva("Total de registros lidos...", total, "\n")
            arq.fechar_arquivo(func)
        }
        senao
        {
            escreva("Arquivo não encontrado neste caminho: ", caminho, "\n")
        }
    }
}
programa
{
    inclua biblioteca Arquivos --> arq

    funcao logico criarArquivo(cadeia caminhoCompleto)
    {
        inteiro arquivo

        arquivo = arq.abrir_arquivo(
            caminhoCompleto,
            arq.MODO_ESCRITA
        )

        se (arquivo != -1)
        {
            arq.fechar_arquivo(arquivo)
            retorne verdadeiro
        }

        retorne falso
    }

    funcao inicio()
    {
        cadeia resposta
        cadeia nomeArquivo
        cadeia caminhoCompleto
        cadeia opcaoCriacao
        cadeia continuar = "S"
        cadeia arquivos[100]
        cadeia tipos[1]
        inteiro quantidade
        inteiro i
        inteiro opcaoArquivo

        tipos[0] = ".txt"

        faca
        {
            arq.listar_arquivos_por_tipo(
                "C:/CURSOLOGICA/dados/",
                arquivos,
                tipos
            )

            escreva("\nArquivos TXT disponíveis:\n")
            quantidade = 0

            para (i = 0; i < 100; i++)
            {
                se (arquivos[i] != "")
                {
                    quantidade = quantidade + 1
                    escreva(quantidade, " - ", arquivos[i], "\n")
                }
            }

            escreva("0 - Digitar outro nome de arquivo\n")
            escreva("Escolha o número do arquivo: ")
            leia(opcaoArquivo)

            se (opcaoArquivo == 0)
            {
                escreva("Digite o nome do arquivo com a extensão .txt: ")
                leia(nomeArquivo)
            }
            senao se (opcaoArquivo >= 1 e opcaoArquivo <= quantidade)
            {
                nomeArquivo = arquivos[opcaoArquivo - 1]
            }
            senao
            {
                nomeArquivo = ""
                escreva("Opção inválida.\n")
            }

            se (nomeArquivo != "")
            {
                caminhoCompleto = "C:/CURSOLOGICA/dados/" + nomeArquivo

                se (arq.arquivo_existe(caminhoCompleto))
                {
                    escreva("Arquivo ", nomeArquivo, " já existe!\n")
                }
                senao
                {
                    escreva("Arquivo não encontrado. Deseja criá-lo? (S/N): ")
                    leia(resposta)

                    se (resposta == "S" ou resposta == "s")
                    {
                        se (criarArquivo(caminhoCompleto))
                        {
                            escreva("Arquivo ", nomeArquivo, " criado com sucesso!\n")
                        }
                        senao
                        {
                            escreva("Não foi possível criar o arquivo.\n")
                        }
                    }
                }
            }

            escreva("\nDeseja verificar outro arquivo? (S/N): ")
            leia(continuar)

        } enquanto (continuar == "S" ou continuar == "s")
    }
}

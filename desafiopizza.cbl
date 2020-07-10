      *divisão de identificação do programa
       identification division.
       program-id. "desafiopizza".
       author. "Maodna Schvambach".
       installation. "PC".
       date-written. 08/07/2020.
       date-compiled. 10/07/2020.



      *--- divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *--- declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *declaração de variáveis
       data division.

      *--- variaveis de arquivos
       file section.


      *--- variaveis de trabalho
       working-storage section.
      *valor universal do vetor relatorio_pizzas
       77 quant_pizzas                             pic 9(02) value 20.
      *vetor das pizzas
       01  relatorio_pizzas  occurs  20.
           05 nome                                 pic x(15).
           05 filler                               pic x(03)
                                                   value " - ".
           05 diametro                             pic 9(03)
                                                   value 0.
           05 filler                               pic x(03)
                                                   value " - ".
           05 preco_pizza                          pic 9(03)v9(02)
                                                   value 0.
           05 filler                               pic x(03)
                                                   value " - ".
           05 preco_cm2                            pic 9(02)v9(02)
                                                   value 0.
           05 filler                               pic x(03)
                                                   value " - ".
           05 diferenca_percentual                 pic 9(03)v9(02)
                                                   value 0.
       77  ind                                     pic 9(02).
       77  opcao_continuar                         pic x(01).
       77  raio_pizza                              pic 9(02)v9(02).
       77  area_pizza                              pic 9(03)v9(02).
       77  valor_pi                                pic 9(01)v9(07)
                                                   value 3,1415926.
       77 controle                                 pic x(12).
       77 aux_nome                                 pic x(15).
       77 aux_diametro                             pic 9(03).
       77 aux_preco_pizza                          pic 9(03)v9(02).
       77 aux_preco_cm2                            pic 9(02)v9(02).
       77 diferenca                                pic 9(03)v9(02).

      *--- variaveis para comunicação entre programas
       linkage section.


      *--- declaração de tela
       screen section.


      *--- declaração do corpo do programa
       procedure division.
      *chama as funcoes de execução do código
           perform inicializa.
           perform processamento.
           perform finaliza.

      *-------------------------inicializacao---------------------------
      *    Inicilizacao de variaveis, abertura de arquivos
      *    procedimentos que serao realizados apenas uma vez
       inicializa section.
           move   "S"       to     opcao_continuar
           .
       inicializa-exit.
           exit.
      *------------------------fim inicializacao------------------------

      *-------------------------processamento---------------------------
       processamento section.
      *    entrada de dados do usuario
           move 0 to ind
           perform until opcao_continuar = "N"
               or opcao_continuar = "n"
               display erase
               add 1 to ind
      *        informacoes da pizza (usuario)
               if ind > quant_pizzas then
                   display "Voce atingiu o limite de " quant_pizzas
      -            " pizzas"
               else
                   display "Informe o nome da pizza "
                   accept nome(ind)

                   display "Informe o diametro "
                   accept diametro(ind)

                   display "Informe o preco "
                   accept preco_pizza(ind)
               end-if

               perform calcular_preco_cm2
               display "Desejas cadastrar mais uma pizza? ('S'/'N')"
               accept opcao_continuar
           end-perform

      *    chama função que coloca os preços em cm2 em ordem crescente
           perform funcao_ordem_crescente
      *    saida das informacoes na ordem correta com percentual(%)
           perform saida_informacoes
           .
       processamento-exit.
           exit.
      *-----------------------fim processamento-------------------------

      *--------------------------finalizacao----------------------------
       finaliza section.
           display " "
           display "--- fim do programa ---"
           Stop run
           .
       finaliza-exit.
           exit.
      *------------------------fim finalizacao--------------------------

      *-----------------------------funções-----------------------------

      *--- função para calcular o preço por cm2 das pizzzas
       calcular_preco_cm2 section.
      *    calculo da area é: pi * raio * raio
      *    calculo preço por cm2 é: preco/area

           compute raio_pizza = diametro(ind) / 2
           compute area_pizza = valor_pi * raio_pizza * raio_pizza
           compute preco_cm2(ind) = preco_pizza(ind) / area_pizza
           .
       calcular_preco_cm2-exit.
           exit.

      *--- funcao para ordenar o cm2 em ordem crescente
       funcao_ordem_crescente section.
           move 0 to ind
           move "continua" to controle
           perform until controle <> "continua"
               move 1 to ind
               move "Ncontinua" to controle
      *        preco_cm2(ind + 1) = 0 -> para n comparar com o vetor
      *        que n foi utilizado pelo usuário
               perform until ind = quant_pizzas
                   or preco_cm2(ind + 1) = 0
                   if preco_cm2(ind) > preco_cm2(ind + 1) then
      *                nome
                       move nome(ind) to aux_nome
                       move nome(ind + 1) to nome(ind)
                       move aux_nome to nome(ind + 1)
      *                diametro
                       move diametro(ind) to aux_diametro
                       move diametro(ind + 1) to diametro(ind)
                       move aux_diametro to diametro(ind + 1)
      *                preco pizza
                       move preco_pizza(ind) to aux_preco_pizza
                       move preco_pizza(ind + 1) to preco_pizza(ind)
                       move aux_preco_pizza to preco_pizza(ind + 1)
      *                preco por centimetro quadrado
                       move preco_cm2(ind) to aux_preco_cm2
                       move preco_cm2(ind + 1) to preco_cm2(ind)
                       move aux_preco_cm2 to preco_cm2(ind + 1)

                       move "continua" to controle
                   end-if
                   add 1 to ind
               end-perform
           end-perform
           move 1 to ind
           .
       funcao_ordem_crescente-exit.
           exit.

      *--- saida das informacoes na ordem crescente e com %
       saida_informacoes section.
           display " "

      *    saida das informacoes
           perform varying ind from 1 by 1 until ind > quant_pizzas
               or nome(ind) = space
               if ind <> 1 then
      *            calcular percentual
                   compute diferenca =  preco_cm2(ind) -
                   preco_cm2(ind - 1)
                   compute diferenca_percentual(ind) =
                   (diferenca * 100)/preco_cm2(ind)
               else
                   move 0 to diferenca_percentual(ind)
               end-if

           display relatorio_pizzas(ind)

           end-perform
           .
       saida_informacoes-exit.
           exit.

      *---------------------------fim funções---------------------------









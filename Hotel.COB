      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOTEL-PARAISO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 NOME-HOSPEDE        PIC A(30).
       77 DIAS-HOSPEDADO      PIC 99.
       77 TIPO-QUARTO         PIC X.
       77 VALOR-DIARIA        PIC 9(5)V99.
       77 VALOR-TOTAL         PIC 9(6)V99.
       77 VALOR-COM-DESCONTO  PIC 9(6)V99.
       77 DESCONTO            PIC 9(5)V99.
       77 TOTAL-GERAL         PIC 9(7)V99 VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "******** HOTEL PARAISO TROPICAL ********".
       PERFORM UNTIL NOME-HOSPEDE = "FIM"
       DISPLAY "Informe o nome do hospede (ou FIM para encerrar):"
       ACCEPT NOME-HOSPEDE
       IF NOME-HOSPEDE NOT = "FIM"
           DISPLAY "Informe o número de dias hospedado:"
           ACCEPT DIAS-HOSPEDADO
           DISPLAY "Informe o tipo de quarto (S - Simples, D - Duplo, "
                           "L - Luxo):"
           ACCEPT TIPO-QUARTO
           EVALUATE TIPO-QUARTO
                    WHEN "S"
                    MOVE 10000 TO VALOR-DIARIA
                    WHEN "D"
                    MOVE 15000 TO VALOR-DIARIA
                    WHEN "L"
                    MOVE 25000 TO VALOR-DIARIA
                    WHEN OTHER
                    DISPLAY "Tipo de quarto inválido. Usando valor 0."
                   MOVE 0 TO VALOR-DIARIA
               END-EVALUATE
               COMPUTE VALOR-TOTAL = VALOR-DIARIA * DIAS-HOSPEDADO

                   IF DIAS-HOSPEDADO > 7
                    COMPUTE DESCONTO = VALOR-TOTAL * 0.10
                    COMPUTE VALOR-COM-DESCONTO = VALOR-TOTAL - DESCONTO
                   ELSE
                       MOVE VALOR-TOTAL TO VALOR-COM-DESCONTO
                   END-IF

                   DISPLAY "Hospede: " NOME-HOSPEDE
                   DISPLAY "Valor a pagar: R$ " VALOR-COM-DESCONTO

                COMPUTE TOTAL-GERAL = TOTAL-GERAL + VALOR-COM-DESCONTO
       END-IF
       END-PERFORM
           DISPLAY "------------------------------------------"
           DISPLAY "TOTAL GERAL ARRECADADO PELO HOTEL: R$ " TOTAL-GERAL
           DISPLAY "******** FIM DO PROCESSAMENTO ********"
            STOP RUN.
       END PROGRAM HOTEL-PARAISO.

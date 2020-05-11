       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLOCKCHAIN.

      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.




       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *DECLARATION VARIABLE START BY 01/02 ETC...
      * CHILD IS UNDER IT VARIABLE WITH '05' AT START OF LINE

       01 INPUT-LINE PIC X(100).


       PROCEDURE DIVISION.

       MAIN.

           PERFORM UNTIL 1 = 0
           DISPLAY "ENTER LINES:  "
           ACCEPT INPUT-LINE
           DISPLAY "YOU ENTER :  " INPUT-LINE
           IF INPUT-LINE = "EXIT"
                   STOP RUN
           END-IF
           END-PERFORM.
           STOP RUN.


               

       


       

                




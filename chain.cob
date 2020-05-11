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
       01 NUM PIC 9(5).

       01 TODAY PIC 99/99/9999.
       01 TODAY-R REDEFINES TODAY.
               05 TODAY-MONTH PIC 99.
               05 FILLER PIC X.
               05 TODAY-DAY PIC 99.
               05 FILLER PIC X.
               05 TODAY-YEAR PIC 9999.



       PROCEDURE DIVISION.

       MAIN.

           MOVE 05122020 TO TODAY
           DISPLAY TODAY-day DISPLAY TODAY-MONTH DISPLAY TODAY-YEAR.

           PERFORM UNTIL 1 = 0
                DISPLAY "ENTER LINES:  " ACCEPT INPUT-LINE
                DISPLAY "YOU ENTER :  " INPUT-LINE
                MOVE FUNCTION UPPER-CASE(INPUT-LINE) TO INPUT-LINE
               
                EVALUATE INPUT-LINE
                        WHEN "EXIT" GO TO THE-END
                        
                END-EVALUATE

               
               
                IF INPUT-LINE = "EXIT"
                   STOP RUN
                END-IF
             END-PERFORM.
           
       THE-END.
           DISPLAY "FINISH"
           
           
           STOP RUN.


               

       


       

                




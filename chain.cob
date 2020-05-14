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
       01 RESPONSEZ.
               05 RESPONSE-IN-WS PIC X      VALUE "C".

       01 DATA-FROM-SCREEN.
               05 ID-IN-WS  PIC XXXX        VALUE SPACES.
               05 NAME-IN-WS PIC X(2)        VALUE SPACES.

       01 BLOCKS.
               05 BLOCKS-NUMBER PIC X(100).
               05 BLOCKS-VERSION PIC 9(10).
               
               
       01 BLOCKSHASH.
               05 BLOCKHASH-NUM PIC X(100).

       SCREEN SECTION.
       
       01 DATA-ENTRY-SCREEN.
          05 VALUE "DATA-ENTRY SCREEN" BLANK SCREEN LINE 1 COL 35.
          05  VALUE "DATA ENTRY SCREEN" BLANK SCREEN     LINE 1 COL 35.
          05  VALUE "ID #"                               LINE 3 COL 10.
          05   ID-INPUT                                  LINE 3 COL 25
                    PIC  X(4)     TO ID-IN-WS.
          05  VALUE "NAME"                               LINE 5 COL 10.
          05  NAME-INPUT                                 LINE 5 COL 25
                    PIC X(20)     TO NAME-IN-WS.
          05  VALUE "C - TO CONTINUE"                    LINE 11 COL 30.
          05  VALUE "Q - TO QUIT"                        LINE 12 COL 30.
          05  VALUE "ENTER RESPONSE"                     LINE 14 COL 30.
          05  RESPONSE-INPUT                             LINE 14 COL 45
                   PIC X         TO RESPONSE-IN-WS.





                       

        PROCEDURE DIVISION.


        MAIN.


           PERFORM UNTIL 1 = 0
                DISPLAY "ENTER LINES:  " ACCEPT INPUT-LINE
                DISPLAY "YOU ENTER :  " INPUT-LINE
                MOVE FUNCTION UPPER-CASE(INPUT-LINE) TO INPUT-LINE
               
                EVALUATE INPUT-LINE
                        WHEN "EXIT" GO TO THE-END
                        
                END-EVALUATE

               
               
           END-PERFORM.
           
       THE-END.
           DISPLAY "FINISH"
           
           
           STOP RUN.


               

       


       

                




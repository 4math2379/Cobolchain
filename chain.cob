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


               

       


       

                




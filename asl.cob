        IDENTIFICATION DIVISION. 
        PROGRAM-ID. ASL.
        DATA DIVISION.
        WORKING-STORAGE SECTION. 
        01 OUT-MSG.
         02 FILLER PIC X(6) VALUE "Hello ". 
         02 MSG    PIC X(20).
        01 INP-MSG PIC X(20) VALUE "What is your name? ".     
        PROCEDURE DIVISION.
           DISPLAY "Hello World!".

       
        END PROGRAM ASL.

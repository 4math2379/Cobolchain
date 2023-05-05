        IDENTIFICATION DIVISION. 
        PROGRAM-ID. ENRCYPT.
        DATA DIVISION.
        WORKING-STORAGE SECTION. 
        01 OUT-MSG.
         02 FILLER PIC X(6) VALUE "Test for COBOL Encryption ". 
         02 MSG    PIC X(20).
        01 INP-MSG PIC X(20) VALUE "poc XXX ".     
        
        01 WS-ENCRYPT-KEY.
         02 FILLER PIC X(8) VALUE "12345678".
         02 MSG    PIC X(8).
        01 INP-MSG-LEN PIC S9(4) COMP VALUE 20.
        
        
        
        PROCEDURE DIVISION.
        
        
        
        END PROGRAM ENRCYPT.

 IDENTIFICATION DIVISION. 
        PROGRAM-ID. MECCG.
        DATA DIVISION.
WORKING-STORAGE SECTION.
       01  RESPONSES.
           05  RESPONSE-IN-WS    PIC X        VALUE SPACES.
       01  DATA-FROM-SCREEN.
           05  ID-IN-WS          PIC XXXX     VALUE SPACES.
           05  NAME-IN-WS        PIC X(20)    VALUE SPACES.
       SCREEN SECTION.
       01  DATA-ENTRY-SCREEN.
           05  ID-SECTION.
               10  VALUE "DATA ENTRY SCREEN"      BLANK SCREEN
                                              LINE 01 COL 30.
               10  VALUE "ID #: "             LINE 05 COL 05.
               10  ID-ON-SCR-IN               LINE 05 COL 15
                       PIC XXXX         TO ID-IN-WS.
           05  NAME-SECTION.
               10  VALUE "NAME:"              LINE 07 COL 05.
               10  NAME-ON-SCR-IN             LINE 07 COL 15
                       PIC X(20)        TO NAME-IN-WS.
           05  RESPONSE-SECTION.
               10  VALUE "C - TO CONTINUE"    LINE 16 COL 30.
               10  VALUE "Q - TO QUIT"        LINE 17 COL 30.
               10  VALUE "ENTER CHOICE:"      LINE 19 COL 30.
               10  RESPONSE-SCR               LINE 19 COL 45
                       PIC X     TO RESPONSE-IN-WS.
       PROCEDURE DIVISION.
       ...
       B-100-PROCESS.
           ...
           DISPLAY ID-SECTION.
           ACCEPT ID-ON-SCR-IN
           DISPLAY NAME-SECTION.
           ACCEPT NAME-ON-SCR-IN.
           DISPLAY RESPONSE-SECTION.
           ACCEPT RESPONSE-SCR.
           PERFORM B-200-LOOP
               UNTIL RESPONSE-IN-WS = "Q".
       B-200-LOOP.
           ... process...
           DISPLAY ID-SECTION.
           ACCEPT ID-ON-SCR-IN.
           DISPLAY NAME-SECTION.
           ACCEPT NAME-ON-SCR-IN.
           DISPLAY RESPONSE-SECTION.
           ACCEPT RESPONSE-SCR.
       C-100-TERMINATE.
           CLOSE OUTPUT-FILE.
           
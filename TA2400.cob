000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TA2400.
000300 AUTHOR. CODRON ROBERT.
000400 DATE-WRITTEN. 23 NOVEMBRE 1978.
000500 DATE-COMPILED.
000600*         CE PROGRAMME DONNE LE OU LES CALENDRIERS
000700*                      D UNE OU PLUSIEURS ANNEES.
000800 ENVIRONMENT DIVISION.
000900 INPUT-OUTPUT SECTION.
001000 FILE-CONTROL.
001100     SELECT FIMP ASSIGN TO "CALEND.TXT"
001200         ORGANIZATION IS LINE SEQUENTIAL.
001300 DATA DIVISION.
001400 FILE SECTION.
001500 FD  FIMP
001600               DATA RECORD LIGNE.
001700******************************************************************
001800*        LIGNE UNIQUE D'IMPRESSION POUR UN CALENDRIER            *
001900******************************************************************
002000 01  LIGNE.
002100     02 LIGNE1 PIC X(132).
002200     02 LIGNE2 REDEFINES LIGNE1.
002300         03 FILLER PIC X(36).
002400         03 TLIGNE2.
002500              04 VLIGNE2 OCCURS 4.
002600                   05 LIGNE2-1 PIC X(12).
002700                   05 LIGNE2-2 PIC X(3).
002800         03 FILLER PIC X(36).
002900     02 LIGNE3 REDEFINES LIGNE1.
003000         03 FILLER PIC X(3).
003100         03 ASTERX PIC X(127).
003200         03 LIGNE4 REDEFINES ASTERX.
003300                   04 AST1   PIC X.
003400                   04 FILLER PIC X.
003500                   04 LIBJOUR PIC X(3).
003600                   04 FILLER PIC X.
003700                   04 AST2   PIC X.
003800                   04 TLIGNE4.
003900                        05 VLIGNE4 OCCURS 6.
004000                                  06   FILLER   PIC X.
004100                                  06   LIBMOIS  PIC X(17).
004200                                  06   FILLER PIC X.
004300                                  06   AST      PIC X.
004400                   04 TABEDT REDEFINES TLIGNE4.
004500                        05 CASE   OCCURS 6.
004600                                  06 ELT  OCCURS 6.
004700                                       07 FILLER PIC X.
004800                                       07 NJOUR PIC ZZ.
004900                                  06 FILLER PIC XX.
005000         03 FILLER PIC XX.
005100 WORKING-STORAGE SECTION.
005200 77  I             PIC  S9(4) USAGE COMP.
005300 77  J             PIC  S9(4) USAGE COMP.
005400 77  K             PIC  S9(4) USAGE COMP.
005500 77  L             PIC  S9(4) USAGE COMP.
005600 77  DEP           PIC  S9(4) USAGE COMP.
005700 77  M             PIC  S9(4) USAGE COMP.
005800 77  Q             PIC  S9(4) USAGE COMP.
005900 77  R             PIC  S9(4) USAGE COMP.
006000 77  R1            PIC  S9(4) USAGE COMP.
006100 77  X             PIC  S9(4) USAGE COMP.
006200 77  W             PIC  S9(4) USAGE COMP.
006300 77  Z             PIC  S9(4) USAGE COMP.
006400 77  CPT           PIC  S9(4) USAGE COMP.
006500 77  NBPAGES       PIC  S9(4) USAGE COMP VALUE 0.
006600 77  ANPRE         PIC  X(4) VALUE LOW-VALUES.
006700 77  IMP150A        PIC  X(4) VALUE "150A".
006800 77  TEXTEHAUT     PIC  X(132).
006900******************************************************************
007000*  T A B L E  D E S  M O I S  E T  J O U R S  D E S   M O I S    *
007100******************************************************************
007200 01  TABLE-MOIS.
007300     02 MOIS.
007400         03 MOIS1  PIC X(19) VALUE "  J A N V I E R  31".
007500         03 MOIS2  PIC X(19) VALUE "  F E V R I E R  28".
007600         03 MOIS3  PIC X(19) VALUE "    M A R S      31".
007700         03 MOIS4  PIC X(19) VALUE "    A V R I L    30".
007800         03 MOIS5  PIC X(19) VALUE "      M A I      31".
007900         03 MOIS6  PIC X(19) VALUE "     J U I N     30".
008000         03 MOIS7  PIC X(19) VALUE "  J U I L L E T  31".
008100         03 MOIS8  PIC X(19) VALUE "     A O U T     31".
008200         03 MOIS9  PIC X(19) VALUE "S E P T E M B R E30".
008300         03 MOIS10 PIC X(19) VALUE "  O C T O B R E  31".
008400         03 MOIS11 PIC X(19) VALUE " N O V E M B R E 30".
008500         03 MOIS12 PIC X(19) VALUE " D E C E M B R E 31".
008600     02 REMOIS REDEFINES MOIS.
008700        03 MOIS-JOUR OCCURS 12.
008800           04 LIBELMOIS           PIC X(17).
008900           04 NBJOURMOIS          PIC 99.
009000******************************************************************
009100*              T A B L E   D E S  J O U R S                      *
009200******************************************************************
009300 01  TABLE-JOUR.
009400     02 JOUR.
009500         03 FILLER PIC X(21) VALUE "LUNMARMERJEUVENSAMDIM".
009600     02 REJOUR REDEFINES JOUR.
009700         03 LIBELJOUR OCCURS 7 PIC X(3).
009800******************************************************************
009900*     T A B L E   D E S   'G R A N D S   C H I F F R E S'        *
010000******************************************************************
010100 01  TABLE-CHIFFRE.
010200     02 CHIFFRE.
010300         03   UN.
010400              04 FILLER PIC X(12) VALUE "     11     ".
010500              04 FILLER PIC X(12) VALUE "    111     ".
010600              04 FILLER PIC X(12) VALUE "   1111     ".
010700              04 FILLER PIC X(12) VALUE "   1111     ".
010800              04 FILLER PIC X(12) VALUE "     11     ".
010900              04 FILLER PIC X(12) VALUE "     11     ".
011000              04 FILLER PIC X(12) VALUE "     11     ".
011100              04 FILLER PIC X(12) VALUE "     11     ".
011200              04 FILLER PIC X(12) VALUE "     11     ".
011300              04 FILLER PIC X(12) VALUE "     11     ".
011400              04 FILLER PIC X(12) VALUE "     11     ".
011500              04 FILLER PIC X(12) VALUE "   111111   ".
011600         03   DEUX.
011700              04 FILLER PIC X(12) VALUE " 2222222222 ".
011800              04 FILLER PIC X(12) VALUE "222222222222".
011900              04 FILLER PIC X(12) VALUE "22        22".
012000              04 FILLER PIC X(12) VALUE "          22".
012100              04 FILLER PIC X(12) VALUE "          22".
012200              04 FILLER PIC X(12) VALUE " 22222222222".
012300              04 FILLER PIC X(12) VALUE "22222222222 ".
012400              04 FILLER PIC X(12) VALUE "22          ".
012500              04 FILLER PIC X(12) VALUE "22          ".
012600              04 FILLER PIC X(12) VALUE "22          ".
012700              04 FILLER PIC X(12) VALUE "222222222222".
012800              04 FILLER PIC X(12) VALUE "222222222222".
012900          03  TROIS.
013000              04 FILLER PIC X(12) VALUE " 3333333333 ".
013100              04 FILLER PIC X(12) VALUE "333333333333".
013200              04 FILLER PIC X(12) VALUE "33        33".
013300              04 FILLER PIC X(12) VALUE "          33".
013400              04 FILLER PIC X(12) VALUE "          33".
013500              04 FILLER PIC X(12) VALUE "  333333333 ".
013600              04 FILLER PIC X(12) VALUE "  333333333 ".
013700              04 FILLER PIC X(12) VALUE "          33".
013800              04 FILLER PIC X(12) VALUE "          33".
013900              04 FILLER PIC X(12) VALUE "33        33".
014000              04 FILLER PIC X(12) VALUE "333333333333".
014100              04 FILLER PIC X(12) VALUE " 3333333333 ".
014200          03 QUATRE.
014300              04 FILLER PIC X(12) VALUE "       44   ".
014400              04 FILLER PIC X(12) VALUE "      444   ".
014500              04 FILLER PIC X(12) VALUE "     4444   ".
014600              04 FILLER PIC X(12) VALUE "    44 44   ".
014700              04 FILLER PIC X(12) VALUE "   44  44   ".
014800              04 FILLER PIC X(12) VALUE "  44   44   ".
014900              04 FILLER PIC X(12) VALUE " 44    44   ".
015000              04 FILLER PIC X(12) VALUE "444444444444".
015100              04 FILLER PIC X(12) VALUE "444444444444".
015200              04 FILLER PIC X(12) VALUE "       44   ".
015300              04 FILLER PIC X(12) VALUE "       44   ".
015400              04 FILLER PIC X(12) VALUE "       44   ".
015500         03   CINQ.
015600              04 FILLER PIC X(12) VALUE "555555555555".
015700              04 FILLER PIC X(12) VALUE "555555555555".
015800              04 FILLER PIC X(12) VALUE "55          ".
015900              04 FILLER PIC X(12) VALUE "55          ".
016000              04 FILLER PIC X(12) VALUE "55          ".
016100              04 FILLER PIC X(12) VALUE "55555555555 ".
016200              04 FILLER PIC X(12) VALUE "555555555555".
016300              04 FILLER PIC X(12) VALUE "          55".
016400              04 FILLER PIC X(12) VALUE "          55".
016500              04 FILLER PIC X(12) VALUE "55        55".
016600              04 FILLER PIC X(12) VALUE "555555555555".
016700              04 FILLER PIC X(12) VALUE " 5555555555 ".
016800           03 SIX.
016900              04 FILLER PIC X(12) VALUE " 6666666666 ".
017000              04 FILLER PIC X(12) VALUE "666666666666".
017100              04 FILLER PIC X(12) VALUE "66        66".
017200              04 FILLER PIC X(12) VALUE "66          ".
017300              04 FILLER PIC X(12) VALUE "66          ".
017400              04 FILLER PIC X(12) VALUE "66666666666 ".
017500              04 FILLER PIC X(12) VALUE "666666666666".
017600              04 FILLER PIC X(12) VALUE "66        66".
017700              04 FILLER PIC X(12) VALUE "66        66".
017800              04 FILLER PIC X(12) VALUE "66        66".
017900              04 FILLER PIC X(12) VALUE "666666666666".
018000              04 FILLER PIC X(12) VALUE " 6666666666 ".
018100         03 SEPT.
018200              04 FILLER PIC X(12) VALUE "777777777777".
018300              04 FILLER PIC X(12) VALUE "777777777777".
018400              04 FILLER PIC X(12) VALUE "77       77 ".
018500              04 FILLER PIC X(12) VALUE "        77  ".
018600              04 FILLER PIC X(12) VALUE "       77   ".
018700              04 FILLER PIC X(12) VALUE "      77    ".
018800              04 FILLER PIC X(12) VALUE "     77     ".
018900              04 FILLER PIC X(12) VALUE "    77      ".
019000              04 FILLER PIC X(12) VALUE "   77       ".
019100              04 FILLER PIC X(12) VALUE "  77        ".
019200              04 FILLER PIC X(12) VALUE " 77         ".
019300              04 FILLER PIC X(12) VALUE " 77         ".
019400         03 HUIT.
019500              04 FILLER PIC X(12) VALUE " 8888888888 ".
019600              04 FILLER PIC X(12) VALUE "888888888888".
019700              04 FILLER PIC X(12) VALUE "88        88".
019800              04 FILLER PIC X(12) VALUE "88        88".
019900              04 FILLER PIC X(12) VALUE "88        88".
020000              04 FILLER PIC X(12) VALUE " 8888888888 ".
020100              04 FILLER PIC X(12) VALUE " 8888888888 ".
020200              04 FILLER PIC X(12) VALUE "88        88".
020300              04 FILLER PIC X(12) VALUE "88        88".
020400              04 FILLER PIC X(12) VALUE "88        88".
020500              04 FILLER PIC X(12) VALUE "888888888888".
020600              04 FILLER PIC X(12) VALUE " 8888888888 ".
020700         03 NEUF.
020800              04 FILLER PIC X(12) VALUE " 9999999999 ".
020900              04 FILLER PIC X(12) VALUE "999999999999".
021000              04 FILLER PIC X(12) VALUE "99        99".
021100              04 FILLER PIC X(12) VALUE "99        99".
021200              04 FILLER PIC X(12) VALUE "99        99".
021300              04 FILLER PIC X(12) VALUE "999999999999".
021400              04 FILLER PIC X(12) VALUE " 99999999999".
021500              04 FILLER PIC X(12) VALUE "          99".
021600              04 FILLER PIC X(12) VALUE "          99".
021700              04 FILLER PIC X(12) VALUE "99        99".
021800              04 FILLER PIC X(12) VALUE "999999999999".
021900              04 FILLER PIC X(12) VALUE " 9999999999 ".
022000         03 ZER.
022100              04 FILLER PIC X(12) VALUE " 0000000000 ".
022200              04 FILLER PIC X(12) VALUE "000000000000".
022300              04 FILLER PIC X(12) VALUE "00        00".
022400              04 FILLER PIC X(12) VALUE "00        00".
022500              04 FILLER PIC X(12) VALUE "00        00".
022600              04 FILLER PIC X(12) VALUE "00        00".
022700              04 FILLER PIC X(12) VALUE "00        00".
022800              04 FILLER PIC X(12) VALUE "00        00".
022900              04 FILLER PIC X(12) VALUE "00        00".
023000              04 FILLER PIC X(12) VALUE "00        00".
023100              04 FILLER PIC X(12) VALUE "000000000000".
023200              04 FILLER PIC X(12) VALUE " 0000000000 ".
023300     02 RECHIFFRE REDEFINES CHIFFRE.
023400         03 POSI-1 OCCURS 10.
023500                   04 LIG-CHIF OCCURS 12 PIC X(12).
023600 01  CARTE.
023700     02  ANNEE.
023800         03 CHIF-AN OCCURS  4     PIC 9.
023900     02  AN REDEFINES ANNEE       PIC 9(4).
024000     02  NBFOIS                   PIC X.
024100     02  RNBFOIS REDEFINES NBFOIS PIC 9.
024200******************************************************************
024300*             T A B L E    D E   T R A V A I L                   *
024400******************************************************************
024500 01  WTABLE.
024600     02 TAB01 OCCURS 7.
024700        03 TAB OCCURS   72   PIC 99.
024800 SCREEN SECTION.
024900 01  SAISIE-AN.
025000     05 BLANK SCREEN FOREGROUND-COLOR 3.
025100     05 LINE 1  COL 15 "Impression de calendrier".
025200     05 LINE 5  COL 15 "Tape l'annee ou fin pour sortir : ".
025300 01  CAS-150AN.
025400     05 BLANK SCREEN FOREGROUND-COLOR 6.
025500     05 LINE 1  COL 15 "Impression de 150 annees de calendrier".
025600     05 LINE 5  COL 15 "Impression speciale de 2022 a 2172 ...".
025700     05 LINE 7  COL 15 "Merci de patienter LoL ...".
025800 PROCEDURE DIVISION.
025900*              *****************************
026000*              *  OUVERTURE  DES FICHIERS. *
026100*              *****************************
026200 OUVERTURE.
026300                 OPEN  OUTPUT FIMP.
026400*                ************************
026500*                * LECTURE D'UNE ANNEE  *
026600*                ************************
026700 LECTURE.
026800       DISPLAY SAISIE-AN.
026900       ACCEPT ANNEE.
027000       IF ANNEE = "fin"   CLOSE FIMP
027100                          STOP RUN.
027200 B150ANS.
027300       IF ANNEE = "spec" MOVE "spec" TO IMP150A
027400                 MOVE "2022" TO ANNEE
027500                 DISPLAY CAS-150AN.
027600       IF IMP150A = "spec" AND ANNEE = "2173" GO TO LECTURE.
027700*          ****************************************
027800*          * CONTROLE DE LA NUMERICITE DE L'ANNEE *
027900*          ****************************************
028000     IF AN NOT NUMERIC   MOVE SPACES TO LIGNE1
028100                         MOVE CARTE TO LIGNE2
028200                         MOVE "ANNEE NON NUMERIQUE" TO TLIGNE2
028300*                        WRITE LIGNE AFTER ADVANCING PAGE
028400                         GO TO LECTURE.
028500*    IF NBFOIS = " " OR RNBFOIS NOT NUMERIC
028600                         MOVE 1 TO RNBFOIS.
028700* TEST RUPTURE ANNEE LUE PRECEDEMENT ET DERNIERE ANNEE LUE
028800     IF ANPRE = ANNEE       GO TO IMP.
028900     MOVE ANNEE TO ANPRE.
029000******************************************************************
029100*         RECHERCHE SI L'ANNEE EST BISSEXTILE                    *
029200******************************************************************
029300*
029400* UNE ANNEE EST BISSEXTILE SI DIVISIBLE PAR 4 MAIS NON DIVISIBLE
029500* PAR 100 SAUF SI DIVISIBLE PAR 400
029600*    EXEMPLE: 1900 N'EST PAS BISSEXTILE
029700*             2000 EST BISSEXTILE
029800*
029900     DIVIDE AN BY 4 GIVING Q REMAINDER R.
030000     IF R = 0
030100        DIVIDE AN BY 100 GIVING Q REMAINDER R1.
030200     IF R = 0 AND R1 = 0
030300        DIVIDE AN BY 400 GIVING Q REMAINDER R.
030400******************************************************************
030500* MISE A JOUR DU POSTE 2 (FEVRIER) DE LA TABLE DES JOURS DU MOIS *
030600******************************************************************
030700     IF R = 0          MOVE 29 TO NBJOURMOIS (2)
030800              ELSE    MOVE 28 TO NBJOURMOIS (2).
030900********************************************************************
031000*                                                                  *
031100*  CALCUL DE L'INDICE J QUI REPRESENTE LE PREMIER JOUR             *
031200*  DE L'ANNEE DANS LA TABLE TABLE-JOUR                             *
031300*  APPLICATION DE LA FORMULE:                                      *
031400*                                                                  *
031500*           I            IAN-1I         IAN-1I         IAN-1I I    *
031600*           I AN + RESTE I----I - RESTE I----I + RESTE I----I I    *
031700*           I            I 4  I         I100 I         I400 I I    *
031800* J = RESTE I-------------------------------------------------I    *
031900*           I                       7                         I    *
032000*                                                                  *
032100*   SI J = 0 ===> J = 7                                            *
032200*                                                                  *
032300*         I A I                                                    *
032400* ( RESTE I---I SIGNIFIE RESTE DE LA DIVISION ENTIERE DE A PAR B ) *
032500*         I B I                               *******              *
032600*                                                                  *
032700********************************************************************
032800* Z = AN
032900     MOVE AN TO Z
033000* W = Z - 1 = AN - 1
033100     SUBTRACT 1 FROM Z GIVING W
033200* PRISE EN COMPTE DES ANNEES MULTIPLES DE 4 (EN PLUS)
033300     MULTIPLY W BY 0.25 GIVING X
033400     ADD X TO Z
033500* PRISE EN COMPTE DES ANNEES MULTIPLES DE 100 (EN MOINS)
033600     MULTIPLY W BY 0.01 GIVING X
033700     SUBTRACT X FROM Z
033800* PRISE EN COMPTE DES ANNEES MULTIPLES DE 400 (EN PLUS)
033900     MULTIPLY W BY 0.0025 GIVING X
034000     ADD X TO Z
034100* DIVISION PAR 7 (MODULO 7)
034200     DIVIDE Z BY 7 GIVING Q REMAINDER J
034300     IF J NOT GREATER 0
034400           ADD 7 TO J.
034500*         **************************************
034600*         * MODULE DE REMPLISSAGE DE LA TABLE. *
034700*         **************************************
034800     MOVE SPACES TO WTABLE.
034900     MOVE 1 TO I K.
035000 SI1.
035100     MOVE 1 TO CPT.
035200 SI2.
035300         IF J GREATER 7       MOVE 1 TO J
035400                        IF CPT NOT = 1       ADD 1 TO I.
035500         MOVE CPT TO TAB (J, I)
035600         ADD 1 TO CPT
035700         ADD 1 TO J
035800         IF CPT NOT GREATER NBJOURMOIS (K)       GO TO SI2.
035900       MULTIPLY K BY 6 GIVING I
036000       ADD 1 TO I
036100       ADD 1 TO K
036200     IF K NOT GREATER 12       GO TO SI1.
036300 IMP.
036400*      ************************************************
036500*      * APPEL DU MODULE D IMPRESSION DES CALENDRIERS *
036600*      ************************************************
036700     PERFORM IMPCAL THRU FIMPCAL RNBFOIS TIMES.
036800     IF IMP150A = "spec"    ADD 1 TO AN
036900                        GO TO B150ANS.
037000     GO TO LECTURE.
037100
037200******************************************************************
037300*                                                                *
037400*   MODULE D'IMPRESSION DES LIGNES DU CALENDRIER                 *
037500*                                                                *
037600******************************************************************
037700 IMPCAL.
037800*
037900*   IMPRESSION DE L'ANNEE EN 'GRANDS' CHIFFRES.
038000*
038100     MOVE SPACES TO LIGNE1
038200     ADD 1 TO NBPAGES
038300     IF NBPAGES > 1       WRITE LIGNE AFTER ADVANCING PAGE.
038400*   IMPRESSION TEXTE de haut de page
038500     MOVE TEXTEHAUT TO LIGNE1
038600     WRITE LIGNE
038700     MOVE SPACES TO LIGNE1
038800     WRITE LIGNE AFTER ADVANCING 2 LINES
038900     MOVE 1 TO I.
039000 SI3.
039100     MOVE 1 TO J.
039200 SI4.
039300         MOVE CHIF-AN (J) TO K.
039400         IF K = 0       MOVE 10 TO K.
039500         MOVE LIG-CHIF (K, I) TO LIGNE2-1 (J)
039600         ADD 1 TO J
039700       IF J NOT GREATER 4       GO TO SI4.
039800       WRITE LIGNE AFTER ADVANCING 1 LINE
039900       MOVE SPACES TO LIGNE1
040000       ADD 1 TO I
040100     IF I NOT GREATER 12       GO TO SI3.
040200     WRITE LIGNE AFTER ADVANCING 3 LINES.
040300*
040400*      IMPRESSION DES LIGNES DU CALENDRIER PROPREMENT DITES.
040500*
040600     MOVE 1 TO M.
040700 SI5.
040800     MOVE 1 TO K.
040900 SI6.
041000     MOVE SPACES TO LIGNE1
041100     IF K = 1 OR 5       MOVE ALL "*" TO ASTERX.
041200     IF K = 2 OR 3 OR 4 OR 6
041300                             MOVE "*" TO AST1 AST2.
041400     MOVE 1 TO I.
041500 SI7.
041600       IF K = 2 OR 3 OR 4 OR 6
041700                  MOVE "*" TO AST (I)
041800                  IF K = 3
041900                          MOVE LIBELMOIS (I) TO LIBMOIS (I)
042000                          IF M = 37
042100                              ADD 6 I GIVING J
042200                             MOVE LIBELMOIS (J) TO LIBMOIS (I).
042300       ADD 1 TO I.
042400       IF I NOT GREATER 6       GO TO SI7.
042500       ADD 1 TO K
042600       WRITE LIGNE AFTER ADVANCING 1 LINE
042700     IF K NOT GREATER 6       GO TO SI6.
042800     MOVE 1 TO K.
042900 SI8.
043000     MOVE SPACES TO LIGNE1
043100     MOVE 1 TO I
043200     MOVE M TO L
043300     MOVE "*" TO AST1 AST2.
043400 SI9.
043500     MOVE 1 TO J.
043600 SI10.
043700             IF TAB (K, L) NOT = SPACES
043800                      MOVE TAB (K, L) TO NJOUR (I, J).
043900             ADD 1 TO J
044000             ADD 1 TO L
044100           IF J NOT GREATER 6       GO TO SI10.
044200           MOVE "*" TO AST (I)
044300           ADD 1 TO I
044400         IF I NOT GREATER 6       GO TO SI9.
044500         MOVE LIBELJOUR (K) TO LIBJOUR
044600         WRITE LIGNE AFTER ADVANCING 1 LINE
044700         MOVE SPACES TO LIGNE1
044800         MOVE "*" TO AST1 AST2 AST (1) AST (2) AST (3)
044900                               AST (4) AST (5) AST (6)
045000         WRITE LIGNE AFTER ADVANCING 1 LINE
045100         ADD 1 TO K
045200       IF K NOT GREATER 7       GO TO SI8.
045300       ADD 36 TO M
045400     IF M NOT GREATER 37       GO TO SI5.
045500     MOVE ALL "*" TO ASTERX
045600     WRITE LIGNE AFTER ADVANCING 1 LINE.
045700 FIMPCAL.
045800     EXIT.

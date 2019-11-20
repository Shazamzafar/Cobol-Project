       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASEBALL-4.
      * Shazam Zafar  
      ******************************************************************
      *
      *    The program will read one input file and write one output file
      *    The purpose of the program is to print a report    
      *    their statistics involved.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
     
           SELECT BASEBALL-4-FILE-IN
               ASSIGN TO 'BASEBALL-4.SEQ'
               ORGANIZATION IS LINE SEQUENTIAL.
     
           SELECT BASEBALL-4-FILE-OUT
               ASSIGN TO 'BASEBALL-4.RPT'
               ORGANIZATION IS LINE SEQUENTIAL.
     
       DATA DIVISION.
       FILE SECTION.
     
       FD  BASEBALL-4-FILE-IN.
       01  BASEBALL-RECORD-IN.
           05  NAME-IN                 PIC X(18).
           05                          PIC X(2).
           05  LEAGUE-IN               PIC X(2).
           05                          PIC X(1).
           05  TEAM-IN                 PIC X(3).
           05                          PIC X(3).
           05  AT-BATS-IN              PIC 9(3).
           05                          PIC X(1).
           05  HITS-IN                 PIC 9(3).
  
           
     
       FD  BASEBALL-4-FILE-OUT.
       01  BASEBALL-4-RECORD-OUT       PIC X(80).
 
         
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS      PIC X(3)  VALUE 'YES'.
       01  FIRST-TIME-THROUGH-FLAG     PIC X(3)  VALUE 'YES'.
       01  LINES-PRINTED               PIC 99    VALUE ZERO.
       01  PAGE-NUMBER                 PIC 99    VALUE ZERO.
       01  CURRENT-DATE.
           05  YEAR-X                   PIC XX.
           05  MONTH-X                  PIC XX.
           05  DAY-X                    PIC XX.
       01  WS-AVERAGE                   PIC 9(3)V9(3).
       01  TOTAL-AT-BATS                PIC 9(5).
       01  TOTAL-HITS                   PIC 9(5).
       01  TOTAL-AVERAGE                PIC 9(3)V9(3).
       01  WS-TEAM-HITS                 PIC 9(8)V99 VALUE ZERO.
       01  WS-TEAM-AT-BATS              PIC 9(8)V99 VALUE ZERO.
       01  WS-LEAGUE-HITS               PIC 9(8)V99 VALUE ZERO.
       01  WS-LEAGUE-AT-BATS            PIC 9(8)V99 VALUE ZERO.
       01  WS-HOLD-LEAGUE               PIC X(2).
       01  WS-HOLD-TEAM                 PIC X(3).
       01  WS-HIGH-AVERAGE              PIC V9(3) VALUE ZERO.
       01  WS-LOW-AVERAGE               PIC V9(3) VALUE .999.


       01  HEADING-LINE-1.
           05                          PIC X(23) VALUE SPACES.
           05                          PIC X(27)
               VALUE 'PLAYER BASEBALL STATISTICS'.
           05                          PIC X(9)  VALUE SPACES.
           05  HL-1-DATE.
               10  HL-1-MONTH          PIC XX.
               10                      PIC X     VALUE '/'.
               10  HL-1-DAY            PIC XX.
               10                      PIC X     VALUE '/'.
               10  HL-1-YEAR           PIC XX.
           05                          PIC X(4)  VALUE SPACES.
           05                          PIC X(5)  VALUE 'PAGE'.
           05  HL-1-PAGE-NUMBER        PIC Z9.
 
       01  HEADING-LINE-2.
           05                          PIC X(6)  VALUE 'LEAGUE'.
           05                          PIC X(3)  VALUE SPACES.
           05                          PIC X(5)  VALUE 'TEAM'.
           05                          PIC X(6) VALUE SPACES.
           05                          PIC X(4)  VALUE 'NAME'.
           05                          PIC X(18) VALUE SPACES.
           05                          PIC X(4)  VALUE 'HITS'.
           05                          PIC X(9) VALUE SPACES.
           05                          PIC X(7)  VALUE 'AT-BATS'.
           05                          PIC X(6)  VALUE SPACES.
           05                          PIC X(7)  VALUE 'AVERAGE'.
 
 
       01  DETAIL-LINE.
           05  DL-LEAGUE               PIC XX.
           05                          PIC X(7)  VALUE SPACES.
           05  DL-TEAM                 PIC X(11).
           05  DL-NAME                 PIC X(20).
                                      
           05  DL-HITS                 PIC ZZ,ZZ9.
           05                          PIC X(10)  VALUE SPACES.
           05  DL-AT-BATS              PIC ZZ,ZZ9.
           05                          PIC X(6) VALUE SPACES.
           05  DL-AVERAGE              PIC Z(3).9(3).
       
       01  TOTAL-LINE.
           05                          PIC X(19) VALUE SPACES.
           05                          PIC X(15) VALUE '* FINAL TOTAL *'.
           05                          PIC X(5) VALUE SPACES.
           05  TL-HITS                 PIC ZZZ,ZZ9.
           05                          PIC X(10)  VALUE SPACES.
           05  TL-AT-BATS              PIC ZZZ,ZZ9.
           05                          PIC X(5) VALUE SPACES.
           05  TL-AVERAGE              PIC Z(3).9(3).
       01  TEAM-TOTAL.
           05  TT-LEAGUE-NAME          PIC X(2).
           05                          PIC X(7) VALUE SPACES.
           05  TT-TEAM-NAME            PIC X(3).
           05                          PIC X(7) VALUE SPACES.
           05                          PIC X(15) VALUE '* Team Totals *'.
           05                          PIC X(7) VALUE SPACES.
           05  TT-HITS-NAME            PIC Z,999.
           05                          PIC X(11) VALUE SPACES.
           05  TT-AT-BATS-NAME         PIC Z,999.
           05                          PIC X(9) VALUE SPACES.
           05  TT-AVERAGE-NAME         PIC .999.
       01  LEAGUE-TOTAL.
           05  LT-LEAGUE-NAME          PIC X(2).
           05                          PIC X(17).
           05                          PIC X(17) VALUE '* League Totals *'.
           05                          PIC X(4) VALUE SPACES.
           05  LT-HITS-NAME            PIC Z9,999.
           05                          PIC X(11) VALUE SPACES.
           05  LT-AT-BATS-NAME         PIC Z9,999.
           05                          PIC X(8) VALUE SPACES.
           05  LT-AVERAGE-NAME         PIC .999.    
       01 HIGH-LOW-LINE.
           05                          PIC X(19) VALUE SPACES.
           05  HILO-HEADING            PIC X(16).
           05                          PIC X(33) VALUE SPACES.
           05  HILO-AVERAGE            PIC Z(3).9(3).

           
     
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT BASEBALL-4-FILE-IN
           OPEN OUTPUT BASEBALL-4-FILE-OUT
           
           ACCEPT CURRENT-DATE FROM DATE
           MOVE MONTH-X TO HL-1-MONTH
           MOVE DAY-X TO HL-1-DAY
           MOVE YEAR-X TO HL-1-YEAR
           PERFORM 300-WRITE-HEADINGS
     
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ BASEBALL-4-FILE-IN
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                           perform 700-PRINT-TEAMS-TOTAL                                           
                           perform 600-PRINT-LEAGUES-TOTAL
                           
                           
                   NOT AT END
                       PERFORM 200-PROCESS-ONE-RECORD
               END-READ
           END-PERFORM
           perform 500-PRINT-TOTAL
           CLOSE BASEBALL-4-FILE-IN
           CLOSE BASEBALL-4-FILE-OUT
           STOP RUN.
     
       200-PROCESS-ONE-RECORD.
           evaluate TRUE 
               WHEN FIRST-TIME-THROUGH-FLAG = 'YES'
                   MOVE LEAGUE-IN TO WS-HOLD-LEAGUE
                   MOVE TEAM-IN TO WS-HOLD-TEAM
                   MOVE 'NO ' TO FIRST-TIME-THROUGH-FLAG
               WHEN LEAGUE-IN NOT = WS-HOLD-LEAGUE
                   perform 700-PRINT-TEAMS-TOTAL
                   perform 600-PRINT-LEAGUES-TOTAL
      *            MOVE SPACES TO BASEBALL-4-RECORD-OUT
      *            WRITE BASEBALL-4-RECORD-OUT
      *                AFTER ADVANCING PAGE
                   MOVE 58 TO LINES-PRINTED
               WHEN TEAM-IN NOT = WS-HOLD-TEAM
                   perform 700-PRINT-TEAMS-TOTAL
           END-EVALUATE
           
           IF LINES-PRINTED >= 57
              PERFORM 300-WRITE-HEADINGS
           END-IF
           
           compute WS-AVERAGE ROUNDED= HITS-IN / AT-BATS-IN 
           IF WS-AVERAGE < WS-LOW-AVERAGE 
               MOVE WS-AVERAGE TO WS-LOW-AVERAGE
           END-IF
           IF WS-AVERAGE > WS-HIGH-AVERAGE
               MOVE WS-AVERAGE TO WS-HIGH-AVERAGE
           END-IF
           ADD HITS-IN TO TOTAL-HITS, WS-LEAGUE-HITS,WS-TEAM-HITS
           ADD AT-BATS-IN TO TOTAL-AT-BATS,  WS-LEAGUE-AT-BATS, WS-TEAM-AT-BATS
           
           
           MOVE NAME-IN TO DL-NAME    
           MOVE LEAGUE-IN TO DL-LEAGUE    
           MOVE TEAM-IN TO DL-TEAM
           MOVE HITS-IN TO DL-HITS
           MOVE AT-BATS-IN TO DL-AT-BATS
           MOVE WS-AVERAGE TO DL-AVERAGE.
           
           MOVE DETAIL-LINE TO BASEBALL-4-RECORD-OUT.
           WRITE BASEBALL-4-RECORD-OUT
               AFTER ADVANCING 1 LINES
           ADD 1 TO LINES-PRINTED.
       
       300-WRITE-HEADINGS.
           ADD 1 TO PAGE-NUMBER
           MOVE PAGE-NUMBER TO HL-1-PAGE-NUMBER
           MOVE HEADING-LINE-1 TO BASEBALL-4-RECORD-OUT
           WRITE BASEBALL-4-RECORD-OUT
               AFTER ADVANCING PAGE
           MOVE HEADING-LINE-2 TO BASEBALL-4-RECORD-OUT
           WRITE BASEBALL-4-RECORD-OUT
               AFTER ADVANCING 2
           MOVE 3 TO LINES-PRINTED.
       
       
       500-PRINT-TOTAL.
           MOVE TOTAL-AT-BATS TO TL-AT-BATS
           MOVE TOTAL-HITS TO TL-HITS
           compute TOTAL-AVERAGE ROUNDED = TOTAL-HITS /TOTAL-AT-BATS
           MOVE TOTAL-AVERAGE TO TL-AVERAGE
           MOVE TOTAL-LINE TO BASEBALL-4-RECORD-OUT
           WRITE BASEBALL-4-RECORD-OUT
               AFTER advancing 1.
           MOVE '* LOW AVERAGE *' TO HILO-HEADING
           MOVE WS-LOW-AVERAGE TO HILO-AVERAGE
           MOVE HIGH-LOW-LINE TO BASEBALL-4-RECORD-OUT
           WRITE BASEBALL-4-RECORD-OUT
               AFTER advancing 2 LINE.
           MOVE '* HIGH AVERAGE *' TO HILO-HEADING
           MOVE WS-HIGH-AVERAGE TO HILO-AVERAGE
           MOVE HIGH-LOW-LINE TO BASEBALL-4-RECORD-OUT
           WRITE BASEBALL-4-RECORD-OUT
               AFTER advancing 2 LINE.   
               
       600-PRINT-LEAGUES-TOTAL.
           MOVE WS-HOLD-LEAGUE TO LT-LEAGUE-NAME
           MOVE 'YES' TO  FIRST-TIME-THROUGH-FLAG
           MOVE WS-LEAGUE-HITS TO LT-HITS-NAME
           MOVE WS-LEAGUE-AT-BATS TO LT-AT-BATS-NAME
           COMPUTE LT-AVERAGE-NAME ROUNDED = WS-LEAGUE-HITS / WS-LEAGUE-AT-BATS
           MOVE SPACE TO WS-HOLD-TEAM
           MOVE SPACE TO WS-HOLD-LEAGUE
           MOVE LEAGUE-TOTAL TO BASEBALL-4-RECORD-OUT
           WRITE BASEBALL-4-RECORD-OUT
               AFTER ADVANCING 2 LINE.
           MOVE spaces TO BASEBALL-4-RECORD-OUT               
           WRITE BASEBALL-4-RECORD-OUT
               AFTER ADVANCING 1 line.
           ADD 3 TO LINES-PRINTED.
           MOVE 0 TO WS-LEAGUE-HITS.
           MOVE 0 TO WS-LEAGUE-AT-BATS.
       

           
       700-PRINT-TEAMS-TOTAL.
           MOVE WS-HOLD-LEAGUE TO TT-LEAGUE-NAME
           MOVE WS-HOLD-TEAM TO TT-TEAM-NAME
           MOVE 'YES' TO  FIRST-TIME-THROUGH-FLAG
           MOVE WS-TEAM-HITS TO TT-HITS-NAME
           MOVE WS-TEAM-AT-BATS TO TT-AT-BATS-NAME
           COMPUTE TT-AVERAGE-NAME ROUNDED = WS-TEAM-HITS / WS-TEAM-AT-BATS
           MOVE SPACE TO WS-HOLD-TEAM
           MOVE TEAM-TOTAL TO BASEBALL-4-RECORD-OUT
           WRITE BASEBALL-4-RECORD-OUT
               AFTER ADVANCING 2 LINE.
           MOVE spaces TO BASEBALL-4-RECORD-OUT
           WRITE BASEBALL-4-RECORD-OUT
               AFTER ADVANCING 1 line.
           ADD 3 TO LINES-PRINTED.
           MOVE 0 TO WS-TEAM-HITS.
           MOVE 0 TO WS-TEAM-AT-BATS.
           
               
               
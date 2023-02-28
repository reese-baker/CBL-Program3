       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGRAM3.
       AUTHOR.        REESE BAKER.
      ******************************************************************
      * This program reads a data file from an external data file.
      * It will create a report to be printed.
      *
      ***INPUT: The Treat file contains the following data:
      * TRUCK ID, Employee ID, Employee Title, Employee Last and First
      * Name, Hire Date, Current Yearly Salary. It also holds an array
      * consisting of: Treat Name, Treat Size, Number in Stock, and
      * Selling Price.
      *
      ***OUTPUT: The Treat Report file contains the following data:
      * date, business name, page number, detail report, TRUCK ID, 
      * TREAT NAME, TREAT SIZE, TREAT STOCK, TREAT PRICE, revenue,
      * total possible revenue, and grand total possible revenue.
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

      *

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TREAT-FILE
             ASSIGN TO 'PR3FA22-TREAT.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

           SELECT TREAT-REPORT
             ASSIGN TO 'TREAT-REPORT.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

       DATA DIVISION.
       FILE SECTION.

      *

       FD TREAT-FILE 
          RECORD CONTAINS 118 CHARACTERS. 

      *

       01 TREAT-RECORD.
          05 TR-TRUCK-ID                       PIC  X(4).
          05 TR-EMPLOYEE-ID                    PIC  X(5).
          05 TR-EMPLOYEE-TITLE                 PIC  A(2).
          05 TR-EMPLOYEE-LNAME                 PIC  X(10).
          05 TR-EMPLOYEE-FNAME                 PIC  X(10).
          05 TR-HIRE-DATE                      PIC  9(8).
          05 TR-CURRENT-YEARLY-SALARY          PIC  9(6).
          05 FILLER                            PIC  X(1).
          05 TR-TREAT-DATA    OCCURS 3 TIMES.
             10 TR-TREAT-NAME                  PIC X(15).
             10 TR-TREAT-SIZE                  PIC  A(1).
             10 TR-TREAT-STOCK                 PIC  9(4).
             10 TR-TREAT-PRICE                 PIC 99V99.

      *

       FD TREAT-REPORT
         RECORD CONTAINS 80 CHARACTERS.

      *

       01 REPORT-LINE                          PIC X(80).

      *

       WORKING-STORAGE SECTION.

      *

       01 FLAGS-N-SWITCHES.
          05 EOF-FLAG                          PIC X VALUE ' '.
             88 NO-MORE-DATA                         VALUE 'N'.
             88 MORE-RECORDS                         VALUE 'Y'.
          05 FIRST-RECORD                      PIC X VALUE 'Y'.

      *

       01 HOLD-FIELD.
          05 HF-TRUCK-ID                       PIC X(4).
      *

       01 TEMP-FIELD.
          05 TRUCK-TOTAL                       PIC S9(7)V99.
          05 TRUCK-GRAND-TOTAL                 PIC S9(8)V99.

      *

      *

       01 REPORT-FIELDS.
          05 PROPER-SPACING                    PIC 9 VALUE 1.
          05 SUB                               PIC 9 VALUE 2.
          05 PAGE-NUMBER                       PIC S9(2) VALUE +0.

      *

       01 WS-CURRENT-DATE.
          05 WS-MONTH                          PIC 99.
          05 WS-DAY                            PIC 99.
          05 WS-YEAR                           PIC 9999.

      *

       01 TOTAL-FIELDS.
          05 TF-POSS-REVENUE                PIC S9(7)V99.
          05 TF-GRAND-TOTAL                 PIC S9(8)V99.
      
      *************************OUTPUT AREA**************************

       01 HEADING-ONE.
          05 H1-DATE.
             10 H1-MONTH                       PIC Z9.
             10 FILLER                         PIC X VALUE '/'.
             10 H1-DAY                         PIC 99.
             10 FILLER                         PIC X VALUE '/'.
             10 H1-YEAR                        PIC 9999.
          05 FILLER                            PIC X(25) VALUE SPACES.
          05 FILLER                            PIC X(13) VALUE 
                                                 'ROLLING PIZZA'.
          05 FILLER                            PIC X(29) VALUE SPACES.
          05 H1-PAGE-NUMBER                    PIC 99 VALUE ZERO.

      *

       01 HEADING-TWO.
          05 FILLER                            PIC X(30) VALUE SPACES.
          05 FILLER                            PIC X(23) VALUE 
                                             'NEW TRUCK DETAIL REPORT'.  
          05 FILLER                            PIC X(26) VALUE SPACES.

      *

       01 TRUCK-LINE.
          05 FILLER                            PIC X(2) VALUE SPACES. 
          05 FILLER                            PIC X(6) VALUE 'TRUCK:'.
          05 TL-TRUCK-ID                       PIC X(10).
          05 FILLER                            PIC X(60) VALUE SPACES.

      *

       01 HEADING-THREE.
          05 FILLER                            PIC X(5) VALUE SPACES.
          05 FILLER                            PIC X(10) VALUE 
                                                         'TREAT NAME'.
          05 FILLER                            PIC X(11) VALUE SPACES.
          05 FILLER                            PIC X(4) VALUE 'SIZE'.
          05 FILLER                            PIC X(8) VALUE SPACES.
          05 FILLER                            PIC X(5) VALUE 'STOCK'.
          05 FILLER                            PIC X(5) VALUE SPACES.
          05 FILLER                            PIC X(5) VALUE 'PRICE'.
          05 FILLER                            PIC X(8) VALUE SPACES.
          05 FILLER                            PIC X(7) VALUE 
                                                            'REVENUE'.
          05 FILLER                            PIC X(11) VALUE SPACES.

      *


      *

       01 DETAIL-LINE.
          05 FILLER                         PIC X(3) VALUE SPACES.
          05 DL-TREAT-NAME                  PIC X(15).
          05 FILLER                         PIC X(5) VALUE SPACES.
          05 DL-TREAT-SIZE                  PIC X(10).
          05 FILLER                         PIC X(5) VALUE SPACES.
          05 DL-TREAT-STOCK                 PIC Z,ZZ9.
          05 FILLER                         PIC X(6) VALUE SPACES.
          05 DL-TREAT-PRICE                 PIC $Z.99.
          05 FILLER                         PIC X(7) VALUE SPACES.
          05 DL-TREAT-REVENUE               PIC $Z,ZZZ,ZZ9.99.
          05 FILLER                         PIC X(11) VALUE SPACES. 

      *

       01 TOTAL-LINE.
          05 FILLER                            PIC X(3) VALUE SPACES.
          05 FILLER                            PIC X(24) VALUE
                                            'TOTAL POSSIBLE REVENUE: '.
          05 TL-POSS-REVENUE                   PIC $Z,Z99,999.99.
          05 FILLER                            PIC X(41) VALUE SPACES.

      *

       01 GRAND-TOTAL-LINE.
          05 FILLER                            PIC X(3) VALUE SPACES.
          05 FILLER                            PIC X(30) VALUE 
                                      'GRAND TOTAL POSSIBLE REVENUE: '.
          05 GL-GRAND-TOTAL                    PIC $ZZ,999,999.99.
          05 FILLER                            PIC X(34) VALUE SPACES.

      *

       PROCEDURE DIVISION.

       10-PRINT-TREAT-REPORT.
         PERFORM 15-HSKPING-ROUTINE
         PERFORM 20-READ-FILE
         PERFORM 55-FINAL-ROUTINE         
       .

       15-HSKPING-ROUTINE.

         OPEN INPUT  TREAT-FILE
              OUTPUT TREAT-REPORT

         ACCEPT WS-CURRENT-DATE FROM DATE

         MOVE WS-MONTH TO H1-MONTH
         MOVE WS-DAY   TO H1-DAY
         MOVE WS-YEAR  TO H1-YEAR

         PERFORM 25-HEADER-ROUTINE

       .

       20-READ-FILE.
      
         PERFORM UNTIL NO-MORE-DATA
             READ TREAT-FILE          
                 AT END
                     MOVE 'N' TO EOF-FLAG
                 NOT AT END
                     PERFORM 40-PROCESS-TREAT-RECORD
             END-READ       
         END-PERFORM

       .

       25-HEADER-ROUTINE.

         ADD 1 TO PAGE-NUMBER
         MOVE PAGE-NUMBER TO H1-PAGE-NUMBER

         WRITE REPORT-LINE FROM HEADING-ONE
             AFTER ADVANCING PAGE

         MOVE HEADING-TWO TO REPORT-LINE
         WRITE REPORT-LINE FROM HEADING-TWO
             AFTER ADVANCING 2 LINES
         MOVE 1 TO PROPER-SPACING


       . 

       30-TRUCK-LINE-HEADER.

         IF TR-TRUCK-ID = 'MONT'
             MOVE 'Montgomery' TO TL-TRUCK-ID
         ELSE
             IF TR-TRUCK-ID = 'MOBL'
                 MOVE 'Mobile' TO TL-TRUCK-ID
             END-IF
         END-IF

         WRITE REPORT-LINE FROM TRUCK-LINE
             AFTER ADVANCING 2 LINES

       .

       35-ATTRIBUTE-HEADER.

         WRITE REPORT-LINE FROM HEADING-THREE
             AFTER ADVANCING 2 LINES

         MOVE 2 TO PROPER-SPACING

       .

       40-PROCESS-TREAT-RECORD.

         EVALUATE TRUE
             WHEN FIRST-RECORD = 'Y'
                 MOVE 'N' TO FIRST-RECORD
                 MOVE TR-TRUCK-ID TO HF-TRUCK-ID
                 PERFORM 30-TRUCK-LINE-HEADER
             WHEN TR-TRUCK-ID NOT = HF-TRUCK-ID
                 PERFORM 45-CONTROL-BREAK
                 PERFORM 25-HEADER-ROUTINE
                 PERFORM 30-TRUCK-LINE-HEADER
         END-EVALUATE

         PERFORM 35-ATTRIBUTE-HEADER

         PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             EVALUATE TRUE
                 WHEN SUB = 1
                     MOVE TR-TREAT-NAME(SUB) TO DL-TREAT-NAME
                 WHEN OTHER
                     MOVE SPACES TO DL-TREAT-NAME
             END-EVALUATE


         EVALUATE TR-TREAT-SIZE(SUB)
             WHEN 'L'
                 MOVE 'Large' TO DL-TREAT-SIZE
             WHEN 'M'
                 MOVE 'Medium' TO DL-TREAT-SIZE
             WHEN 'S'
                 MOVE 'Small' TO DL-TREAT-SIZE
             WHEN OTHER
                 MOVE 'ERROR' TO DL-TREAT-SIZE
         END-EVALUATE


         EVALUATE TR-TREAT-STOCK(SUB)
             WHEN NOT NUMERIC
                 MOVE '0' TO DL-TREAT-STOCK, TR-TREAT-STOCK(SUB)
             WHEN NUMERIC
                 MOVE TR-TREAT-STOCK(SUB) TO DL-TREAT-STOCK
         END-EVALUATE


         EVALUATE TR-TREAT-PRICE(SUB)
             WHEN NOT NUMERIC 
                 MOVE '0' TO DL-TREAT-PRICE, TR-TREAT-PRICE(SUB)
             WHEN NUMERIC
                 MOVE TR-TREAT-PRICE(SUB) TO DL-TREAT-PRICE
         END-EVALUATE


         MULTIPLY TR-TREAT-PRICE(SUB) BY TR-TREAT-STOCK(SUB)
             GIVING DL-TREAT-REVENUE, TF-POSS-REVENUE

         ADD TF-POSS-REVENUE TO TRUCK-TOTAL


         WRITE REPORT-LINE FROM DETAIL-LINE
             AFTER ADVANCING PROPER-SPACING

         MOVE 1 TO PROPER-SPACING

         END-PERFORM

       .

       45-CONTROL-BREAK.

         MOVE TRUCK-TOTAL TO TL-POSS-REVENUE

         WRITE REPORT-LINE FROM TOTAL-LINE
             AFTER ADVANCING 3 LINES

         ADD TRUCK-TOTAL TO TRUCK-GRAND-TOTAL

         MOVE 0 TO TRUCK-TOTAL

         MOVE TR-TRUCK-ID TO HF-TRUCK-ID

         MOVE 1 TO PROPER-SPACING

       .

       50-GRAND-TOTAL.

         MOVE TRUCK-GRAND-TOTAL TO GL-GRAND-TOTAL
         
         WRITE REPORT-LINE FROM GRAND-TOTAL-LINE
             AFTER ADVANCING 2 LINES.



       55-FINAL-ROUTINE.

         PERFORM 45-CONTROL-BREAK
		 PERFORM 50-GRAND-TOTAL

         CLOSE TREAT-FILE
               TREAT-REPORT
  
         STOP RUN

       .


































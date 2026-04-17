*> ============================================================
      *> cob2csv.cbl - Fixed-width COBOL flat file to CSV converter
      *>
      *> Usage:
      *>   cob2csv -s schema.cfg -i input.dat -o output.csv
      *>   cob2csv -s schema.cfg -i input.dat        (stdout)
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COB2CSV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  ASSIGN TO DYNAMIC WS-INPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO DYNAMIC WS-OUTPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SCHEMA-FILE ASSIGN TO DYNAMIC WS-SCHEMA-FILE
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD              PIC X(4096).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD             PIC X(8192).

       FD  SCHEMA-FILE.
       01  SCHEMA-RECORD             PIC X(256).

       WORKING-STORAGE SECTION.

      *> --- CLI args ---
       01  WS-SCHEMA-FILE            PIC X(256) VALUE SPACES.
       01  WS-INPUT-FILE             PIC X(256) VALUE SPACES.
       01  WS-OUTPUT-FILE            PIC X(256) VALUE SPACES.
       01  WS-TO-STDOUT              PIC X VALUE 'Y'.
           88  OUTPUT-TO-STDOUT      VALUE 'Y'.
           88  OUTPUT-TO-FILE        VALUE 'N'.

      *> --- Command line parsing ---
       01  WS-CMDLINE                PIC X(1024) VALUE SPACES.
       01  WS-CMD-POS                PIC 9(5) VALUE 1.
       01  WS-CMD-LEN                PIC 9(5) VALUE 0.
       01  WS-CMD-TOKEN              PIC X(256) VALUE SPACES.
       01  WS-CMD-CHAR               PIC X VALUE SPACES.
       01  WS-CMD-TOK-POS            PIC 9(5) VALUE 1.
       01  WS-LAST-FLAG              PIC X(4) VALUE SPACES.

      *> --- Schema table ---
       01  WS-FIELD-COUNT            PIC 9(3) VALUE 0.
       01  WS-FIELDS OCCURS 64 TIMES INDEXED BY WS-FLD-IDX.
           05  WS-FIELD-NAME         PIC X(32).
           05  WS-FIELD-START        PIC 9(5).
           05  WS-FIELD-LENGTH       PIC 9(5).
           05  WS-FIELD-TYPE         PIC X(8).
           05  WS-FIELD-DECIMALS     PIC 9(2) VALUE 0.

      *> --- Schema parsing ---
       01  WS-RAW-LINE               PIC X(256).
       01  WS-PARSE-TOKEN            PIC X(64).
       01  WS-PARSE-VALUE            PIC X(64).
       01  WS-PARSE-POS              PIC 9(5).
       01  WS-PARSE-CHAR             PIC X.
       01  WS-TOKEN-POS              PIC 9(3).
       01  WS-EQ-POS                 PIC 9(3).
       01  WS-LINE-LEN               PIC 9(5).
       01  WS-FIELD-IDX-NUM          PIC 9(3).

      *> --- Conversion working storage ---
       01  WS-CSV-LINE               PIC X(8192).
       01  WS-CSV-POS                PIC 9(5).
       01  WS-FIELD-VAL              PIC X(512).
       01  WS-FIELD-TRIMMED          PIC X(512).
       01  WS-FIELD-LEN              PIC 9(5).
       01  WS-DECIMAL-STR            PIC X(64).
       01  WS-DECIMAL-INT            PIC X(32).
       01  WS-DECIMAL-FRAC           PIC X(32).
       01  WS-DECIMAL-LEN            PIC 9(3).
       01  WS-INT-LEN                PIC 9(3).
       01  WS-FRAC-LEN               PIC 9(3).
       01  WS-NEEDS-QUOTE            PIC X VALUE 'N'.
       01  WS-CHAR-IDX               PIC 9(5).
       01  WS-CHECK-CHAR             PIC X.
       01  WS-RECORD-COUNT           PIC 9(9) VALUE 0.
       01  WS-EXTRACT-START          PIC 9(5).
       01  WS-EXTRACT-LEN            PIC 9(5).

       PROCEDURE DIVISION.

       MAIN.
           PERFORM PARSE-CMDLINE
           PERFORM VALIDATE-ARGS
           PERFORM LOAD-SCHEMA
           PERFORM OPEN-FILES
           PERFORM WRITE-HEADER
           PERFORM PROCESS-RECORDS
           PERFORM CLOSE-FILES
           DISPLAY "Done. " WS-RECORD-COUNT " records converted."
           STOP RUN.

      *> --------------------------------------------------------
      *> Parse command line - ACCEPT gives entire line as string
      *> --------------------------------------------------------
       PARSE-CMDLINE.
           ACCEPT WS-CMDLINE FROM COMMAND-LINE
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-CMDLINE TRAILING))
               TO WS-CMD-LEN
           MOVE 1 TO WS-CMD-POS
           PERFORM UNTIL WS-CMD-POS > WS-CMD-LEN
               PERFORM CMD-SKIP-SPACES
               IF WS-CMD-POS > WS-CMD-LEN
                   EXIT PERFORM
               END-IF
               PERFORM CMD-COLLECT-TOKEN
               IF WS-CMD-TOKEN NOT = SPACES
                   PERFORM CMD-APPLY-TOKEN
               END-IF
           END-PERFORM.

       CMD-SKIP-SPACES.
           PERFORM UNTIL WS-CMD-POS > WS-CMD-LEN
               MOVE WS-CMDLINE(WS-CMD-POS:1) TO WS-CMD-CHAR
               IF WS-CMD-CHAR = ' '
                   ADD 1 TO WS-CMD-POS
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       CMD-COLLECT-TOKEN.
           MOVE SPACES TO WS-CMD-TOKEN
           MOVE 1 TO WS-CMD-TOK-POS
           PERFORM UNTIL WS-CMD-POS > WS-CMD-LEN
               MOVE WS-CMDLINE(WS-CMD-POS:1) TO WS-CMD-CHAR
               IF WS-CMD-CHAR = ' '
                   EXIT PERFORM
               ELSE
                   MOVE WS-CMD-CHAR
                       TO WS-CMD-TOKEN(WS-CMD-TOK-POS:1)
                   ADD 1 TO WS-CMD-TOK-POS
                   ADD 1 TO WS-CMD-POS
               END-IF
           END-PERFORM.

       CMD-APPLY-TOKEN.
           EVALUATE TRUE
               WHEN WS-CMD-TOKEN(1:2) = '-s'
                   MOVE '-s  ' TO WS-LAST-FLAG
               WHEN WS-CMD-TOKEN(1:8) = '--schema'
                   MOVE '-s  ' TO WS-LAST-FLAG
               WHEN WS-CMD-TOKEN(1:2) = '-i'
                   MOVE '-i  ' TO WS-LAST-FLAG
               WHEN WS-CMD-TOKEN(1:7) = '--input'
                   MOVE '-i  ' TO WS-LAST-FLAG
               WHEN WS-CMD-TOKEN(1:2) = '-o'
                   MOVE '-o  ' TO WS-LAST-FLAG
               WHEN WS-CMD-TOKEN(1:8) = '--output'
                   MOVE '-o  ' TO WS-LAST-FLAG
               WHEN WS-LAST-FLAG = '-s  '
                   MOVE FUNCTION TRIM(WS-CMD-TOKEN)
                       TO WS-SCHEMA-FILE
                   MOVE SPACES TO WS-LAST-FLAG
               WHEN WS-LAST-FLAG = '-i  '
                   MOVE FUNCTION TRIM(WS-CMD-TOKEN)
                       TO WS-INPUT-FILE
                   MOVE SPACES TO WS-LAST-FLAG
               WHEN WS-LAST-FLAG = '-o  '
                   MOVE FUNCTION TRIM(WS-CMD-TOKEN)
                       TO WS-OUTPUT-FILE
                   MOVE 'N' TO WS-TO-STDOUT
                   MOVE SPACES TO WS-LAST-FLAG
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       VALIDATE-ARGS.
           IF WS-SCHEMA-FILE = SPACES
               DISPLAY "ERROR: -s <schema.cfg> is required"
               PERFORM PRINT-USAGE
               STOP RUN
           END-IF
           IF WS-INPUT-FILE = SPACES
               DISPLAY "ERROR: -i <input.dat> is required"
               PERFORM PRINT-USAGE
               STOP RUN
           END-IF.

       PRINT-USAGE.
           DISPLAY "Usage: cob2csv -s schema.cfg -i input.dat"
                   " [-o output.csv]"
           DISPLAY "  -s, --schema   Schema config file"
           DISPLAY "  -i, --input    Fixed-width input file"
           DISPLAY "  -o, --output   Output CSV (default: stdout)".

      *> --------------------------------------------------------
      *> Schema loading
      *> --------------------------------------------------------
       LOAD-SCHEMA.
           OPEN INPUT SCHEMA-FILE
           MOVE 0 TO WS-FIELD-COUNT
           PERFORM UNTIL EXIT
               READ SCHEMA-FILE INTO WS-RAW-LINE
                   AT END EXIT PERFORM
               END-READ
               PERFORM PROCESS-SCHEMA-LINE
           END-PERFORM
           CLOSE SCHEMA-FILE
           IF WS-FIELD-COUNT = 0
               DISPLAY "ERROR: No fields found in schema: "
                       WS-SCHEMA-FILE
               STOP RUN
           END-IF
           DISPLAY "Schema loaded: " WS-FIELD-COUNT " fields.".

       PROCESS-SCHEMA-LINE.
           MOVE FUNCTION TRIM(WS-RAW-LINE LEADING) TO WS-RAW-LINE
           IF WS-RAW-LINE = SPACES
               EXIT PARAGRAPH
           END-IF
           IF WS-RAW-LINE(1:1) = '#'
               EXIT PARAGRAPH
           END-IF
           IF WS-RAW-LINE(1:5) NOT = 'FIELD'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-RAW-LINE(7:249) TO WS-RAW-LINE
           ADD 1 TO WS-FIELD-COUNT
           MOVE WS-FIELD-COUNT TO WS-FIELD-IDX-NUM
           SET WS-FLD-IDX TO WS-FIELD-IDX-NUM
           MOVE SPACES TO WS-FIELD-NAME(WS-FLD-IDX)
           MOVE 0      TO WS-FIELD-START(WS-FLD-IDX)
           MOVE 0      TO WS-FIELD-LENGTH(WS-FLD-IDX)
           MOVE SPACES TO WS-FIELD-TYPE(WS-FLD-IDX)
           MOVE 0      TO WS-FIELD-DECIMALS(WS-FLD-IDX)
           PERFORM PARSE-SCHEMA-TOKENS.

       PARSE-SCHEMA-TOKENS.
           MOVE 1 TO WS-PARSE-POS
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-RAW-LINE TRAILING))
               TO WS-LINE-LEN
           PERFORM UNTIL WS-PARSE-POS > WS-LINE-LEN
               PERFORM EXTRACT-SCHEMA-TOKEN
               IF WS-PARSE-TOKEN NOT = SPACES
                   PERFORM APPLY-SCHEMA-TOKEN
               END-IF
           END-PERFORM.

       EXTRACT-SCHEMA-TOKEN.
           MOVE SPACES TO WS-PARSE-TOKEN
           PERFORM UNTIL WS-PARSE-POS > WS-LINE-LEN
               MOVE WS-RAW-LINE(WS-PARSE-POS:1) TO WS-PARSE-CHAR
               IF WS-PARSE-CHAR = ' '
                   ADD 1 TO WS-PARSE-POS
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           MOVE 1 TO WS-TOKEN-POS
           PERFORM UNTIL WS-PARSE-POS > WS-LINE-LEN
               MOVE WS-RAW-LINE(WS-PARSE-POS:1) TO WS-PARSE-CHAR
               IF WS-PARSE-CHAR = ' '
                   EXIT PERFORM
               ELSE
                   MOVE WS-PARSE-CHAR
                       TO WS-PARSE-TOKEN(WS-TOKEN-POS:1)
                   ADD 1 TO WS-TOKEN-POS
                   ADD 1 TO WS-PARSE-POS
               END-IF
           END-PERFORM.

       APPLY-SCHEMA-TOKEN.
           MOVE 0 TO WS-EQ-POS
           PERFORM VARYING WS-TOKEN-POS FROM 1 BY 1
               UNTIL WS-TOKEN-POS > 64
               IF WS-PARSE-TOKEN(WS-TOKEN-POS:1) = '='
                   MOVE WS-TOKEN-POS TO WS-EQ-POS
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF WS-EQ-POS = 0
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-PARSE-VALUE
           MOVE WS-PARSE-TOKEN(WS-EQ-POS + 1:
               64 - WS-EQ-POS)
               TO WS-PARSE-VALUE
           MOVE WS-PARSE-TOKEN(1:WS-EQ-POS - 1)
               TO WS-PARSE-TOKEN
           EVALUATE WS-PARSE-TOKEN
               WHEN 'NAME'
                   MOVE FUNCTION TRIM(WS-PARSE-VALUE)
                       TO WS-FIELD-NAME(WS-FLD-IDX)
               WHEN 'START'
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-PARSE-VALUE))
                       TO WS-FIELD-START(WS-FLD-IDX)
               WHEN 'LENGTH'
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-PARSE-VALUE))
                       TO WS-FIELD-LENGTH(WS-FLD-IDX)
               WHEN 'TYPE'
                   MOVE FUNCTION TRIM(WS-PARSE-VALUE)
                       TO WS-FIELD-TYPE(WS-FLD-IDX)
               WHEN 'DECIMALS'
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-PARSE-VALUE))
                       TO WS-FIELD-DECIMALS(WS-FLD-IDX)
           END-EVALUATE.

      *> --------------------------------------------------------
      *> File open / close
      *> --------------------------------------------------------
       OPEN-FILES.
           OPEN INPUT INPUT-FILE
           IF OUTPUT-TO-FILE
               OPEN OUTPUT OUTPUT-FILE
           END-IF.

       CLOSE-FILES.
           CLOSE INPUT-FILE
           IF OUTPUT-TO-FILE
               CLOSE OUTPUT-FILE
           END-IF.

      *> --------------------------------------------------------
      *> Write CSV header row
      *> --------------------------------------------------------
       WRITE-HEADER.
           MOVE SPACES TO WS-CSV-LINE
           MOVE 1 TO WS-CSV-POS
           PERFORM VARYING WS-FLD-IDX FROM 1 BY 1
               UNTIL WS-FLD-IDX > WS-FIELD-COUNT
               MOVE FUNCTION TRIM(WS-FIELD-NAME(WS-FLD-IDX))
                   TO WS-FIELD-TRIMMED
               MOVE FUNCTION LENGTH(
                   FUNCTION TRIM(WS-FIELD-TRIMMED TRAILING))
                   TO WS-FIELD-LEN
               MOVE WS-FIELD-TRIMMED(1:WS-FIELD-LEN)
                   TO WS-CSV-LINE(WS-CSV-POS:WS-FIELD-LEN)
               ADD WS-FIELD-LEN TO WS-CSV-POS
               IF WS-FLD-IDX < WS-FIELD-COUNT
                   MOVE ',' TO WS-CSV-LINE(WS-CSV-POS:1)
                   ADD 1 TO WS-CSV-POS
               END-IF
           END-PERFORM
           PERFORM EMIT-LINE.

      *> --------------------------------------------------------
      *> Record loop
      *> --------------------------------------------------------
       PROCESS-RECORDS.
           PERFORM UNTIL EXIT
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END EXIT PERFORM
               END-READ
               PERFORM CONVERT-RECORD
               ADD 1 TO WS-RECORD-COUNT
           END-PERFORM.

       CONVERT-RECORD.
           MOVE SPACES TO WS-CSV-LINE
           MOVE 1 TO WS-CSV-POS
           PERFORM VARYING WS-FLD-IDX FROM 1 BY 1
               UNTIL WS-FLD-IDX > WS-FIELD-COUNT
               PERFORM EXTRACT-FIELD
               PERFORM FORMAT-FIELD
               PERFORM APPEND-TO-CSV
               IF WS-FLD-IDX < WS-FIELD-COUNT
                   MOVE ',' TO WS-CSV-LINE(WS-CSV-POS:1)
                   ADD 1 TO WS-CSV-POS
               END-IF
           END-PERFORM
           PERFORM EMIT-LINE.

       EXTRACT-FIELD.
           MOVE SPACES TO WS-FIELD-VAL
           MOVE WS-FIELD-START(WS-FLD-IDX)  TO WS-EXTRACT-START
           MOVE WS-FIELD-LENGTH(WS-FLD-IDX) TO WS-EXTRACT-LEN
           MOVE INPUT-RECORD(WS-EXTRACT-START:WS-EXTRACT-LEN)
               TO WS-FIELD-VAL.

       FORMAT-FIELD.
           EVALUATE WS-FIELD-TYPE(WS-FLD-IDX)
               WHEN 'ALPHA'
                   PERFORM FORMAT-ALPHA
               WHEN 'NUM'
                   PERFORM FORMAT-NUM
               WHEN 'DECIMAL'
                   PERFORM FORMAT-DECIMAL
               WHEN OTHER
                   PERFORM FORMAT-ALPHA
           END-EVALUATE.

       FORMAT-ALPHA.
           MOVE FUNCTION TRIM(WS-FIELD-VAL TRAILING)
               TO WS-FIELD-TRIMMED
           MOVE 'N' TO WS-NEEDS-QUOTE
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-FIELD-TRIMMED TRAILING))
               TO WS-FIELD-LEN
           PERFORM VARYING WS-CHAR-IDX FROM 1 BY 1
               UNTIL WS-CHAR-IDX > WS-FIELD-LEN
               MOVE WS-FIELD-TRIMMED(WS-CHAR-IDX:1)
                   TO WS-CHECK-CHAR
               IF WS-CHECK-CHAR = ',' OR WS-CHECK-CHAR = '"'
                   MOVE 'Y' TO WS-NEEDS-QUOTE
               END-IF
           END-PERFORM.

       FORMAT-NUM.
           MOVE FUNCTION TRIM(WS-FIELD-VAL) TO WS-FIELD-TRIMMED
           PERFORM STRIP-LEADING-ZEROS-TRIMMED
           MOVE 'N' TO WS-NEEDS-QUOTE.

       FORMAT-DECIMAL.
      *> Work from raw field value - split BEFORE stripping zeros
           MOVE FUNCTION TRIM(WS-FIELD-VAL) TO WS-DECIMAL-STR
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-DECIMAL-STR TRAILING))
               TO WS-DECIMAL-LEN
           MOVE WS-FIELD-DECIMALS(WS-FLD-IDX) TO WS-FRAC-LEN
           SUBTRACT WS-FRAC-LEN FROM WS-DECIMAL-LEN
               GIVING WS-INT-LEN
      *> Split raw digits into int and frac parts
           MOVE SPACES TO WS-DECIMAL-INT
           MOVE SPACES TO WS-DECIMAL-FRAC
           IF WS-INT-LEN > 0
               MOVE WS-DECIMAL-STR(1:WS-INT-LEN) TO WS-DECIMAL-INT
           ELSE
               MOVE '0' TO WS-DECIMAL-INT
           END-IF
           IF WS-FRAC-LEN > 0
               IF WS-INT-LEN > 0
                   MOVE WS-DECIMAL-STR(WS-INT-LEN + 1:WS-FRAC-LEN)
                       TO WS-DECIMAL-FRAC
               ELSE
                   MOVE WS-DECIMAL-STR(1:WS-FRAC-LEN)
                       TO WS-DECIMAL-FRAC
               END-IF
           END-IF
      *> Strip leading zeros from integer part only
           MOVE FUNCTION TRIM(WS-DECIMAL-INT TRAILING)
               TO WS-FIELD-TRIMMED
           PERFORM STRIP-LEADING-ZEROS-TRIMMED
           MOVE WS-FIELD-TRIMMED TO WS-DECIMAL-INT
      *> Build result
           MOVE SPACES TO WS-FIELD-TRIMMED
           IF WS-FRAC-LEN > 0
               STRING FUNCTION TRIM(WS-DECIMAL-INT TRAILING)
                   '.'
                   WS-DECIMAL-FRAC(1:WS-FRAC-LEN)
                   DELIMITED SIZE
                   INTO WS-FIELD-TRIMMED
           ELSE
               MOVE FUNCTION TRIM(WS-DECIMAL-INT TRAILING)
                   TO WS-FIELD-TRIMMED
           END-IF
           MOVE 'N' TO WS-NEEDS-QUOTE.

       STRIP-LEADING-ZEROS-TRIMMED.
      *> Removes leading '0' chars from WS-FIELD-TRIMMED
      *> Leaves at least one digit if all zeros
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-FIELD-TRIMMED TRAILING))
               TO WS-FIELD-LEN
           MOVE 1 TO WS-CHAR-IDX
           PERFORM UNTIL WS-CHAR-IDX >= WS-FIELD-LEN
               IF WS-FIELD-TRIMMED(WS-CHAR-IDX:1) = '0'
                   ADD 1 TO WS-CHAR-IDX
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF WS-CHAR-IDX > 1
               MOVE WS-FIELD-TRIMMED(WS-CHAR-IDX:
                   WS-FIELD-LEN - WS-CHAR-IDX + 1)
                   TO WS-FIELD-TRIMMED
           END-IF
           IF FUNCTION TRIM(WS-FIELD-TRIMMED TRAILING) = SPACES
               MOVE '0' TO WS-FIELD-TRIMMED
           END-IF.

       APPEND-TO-CSV.
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-FIELD-TRIMMED TRAILING))
               TO WS-FIELD-LEN
           IF WS-NEEDS-QUOTE = 'Y'
               MOVE '"' TO WS-CSV-LINE(WS-CSV-POS:1)
               ADD 1 TO WS-CSV-POS
               MOVE WS-FIELD-TRIMMED(1:WS-FIELD-LEN)
                   TO WS-CSV-LINE(WS-CSV-POS:WS-FIELD-LEN)
               ADD WS-FIELD-LEN TO WS-CSV-POS
               MOVE '"' TO WS-CSV-LINE(WS-CSV-POS:1)
               ADD 1 TO WS-CSV-POS
           ELSE
               MOVE WS-FIELD-TRIMMED(1:WS-FIELD-LEN)
                   TO WS-CSV-LINE(WS-CSV-POS:WS-FIELD-LEN)
               ADD WS-FIELD-LEN TO WS-CSV-POS
           END-IF.

       EMIT-LINE.
           IF OUTPUT-TO-STDOUT
               DISPLAY FUNCTION TRIM(WS-CSV-LINE TRAILING)
           ELSE
               MOVE FUNCTION TRIM(WS-CSV-LINE TRAILING)
                   TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
           END-IF.

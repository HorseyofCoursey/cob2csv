*> ============================================================
      *> csv2cob.cbl - CSV to fixed-width COBOL flat file converter
      *>
      *> Usage:
      *>   csv2cob -s schema.cfg -i input.csv -o output.dat
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSV2COB.

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
       01  INPUT-RECORD              PIC X(8192).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD             PIC X(4096).

       FD  SCHEMA-FILE.
       01  SCHEMA-RECORD             PIC X(256).

       WORKING-STORAGE SECTION.

      *> --- CLI args ---
       01  WS-SCHEMA-FILE            PIC X(256) VALUE SPACES.
       01  WS-INPUT-FILE             PIC X(256) VALUE SPACES.
       01  WS-OUTPUT-FILE            PIC X(256) VALUE SPACES.

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

      *> --- CSV parsing ---
       01  WS-CSV-LINE               PIC X(8192).
       01  WS-CSV-POS                PIC 9(5).
       01  WS-CSV-LEN                PIC 9(5).
       01  WS-IN-QUOTES              PIC X VALUE 'N'.
       01  WS-FIELD-BUF              PIC X(512).
       01  WS-FIELD-BUF-POS          PIC 9(5).
       01  WS-CURRENT-FIELD          PIC 9(3).
       01  WS-CSV-CHAR               PIC X.

      *> --- Output record building ---
       01  WS-OUTPUT-BUF             PIC X(4096) VALUE SPACES.
       01  WS-FIELD-VAL              PIC X(512).
       01  WS-FIELD-TRIMMED          PIC X(512).
       01  WS-DEST-START             PIC 9(5).
       01  WS-DEST-LEN               PIC 9(5).
       01  WS-VAL-LEN                PIC 9(5).
       01  WS-PAD-LEN                PIC 9(5).
       01  WS-PAD-IDX                PIC 9(5).
       01  WS-RECORD-LEN             PIC 9(5) VALUE 0.
       01  WS-RECORD-COUNT           PIC 9(9) VALUE 0.
       01  WS-SKIP-HEADER            PIC X VALUE 'Y'.

       PROCEDURE DIVISION.

       MAIN.
           PERFORM PARSE-CMDLINE
           PERFORM VALIDATE-ARGS
           PERFORM LOAD-SCHEMA
           PERFORM CALC-RECORD-LEN
           PERFORM OPEN-FILES
           PERFORM PROCESS-RECORDS
           PERFORM CLOSE-FILES
           DISPLAY "Done. " WS-RECORD-COUNT " records written."
           STOP RUN.

      *> --------------------------------------------------------
      *> Command line parsing
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
               DISPLAY "ERROR: -i <input.csv> is required"
               PERFORM PRINT-USAGE
               STOP RUN
           END-IF
           IF WS-OUTPUT-FILE = SPACES
               DISPLAY "ERROR: -o <output.dat> is required"
               PERFORM PRINT-USAGE
               STOP RUN
           END-IF.

       PRINT-USAGE.
           DISPLAY "Usage: csv2cob -s schema.cfg"
                   " -i input.csv -o output.dat"
           DISPLAY "  -s, --schema   Schema config file"
           DISPLAY "  -i, --input    CSV input file"
           DISPLAY "  -o, --output   Fixed-width output file".

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
      *> Calculate record length from schema
      *> --------------------------------------------------------
       CALC-RECORD-LEN.
           MOVE 0 TO WS-RECORD-LEN
           PERFORM VARYING WS-FLD-IDX FROM 1 BY 1
               UNTIL WS-FLD-IDX > WS-FIELD-COUNT
               COMPUTE WS-DEST-START =
                   WS-FIELD-START(WS-FLD-IDX)
                   + WS-FIELD-LENGTH(WS-FLD-IDX) - 1
               IF WS-DEST-START > WS-RECORD-LEN
                   MOVE WS-DEST-START TO WS-RECORD-LEN
               END-IF
           END-PERFORM
           DISPLAY "Output record length: " WS-RECORD-LEN " bytes.".

      *> --------------------------------------------------------
      *> File open / close
      *> --------------------------------------------------------
       OPEN-FILES.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE.

       CLOSE-FILES.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.

      *> --------------------------------------------------------
      *> Record loop - skip header line, convert the rest
      *> --------------------------------------------------------
       PROCESS-RECORDS.
           MOVE 'Y' TO WS-SKIP-HEADER
           PERFORM UNTIL EXIT
               READ INPUT-FILE INTO WS-CSV-LINE
                   AT END EXIT PERFORM
               END-READ
               IF WS-SKIP-HEADER = 'Y'
                   MOVE 'N' TO WS-SKIP-HEADER
               ELSE
                   PERFORM CONVERT-CSV-RECORD
                   ADD 1 TO WS-RECORD-COUNT
               END-IF
           END-PERFORM.

       CONVERT-CSV-RECORD.
           MOVE SPACES TO WS-OUTPUT-BUF
           MOVE 1 TO WS-CURRENT-FIELD
           MOVE 1 TO WS-CSV-POS
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-CSV-LINE TRAILING))
               TO WS-CSV-LEN
           MOVE 'N' TO WS-IN-QUOTES
           MOVE SPACES TO WS-FIELD-BUF
           MOVE 1 TO WS-FIELD-BUF-POS
           PERFORM UNTIL WS-CSV-POS > WS-CSV-LEN
               MOVE WS-CSV-LINE(WS-CSV-POS:1) TO WS-CSV-CHAR
               EVALUATE TRUE
                   WHEN WS-CSV-CHAR = '"'
                       IF WS-IN-QUOTES = 'Y'
                           MOVE 'N' TO WS-IN-QUOTES
                       ELSE
                           MOVE 'Y' TO WS-IN-QUOTES
                       END-IF
                   WHEN WS-CSV-CHAR = ','
                       AND WS-IN-QUOTES = 'N'
                       PERFORM WRITE-FIELD-TO-OUTPUT
                       ADD 1 TO WS-CURRENT-FIELD
                       MOVE SPACES TO WS-FIELD-BUF
                       MOVE 1 TO WS-FIELD-BUF-POS
                   WHEN OTHER
                       MOVE WS-CSV-CHAR
                           TO WS-FIELD-BUF(WS-FIELD-BUF-POS:1)
                       ADD 1 TO WS-FIELD-BUF-POS
               END-EVALUATE
               ADD 1 TO WS-CSV-POS
           END-PERFORM
           PERFORM WRITE-FIELD-TO-OUTPUT
           MOVE WS-OUTPUT-BUF(1:WS-RECORD-LEN) TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.

       WRITE-FIELD-TO-OUTPUT.
           IF WS-CURRENT-FIELD > WS-FIELD-COUNT
               EXIT PARAGRAPH
           END-IF
           SET WS-FLD-IDX TO WS-CURRENT-FIELD
           MOVE WS-FIELD-START(WS-FLD-IDX)  TO WS-DEST-START
           MOVE WS-FIELD-LENGTH(WS-FLD-IDX) TO WS-DEST-LEN
           MOVE FUNCTION TRIM(WS-FIELD-BUF TRAILING)
               TO WS-FIELD-TRIMMED
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-FIELD-TRIMMED TRAILING))
               TO WS-VAL-LEN
           EVALUATE WS-FIELD-TYPE(WS-FLD-IDX)
               WHEN 'ALPHA'
      *> Left-justify, space-pad right (buffer already spaces)
                   IF WS-VAL-LEN > WS-DEST-LEN
                       MOVE WS-DEST-LEN TO WS-VAL-LEN
                   END-IF
                   MOVE WS-FIELD-TRIMMED(1:WS-VAL-LEN)
                       TO WS-OUTPUT-BUF(WS-DEST-START:WS-VAL-LEN)
               WHEN 'NUM'
      *> Right-justify, zero-pad left
                   IF WS-VAL-LEN > WS-DEST-LEN
                       MOVE WS-DEST-LEN TO WS-VAL-LEN
                   END-IF
                   COMPUTE WS-PAD-LEN = WS-DEST-LEN - WS-VAL-LEN
                   PERFORM VARYING WS-PAD-IDX FROM 1 BY 1
                       UNTIL WS-PAD-IDX > WS-PAD-LEN
                       MOVE '0' TO WS-OUTPUT-BUF(
                           WS-DEST-START + WS-PAD-IDX - 1:1)
                   END-PERFORM
                   MOVE WS-FIELD-TRIMMED(1:WS-VAL-LEN)
                       TO WS-OUTPUT-BUF(
                           WS-DEST-START + WS-PAD-LEN:WS-VAL-LEN)
               WHEN 'DECIMAL'
      *> Strip decimal point, then right-justify digits
                   MOVE SPACES TO WS-FIELD-VAL
                   MOVE 1 TO WS-FIELD-BUF-POS
                   PERFORM VARYING WS-TOKEN-POS FROM 1 BY 1
                       UNTIL WS-TOKEN-POS > WS-VAL-LEN
                       IF WS-FIELD-TRIMMED(WS-TOKEN-POS:1) NOT = '.'
                           MOVE WS-FIELD-TRIMMED(WS-TOKEN-POS:1)
                               TO WS-FIELD-VAL(WS-FIELD-BUF-POS:1)
                           ADD 1 TO WS-FIELD-BUF-POS
                       END-IF
                   END-PERFORM
                   MOVE FUNCTION TRIM(WS-FIELD-VAL TRAILING)
                       TO WS-FIELD-TRIMMED
                   MOVE FUNCTION LENGTH(
                       FUNCTION TRIM(WS-FIELD-TRIMMED TRAILING))
                       TO WS-VAL-LEN
                   IF WS-VAL-LEN > WS-DEST-LEN
                       MOVE WS-DEST-LEN TO WS-VAL-LEN
                   END-IF
                   COMPUTE WS-PAD-LEN = WS-DEST-LEN - WS-VAL-LEN
                   PERFORM VARYING WS-PAD-IDX FROM 1 BY 1
                       UNTIL WS-PAD-IDX > WS-PAD-LEN
                       MOVE '0' TO WS-OUTPUT-BUF(
                           WS-DEST-START + WS-PAD-IDX - 1:1)
                   END-PERFORM
                   MOVE WS-FIELD-TRIMMED(1:WS-VAL-LEN)
                       TO WS-OUTPUT-BUF(
                           WS-DEST-START + WS-PAD-LEN:WS-VAL-LEN)
           END-EVALUATE.

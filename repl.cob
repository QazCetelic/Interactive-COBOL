000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. INTERACTIVE-COBOL.
000300 DATA DIVISION.
000400 WORKING-STORAGE SECTION.
000500     01 SRC-PATH pic x(100).
000600     01 FILE-INFO.
000700        05 file-size        pic x(8) comp-x.
000800* Modification date and time of the file        
000900        05 file-date.
001000           10 f-day         pic x comp-x.
001100          10 f-month       pic x comp-x.
001200           10 f-year        pic xx comp-x.
001300
001400       05 file-time.
001500           10 f-hours       pic x comp-x.
001600           10 f-minutes     pic x comp-x.
001700           10 f-seconds     pic x comp-x.
001800           10 f-hundredths  pic x comp-x.
001900     01 NEW-FILE-INFO.
002000          05 new-file-size    pic x(8) comp-x.
002100          05 new-file-date.
002200               10 new-f-day      pic x comp-x.
002300               10 new-f-month    pic x comp-x.
002400               10 new-f-year     pic xx comp-x.
002500   
002600          05 new-file-time.
002700               10 new-f-hours    pic x comp-x.
002800               10 new-f-minutes  pic x comp-x.
002900               10 new-f-seconds  pic x comp-x.
003000               10 new-f-hundredths pic x comp-x.
003100     01 WS-TEMP-DT.   
003200         05 WS-TEMP-DATE-TIME.            
003300            10 WS-TEMP-DATE.              
003400               15 WS-TEMP-YEAR  PIC  9(4). 
003500               15 WS-TEMP-MONTH PIC  9(2).
003600               15 WS-TEMP-DAY   PIC  9(2).
003700            10 WS-TEMP-TIME.              
003800               15 WS-TEMP-HOUR  PIC  9(2).
003900               15 WS-TEMP-MIN   PIC  9(2).
004000               15 WS-TEMP-SEC   PIC  9(2).
004100               15 WS-TEMP-MS    PIC  9(2).
004200            10 WS-DIFF-GMT         PIC S9(4).

004400     01 WS-FORMATTED-DT.   
004500         05 WS-FORMATTED-DATE-TIME.                       
004600            15 WS-FORMATTED-YEAR  PIC  9(4). 
004700            15 FILLER             PIC X VALUE '-'.
004800            15 WS-FORMATTED-MONTH PIC  9(2).
004900            15 FILLER             PIC X VALUE '-'.
005000            15 WS-FORMATTED-DAY   PIC  9(2).  
005100            15 FILLER             PIC X VALUE ' '.           
005200            15 WS-FORMATTED-HOUR  PIC  9(2).
005300            15 FILLER             PIC X VALUE ':'.
005400            15 WS-FORMATTED-MIN   PIC  9(2).
005500            15 FILLER             PIC X VALUE ':'.
005600            15 WS-FORMATTED-SEC   PIC  9(2).
005700            15 FILLER             PIC X VALUE ':'.
005800            15 WS-FORMATTED-MS    PIC  9(2).
005900     01 WAITING-MESSAGE-SHOWN pic 9(1) value 0.
006000     01 FILE-TYPE-CMD pic x(130).
006100     01 COMPILE-CMD pic x(130).
006200 PROCEDURE DIVISION.
006300 000-MAIN.
006400     PERFORM 300-CHECK-DEPENDENCIES.
006500     CALL "SYSTEM" USING BY CONTENT "clear".
006600     ACCEPT SRC-PATH FROM COMMAND-LINE.
006700     CALL "CBL_CHECK_FILE_EXIST" USING SRC-PATH FILE-INFO.
006800     IF RETURN-CODE NOT EQUAL ZERO THEN
006900         DISPLAY X"1B" & "[31;1;4mError: File " SRC-PATH(1:20)
007000         " does not exist" X"1B" & "[0m"
007100     ELSE
007200         PERFORM 200-COMPILE-AND-EXECUTE
007300     END-IF.
007400     STOP RUN.
007500 100-CHECK-FOR-CHANGES.
007600     CALL "CBL_CHECK_FILE_EXIST" USING SRC-PATH NEW-FILE-INFO.
007700     IF RETURN-CODE NOT EQUAL ZERO THEN
007800          DISPLAY X"1B" & "[31;1;4mError: File " SRC-PATH(1:20)
007900          " does not exist anymore" X"1B" & "[0m"
008000     END-IF.
008100     IF FILE-INFO NOT = NEW-FILE-INFO
008200         CALL "SYSTEM" USING BY CONTENT "clear"
008300         DISPLAY "File has been modified"
008400         MOVE NEW-FILE-INFO TO FILE-INFO
008500         PERFORM 200-COMPILE-AND-EXECUTE
008600     ELSE
008700         IF WAITING-MESSAGE-SHOWN = 0 THEN
008800             DISPLAY ""
008900             DISPLAY X"1B" & "[33;1mFile has not been modified, "
009000             "waiting..." & X"1B" & "[0m"
009100             MOVE 1 TO WAITING-MESSAGE-SHOWN
009200         END-IF
009300         CONTINUE AFTER 1 SECONDS
009400     END-IF.
009500    GO TO 100-CHECK-FOR-CHANGES.
009600 200-COMPILE-AND-EXECUTE.
009700     MOVE 0 TO WAITING-MESSAGE-SHOWN.
009800* Check if file is an ASCII text file
009900     STRING "file '" DELIMITED BY SIZE
010000         SRC-PATH(1:100) DELIMITED BY SPACE
010100         "' | grep 'ASCII text' &> /dev/null" DELIMITED BY SIZE
010200         INTO FILE-TYPE-CMD
010300     END-STRING.
010400     CALL "SYSTEM" USING FILE-TYPE-CMD RETURNING RETURN-CODE.
010500     IF RETURN-CODE NOT EQUAL ZERO THEN
010600         DISPLAY X"1B" & "[31;1;4mError: File " SRC-PATH(1:100)
010700         " is not a text file" X"1B" & "[0m"
010800         PERFORM 100-CHECK-FOR-CHANGES
010900     END-IF. 
011000* Compile the file
011100     DISPLAY X"1B" & "[33;1mCOMPILER OUTPUT: " X"1B" & "[0m"
011200     STRING "cobc -x " DELIMITED BY SIZE
011300       SRC-PATH(1:100) DELIMITED BY SPACE
011400       " -o /tmp/cobol-interactive" DELIMITED BY SIZE
011500       INTO COMPILE-CMD
011600     END-STRING.
011700     CALL "SYSTEM" USING COMPILE-CMD.
011800* Check if the compilation was successful
011900     CALL "SYSTEM" USING BY CONTENT
012000     "test -x /tmp/cobol-interactive" RETURNING RETURN-CODE.
012100     IF RETURN-CODE NOT EQUAL ZERO THEN
012200         DISPLAY ""
012300         DISPLAY X"1B" & "[31;1;4mPROGRAM FAILED TO COMPILE!"
012400             X"1B" & "[0m"
012500         PERFORM 100-CHECK-FOR-CHANGES
012600     ELSE
012700         MOVE FUNCTION CURRENT-DATE TO WS-TEMP-DATE-TIME
012800         MOVE WS-TEMP-YEAR  TO WS-FORMATTED-YEAR
012900         MOVE WS-TEMP-MONTH TO WS-FORMATTED-MONTH
013000         MOVE WS-TEMP-DAY   TO WS-FORMATTED-DAY
013100         MOVE WS-TEMP-HOUR  TO WS-FORMATTED-HOUR
013200         MOVE WS-TEMP-MIN   TO WS-FORMATTED-MIN
013300         MOVE WS-TEMP-SEC   TO WS-FORMATTED-SEC
013400         MOVE WS-TEMP-MS    TO WS-FORMATTED-MS
013500         DISPLAY X"1B" & "[32mPROGRAM RAN AT "
013600         WS-FORMATTED-DATE-TIME X"1B" & "[0m"
013700         CALL "SYSTEM" USING BY CONTENT "/tmp/cobol-interactive"
013800         CALL "SYSTEM" USING BY CONTENT 
013900         "rm /tmp/cobol-interactive"
014000     END-IF.
014100     PERFORM 100-CHECK-FOR-CHANGES

014300     STOP RUN.
014400 300-CHECK-DEPENDENCIES.
014500     CALL "SYSTEM" USING BY CONTENT "which cobc"
014600         RETURNING RETURN-CODE.
014700     IF RETURN-CODE NOT EQUAL ZERO THEN
014800         DISPLAY X"1B" & "[31;1;4m Error: 'cobc' COBOL compiler "
014900         "not found" X"1B" & "[0m"
015000         STOP RUN
015100     END-IF.
015200     CALL "SYSTEM" USING BY CONTENT "which grep"
015300         RETURNING RETURN-CODE.
015400     IF RETURN-CODE NOT EQUAL ZERO THEN
015500         DISPLAY X"1B" & "[31;1;4mError: 'grep' utility not found"
015600         X"1B" & "[0m"
015700         STOP RUN
015800     END-IF.
015900     CALL "SYSTEM" USING BY CONTENT "which file"
016000         RETURNING RETURN-CODE.
016100     IF RETURN-CODE NOT EQUAL ZERO THEN
016200         DISPLAY X"1B" & "[31;1;4mError: 'file' utility not found"
016300         X"1B" & "[0m"
016400         STOP RUN
016500     END-IF.
016600     CALL "SYSTEM" USING BY CONTENT "which test"
016700         RETURNING RETURN-CODE.
016800     IF RETURN-CODE NOT EQUAL ZERO THEN
016900         DISPLAY X"1B" & "[31;1;4mError: 'test' utility not found"
017000         X"1B" & "[0m"
017100         STOP RUN
017200     END-IF.
017300     CALL "SYSTEM" USING BY CONTENT "which rm"
017400         RETURNING RETURN-CODE.
017500     IF RETURN-CODE NOT EQUAL ZERO THEN
017600         DISPLAY X"1B" & "[31;1;4mError: 'rm' utility not found"
017700         X"1B" & "[0m"
017800         STOP RUN
017900     END-IF.

018100 END PROGRAM INTERACTIVE-COBOL.

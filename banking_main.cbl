IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-TRANSACTION-PROCESSOR.
       DATE-WRITTEN. 2024-01-15.
       AUTHOR. Updated 2025-09-03 for DB2 Integration.
       REMARKS. DAILY BANKING TRANSACTION PROCESSING SYSTEM WITH DB2 INTEGRATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           * Configuration file containing DB2 connection parameters
           SELECT CONFIG-FILE 
               ASSIGN TO "DB2CONFIG"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONFIG-FILE-STATUS.
           
           * These files are still used for reporting and error logging
           SELECT DAILY-REPORT-FILE 
               ASSIGN TO "DAILYREPORT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REPORT-FILE-STATUS.
           
           SELECT ERROR-LOG-FILE 
               ASSIGN TO "ERRORLOG"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ERROR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       
       * DB2 configuration file definition
       COPY "config_file.cpy".
       
       * Daily Report File Record Structure (still file-based)
       FD  DAILY-REPORT-FILE.
       01  REPORT-RECORD.
           05  RP-TIMESTAMP.
               10  RP-DATE.
                   15  RP-YEAR        PIC 9(4).
                   15  RP-MONTH       PIC 9(2).
                   15  RP-DAY         PIC 9(2).
               10  RP-TIME.
                   15  RP-HOUR        PIC 9(2).
                   15  RP-MINUTE      PIC 9(2).
                   15  RP-SECOND      PIC 9(2).
                   15  RP-MILLISEC    PIC 9(2).
           05  RP-CUSTOMER-ID         PIC X(10).
           05  RP-TRANSACTION-TYPE    PIC X.
           05  RP-AMOUNT              PIC 9(7)V99.
           05  RP-STATUS              PIC X(10).
           05  RP-TRANSACTION-ID      PIC 9(10).
           05  RP-TARGET-ACCOUNT      PIC X(10).
       
       * Error Log File Record Structure (still file-based)
       FD  ERROR-LOG-FILE.
       01  ERROR-RECORD.
           05  ER-TIMESTAMP.
               10  ER-DATE.
                   15  ER-YEAR        PIC 9(4).
                   15  ER-MONTH       PIC 9(2).
                   15  ER-DAY         PIC 9(2).
               10  ER-TIME.
                   15  ER-HOUR        PIC 9(2).
                   15  ER-MINUTE      PIC 9(2).
                   15  ER-SECOND      PIC 9(2).
                   15  ER-MILLISEC    PIC 9(2).
           05  ER-ERROR-CODE          PIC 9(4).
           05  ER-CUSTOMER-ID         PIC X(10).
           05  ER-ERROR-MESSAGE       PIC X(80).
           
       WORKING-STORAGE SECTION.
       * Include SQL declarations
       COPY "db2declarations.cpy".
       
       * Transaction Record Structure (now filled from DB2)
       01  TRANSACTION-RECORD.
           05  TR-CUSTOMER-ID         PIC X(10).
           05  TR-TRANSACTION-TYPE    PIC X.
               88  DEPOSIT-TRANSACTION    VALUE 'D'.
               88  WITHDRAWAL-TRANSACTION VALUE 'W'.
               88  TRANSFER-TRANSACTION   VALUE 'T'.
               88  INQUIRY-TRANSACTION    VALUE 'I'.
               88  PAYMENT-TRANSACTION    VALUE 'P'.
           05  TR-AMOUNT              PIC 9(7)V99.
           05  TR-TARGET-ACCOUNT      PIC X(10).
           05  TR-REFERENCE-ID        PIC X(20).

       * Account Record Structure (now filled from DB2)
       01  ACCOUNT-RECORD.
           05  AM-CUSTOMER-ID         PIC X(10).
           05  AM-ACCOUNT-BALANCE     PIC 9(10)V99.
           05  AM-ACCOUNT-STATUS      PIC X(10).
               88  ACCOUNT-ACTIVE     VALUE 'ACTIVE'.
               88  ACCOUNT-SUSPENDED  VALUE 'SUSPENDED'.

       * Daily Report File Record Structure
       FD  DAILY-REPORT-FILE.
       01  REPORT-RECORD.
           05  RP-TIMESTAMP.
               10  RP-DATE.
                   15  RP-YEAR        PIC 9(4).
                   15  RP-MONTH       PIC 9(2).
                   15  RP-DAY         PIC 9(2).
               10  RP-TIME.
                   15  RP-HOUR        PIC 9(2).
                   15  RP-MINUTE      PIC 9(2).
                   15  RP-SECOND      PIC 9(2).
                   15  RP-MILLISEC    PIC 9(2).
           05  RP-CUSTOMER-ID         PIC X(10).
           05  RP-TRANSACTION-TYPE    PIC X.
           05  RP-AMOUNT              PIC 9(7)V99.
           05  RP-STATUS              PIC X(10).
           05  RP-TRANSACTION-ID      PIC 9(10).
           05  RP-TARGET-ACCOUNT      PIC X(10).

       * Error Log File Record Structure
       FD  ERROR-LOG-FILE.
       01  ERROR-RECORD.
           05  ER-TIMESTAMP.
               10  ER-DATE.
                   15  ER-YEAR        PIC 9(4).
                   15  ER-MONTH       PIC 9(2).
                   15  ER-DAY         PIC 9(2).
               10  ER-TIME.
                   15  ER-HOUR        PIC 9(2).
                   15  ER-MINUTE      PIC 9(2).
                   15  ER-SECOND      PIC 9(2).
                   15  ER-MILLISEC    PIC 9(2).
           05  ER-ERROR-CODE          PIC 9(4).
           05  ER-CUSTOMER-ID         PIC X(10).
           05  ER-ERROR-MESSAGE       PIC X(80).

       WORKING-STORAGE SECTION.
       * File Status Variables
       01  WS-FILE-STATUSES.
           05  WS-CONFIG-FILE-STATUS      PIC XX.
           05  WS-REPORT-FILE-STATUS      PIC XX.
           05  WS-ERROR-FILE-STATUS       PIC XX.
       
       * DB2 Configuration Information
       COPY "db2config.cpy".

       * Processing Statistics
       01  WS-PROCESSING-STATS.
           05  WS-TOTAL-TRANSACTIONS      PIC 9(5) COMP VALUE ZERO.
           05  WS-SUCCESSFUL-TRANSACTIONS PIC 9(5) COMP VALUE ZERO.
           05  WS-FAILED-TRANSACTIONS     PIC 9(5) COMP VALUE ZERO.
           
       * Error Code Management
       01  WS-ERROR-CODES.
           05  WS-ERROR-CODE              PIC 9(4).
               88  ERR-NONE               VALUE 0000.
               88  ERR-INVALID-CUSTOMER   VALUE 1001.
               88  ERR-INSUF-FUNDS        VALUE 1002.
               88  ERR-ACCOUNT-INACTIVE   VALUE 1003.
               88  ERR-INVALID-AMOUNT     VALUE 1004.
               88  ERR-MAX-DAILY-LIMIT    VALUE 1005.
               88  ERR-INVALID-TRANS-TYPE VALUE 1006.
               88  ERR-UPDATE-FAILED      VALUE 2001.
               88  ERR-FILE-ACCESS        VALUE 3001.
               88  ERR-SYSTEM-ERROR       VALUE 9999.

       * Work Areas and Flags
       01  WS-WORK-AREAS.
           05  WS-EOF-FLAG                PIC X VALUE 'N'.
               88  END-OF-FILE            VALUE 'Y'.
               88  NOT-END-OF-FILE        VALUE 'N'.
           
           05  WS-TRANSACTION-STATUS      PIC X.
               88  TRANSACTION-VALID      VALUE 'V'.
               88  TRANSACTION-INVALID    VALUE 'I'.
               
           05  WS-TRANSACTION-TRACKING.
               10  WS-TRANSACTION-ID      PIC 9(10) COMP VALUE 0.
               10  WS-TRANSACTION-PHASE   PIC X.
                   88  PHASE-VALIDATION   VALUE 'V'.
                   88  PHASE-PROCESSING   VALUE 'P'.
                   88  PHASE-COMMIT       VALUE 'C'.
                   88  PHASE-ROLLBACK     VALUE 'R'.

       * Transaction Rollback Support
       01  WS-ROLLBACK-AREA.
           05  WS-ORIGINAL-BALANCE        PIC 9(10)V99.
           05  WS-SOURCE-ACCOUNT          PIC X(10).
           05  WS-TARGET-ACCOUNT          PIC X(10).
           05  WS-NEED-ROLLBACK           PIC X VALUE 'N'.
               88  ROLLBACK-NEEDED        VALUE 'Y'.
               88  ROLLBACK-NOT-NEEDED    VALUE 'N'.

       * Date and Time Fields
       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-YEAR            PIC 9(4).
               10  WS-MONTH           PIC 9(2).
               10  WS-DAY             PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-HOUR            PIC 9(2).
               10  WS-MINUTE          PIC 9(2).
               10  WS-SECOND          PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCESSING-ROUTINE.
           PERFORM INITIALIZATION-ROUTINE
           PERFORM PROCESS-TRANSACTIONS 
               UNTIL END-OF-FILE
           PERFORM GENERATE-SUMMARY-REPORT
           PERFORM CLEANUP-ROUTINE
           STOP RUN.

       INITIALIZATION-ROUTINE.
           * Open report and error log files
           OPEN OUTPUT DAILY-REPORT-FILE
                OUTPUT ERROR-LOG-FILE

           * Validate file openings
           PERFORM CHECK-FILE-STATUSES

           * Get current system date and time
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           
           * Display initialization message
           DISPLAY "BANKING TRANSACTION PROCESSOR WITH DB2"
           DISPLAY "DATE: " WS-YEAR "-" WS-MONTH "-" WS-DAY
           DISPLAY "TIME: " WS-HOUR ":" WS-MINUTE ":" WS-SECOND
           
           * Load database configuration from external file
           PERFORM LOAD-DATABASE-CONFIG
           
           * Connect to DB2 database
           PERFORM DB-CONNECT
           
           * Prepare for transaction processing
           PERFORM DB-FETCH-PENDING-TRANSACTIONS.

       CHECK-FILE-STATUSES.
           * Comprehensive file status checking for reporting files
           IF WS-REPORT-FILE-STATUS NOT = "00"
              SET ERR-FILE-ACCESS TO TRUE
              PERFORM LOG-ERROR-WITH-CODE
                   WITH "ERROR OPENING REPORT FILE"
              PERFORM ABNORMAL-TERMINATION
           END-IF.
           
           IF WS-ERROR-FILE-STATUS NOT = "00"
              DISPLAY "ERROR OPENING ERROR LOG FILE"
              PERFORM ABNORMAL-TERMINATION
           END-IF.

       PROCESS-TRANSACTIONS.
           * Read next transaction from DB2
           PERFORM DB-READ-NEXT-TRANSACTION
           
           * If at end of transactions, exit
           IF END-OF-FILE
               GO TO EXIT-TRANSACTION-PROCESSING
           END-IF

           * Increment total transaction counter
           ADD 1 TO WS-TOTAL-TRANSACTIONS

           * Validate and process transaction
           PERFORM VALIDATE-TRANSACTION
           
           IF TRANSACTION-VALID
               PERFORM PROCESS-VALID-TRANSACTION
               * Update transaction status in DB2
               PERFORM DB-UPDATE-TRANSACTION-STATUS
           ELSE
               PERFORM PROCESS-INVALID-TRANSACTION
           END-IF.

       EXIT-TRANSACTION-PROCESSING.
           EXIT.

       GENERATE-SUMMARY-REPORT.
           * Display processing summary
           DISPLAY "TRANSACTION PROCESSING SUMMARY"
           DISPLAY "Total Transactions:     " WS-TOTAL-TRANSACTIONS
           DISPLAY "Successful Transactions:" WS-SUCCESSFUL-TRANSACTIONS
           DISPLAY "Failed Transactions:    " WS-FAILED-TRANSACTIONS.

       CLEANUP-ROUTINE.
           * Close database cursor
           PERFORM DB-CLOSE-TRANSACTION-CURSOR
           
           * Disconnect from DB2 database
           PERFORM DB-DISCONNECT
           
           * Close report and error files
           CLOSE DAILY-REPORT-FILE
                 ERROR-LOG-FILE
           
           DISPLAY "BANKING TRANSACTION PROCESSING COMPLETE".

       ABNORMAL-TERMINATION.
           * Handle critical errors
           DISPLAY "CRITICAL ERROR: SYSTEM TERMINATING"
           MOVE "SYSTEM TERMINATED DUE TO CRITICAL ERROR" 
             TO ER-ERROR-MESSAGE
           WRITE ERROR-RECORD
           STOP RUN.

       COPY "transaction_processing.cbl".

       IDENTIFICATION DIVISION.
       * Optional declarative section for additional error handling
       DECLARATIVES.
       FILE-ERROR-HANDLER SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON 
               TRANSACTION-FILE 
               ACCOUNT-MASTER-FILE 
               DAILY-REPORT-FILE 
               ERROR-LOG-FILE.
           
           DISPLAY "FILE PROCESSING ERROR DETECTED"
           PERFORM ABNORMAL-TERMINATION.
       END DECLARATIVES.

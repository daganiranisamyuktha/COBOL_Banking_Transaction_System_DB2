      *================================================================*
      * DB2 Database Operations Procedures                           *
      * Contains routines for DB2 database operations                *
      *================================================================*

       DB-CONNECT.
           * Connect to DB2 database using configuration parameters
           EXEC SQL
               CONNECT TO :DB-NAME
               USER :DB-USER
               USING :DB-PASSWORD
           END-EXEC
           
           * Check connection status
           IF SQLCODE = 0
               SET DB-CONNECTION-OK TO TRUE
               DISPLAY "Successfully connected to DB2 database"
           ELSE
               MOVE SQLCODE TO WS-ERROR-CODE
               STRING "DB Connection Error - SQLCODE: " SQLCODE
                   INTO ER-ERROR-MESSAGE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH ER-ERROR-MESSAGE
               PERFORM ABNORMAL-TERMINATION
           END-IF.
           
       DB-DISCONNECT.
           * Disconnect from DB2 database
           EXEC SQL
               DISCONNECT
           END-EXEC
           
           * Check disconnection status
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-ERROR-CODE
               STRING "DB Disconnect Error - SQLCODE: " SQLCODE
                   INTO ER-ERROR-MESSAGE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH ER-ERROR-MESSAGE
           END-IF.
           
       DB-FETCH-PENDING-TRANSACTIONS.
           * Initialize EOF flag
           SET NOT-END-OF-FILE TO TRUE
           
           * Declare cursor for pending transactions
           EXEC SQL
               DECLARE TRANSACTION_CURSOR CURSOR FOR
               SELECT CUSTOMER_ID, TRANSACTION_TYPE, AMOUNT,
                      TARGET_ACCOUNT, REFERENCE_ID,
                      CREATION_DATE, CREATION_TIME, STATUS
               FROM TRANSACTION_TABLE
               WHERE STATUS = 'PENDING'
               ORDER BY CREATION_DATE, CREATION_TIME
           END-EXEC
           
           * Open the cursor
           EXEC SQL
               OPEN TRANSACTION_CURSOR
           END-EXEC
           
           * Check open status
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-ERROR-CODE
               STRING "Error opening transaction cursor - SQLCODE: " SQLCODE
                   INTO ER-ERROR-MESSAGE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH ER-ERROR-MESSAGE
               SET END-OF-FILE TO TRUE
           END-IF.

       DB-READ-NEXT-TRANSACTION.
           * Fetch next pending transaction
           EXEC SQL
               FETCH TRANSACTION_CURSOR INTO
               :SQL-TR-CUSTOMER-ID,
               :SQL-TR-TRANSACTION-TYPE,
               :SQL-TR-AMOUNT,
               :SQL-TR-TARGET-ACCOUNT,
               :SQL-TR-REFERENCE-ID,
               :SQL-TR-CREATION-DATE,
               :SQL-TR-CREATION-TIME,
               :SQL-TR-STATUS
           END-EXEC
           
           * Check fetch status
           EVALUATE SQLCODE
               WHEN 0
                   * Successfully fetched a transaction
                   PERFORM COPY-SQL-TO-TRANSACTION-RECORD
                   SET TRANSACTION-VALID TO TRUE
                   
               WHEN 100
                   * No more transactions to process
                   SET END-OF-FILE TO TRUE
                   
               WHEN OTHER
                   * Error occurred during fetch
                   MOVE SQLCODE TO WS-ERROR-CODE
                   STRING "Error fetching transaction - SQLCODE: " SQLCODE
                       INTO ER-ERROR-MESSAGE
                   PERFORM LOG-ERROR-WITH-CODE
                       WITH ER-ERROR-MESSAGE
                   SET END-OF-FILE TO TRUE
           END-EVALUATE.
           
       COPY-SQL-TO-TRANSACTION-RECORD.
           * Copy SQL record to transaction record
           MOVE SQL-TR-CUSTOMER-ID TO TR-CUSTOMER-ID
           MOVE SQL-TR-TRANSACTION-TYPE TO TR-TRANSACTION-TYPE
           MOVE SQL-TR-AMOUNT TO TR-AMOUNT
           MOVE SQL-TR-TARGET-ACCOUNT TO TR-TARGET-ACCOUNT
           MOVE SQL-TR-REFERENCE-ID TO TR-REFERENCE-ID.

       DB-CLOSE-TRANSACTION-CURSOR.
           * Close transaction cursor
           EXEC SQL
               CLOSE TRANSACTION_CURSOR
           END-EXEC
           
           * Check close status
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-ERROR-CODE
               STRING "Error closing transaction cursor - SQLCODE: " SQLCODE
                   INTO ER-ERROR-MESSAGE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH ER-ERROR-MESSAGE
           END-IF.
           
       DB-GET-ACCOUNT-INFO.
           * Get account information by customer ID
           EXEC SQL
               SELECT ACCOUNT_BALANCE, ACCOUNT_STATUS
               INTO :SQL-AM-ACCOUNT-BALANCE, :SQL-AM-ACCOUNT-STATUS
               FROM ACCOUNT_TABLE
               WHERE CUSTOMER_ID = :TR-CUSTOMER-ID
           END-EXEC
           
           * Check account retrieval status
           EVALUATE SQLCODE
               WHEN 0
                   * Successfully retrieved account info
                   MOVE SQL-AM-ACCOUNT-BALANCE TO AM-ACCOUNT-BALANCE
                   MOVE SQL-AM-ACCOUNT-STATUS TO AM-ACCOUNT-STATUS
                   
               WHEN 100
                   * Account not found
                   SET TRANSACTION-INVALID TO TRUE
                   SET ERR-INVALID-CUSTOMER TO TRUE
                   PERFORM LOG-ERROR-WITH-CODE
                       WITH "INVALID CUSTOMER ID - ACCOUNT NOT FOUND"
                   
               WHEN OTHER
                   * Error occurred during retrieval
                   SET TRANSACTION-INVALID TO TRUE
                   MOVE SQLCODE TO WS-ERROR-CODE
                   STRING "Error retrieving account - SQLCODE: " SQLCODE
                       INTO ER-ERROR-MESSAGE
                   PERFORM LOG-ERROR-WITH-CODE
                       WITH ER-ERROR-MESSAGE
           END-EVALUATE.

       DB-UPDATE-ACCOUNT-BALANCE.
           * Update account balance
           EXEC SQL
               UPDATE ACCOUNT_TABLE
               SET ACCOUNT_BALANCE = :AM-ACCOUNT-BALANCE
               WHERE CUSTOMER_ID = :AM-CUSTOMER-ID
           END-EXEC
           
           * Check update status
           IF SQLCODE = 0
               * Successfully updated account balance
               CONTINUE
           ELSE
               * Error during update
               SET ROLLBACK-NEEDED TO TRUE
               MOVE SQLCODE TO WS-ERROR-CODE
               STRING "Error updating account balance - SQLCODE: " SQLCODE
                   INTO ER-ERROR-MESSAGE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH ER-ERROR-MESSAGE
           END-IF.
           
       DB-UPDATE-TRANSACTION-STATUS.
           * Update transaction status
           MOVE "PROCESSED" TO SQL-TR-STATUS
           
           EXEC SQL
               UPDATE TRANSACTION_TABLE
               SET STATUS = :SQL-TR-STATUS
               WHERE CUSTOMER_ID = :TR-CUSTOMER-ID
               AND TRANSACTION_TYPE = :TR-TRANSACTION-TYPE
               AND AMOUNT = :TR-AMOUNT
               AND CREATION_DATE = :SQL-TR-CREATION-DATE
               AND CREATION_TIME = :SQL-TR-CREATION-TIME
           END-EXEC
           
           * Check update status
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-ERROR-CODE
               STRING "Error updating transaction status - SQLCODE: " SQLCODE
                   INTO ER-ERROR-MESSAGE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH ER-ERROR-MESSAGE
           END-IF.

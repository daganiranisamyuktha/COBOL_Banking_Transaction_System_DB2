      *================================================================*
      * Transaction Processing Module                                 *
      * Contains routines for validating and processing transactions  *
      *================================================================*

       VALIDATE-TRANSACTION.
           * Reset transaction status and error code
           MOVE 'V' TO WS-TRANSACTION-STATUS
           SET ERR-NONE TO TRUE
           SET PHASE-VALIDATION TO TRUE
           ADD 1 TO WS-TRANSACTION-ID
           
           * Get current timestamp for logging
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS

           * Validate Customer ID exists - now using DB2
           MOVE TR-CUSTOMER-ID TO AM-CUSTOMER-ID
           PERFORM DB-GET-ACCOUNT-INFO
           
           * Validate account is active
           IF TRANSACTION-VALID AND NOT ACCOUNT-ACTIVE
               SET TRANSACTION-INVALID TO TRUE
               SET ERR-ACCOUNT-INACTIVE TO TRUE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH "ACCOUNT IS NOT ACTIVE"
           END-IF
           
           * Validate transaction type is supported
           IF TRANSACTION-VALID AND 
              NOT (DEPOSIT-TRANSACTION OR 
                   WITHDRAWAL-TRANSACTION OR
                   TRANSFER-TRANSACTION OR
                   INQUIRY-TRANSACTION OR
                   PAYMENT-TRANSACTION)
               SET TRANSACTION-INVALID TO TRUE
               SET ERR-INVALID-TRANS-TYPE TO TRUE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH "INVALID TRANSACTION TYPE"
           END-IF
           
           * Validate amount is positive for transactions requiring amounts
           IF TRANSACTION-VALID AND
              (DEPOSIT-TRANSACTION OR 
               WITHDRAWAL-TRANSACTION OR 
               TRANSFER-TRANSACTION OR
               PAYMENT-TRANSACTION) AND
              TR-AMOUNT <= ZERO
               SET TRANSACTION-INVALID TO TRUE
               SET ERR-INVALID-AMOUNT TO TRUE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH "INVALID TRANSACTION AMOUNT"
           END-IF
           
           * For Transfer, validate target account exists
           IF TRANSACTION-VALID AND TRANSFER-TRANSACTION
               MOVE TR-TARGET-ACCOUNT TO AM-CUSTOMER-ID
               READ ACCOUNT-MASTER-FILE
                   INVALID KEY 
                       SET TRANSACTION-INVALID TO TRUE
                       SET ERR-INVALID-CUSTOMER TO TRUE
                       PERFORM LOG-ERROR-WITH-CODE
                           WITH "INVALID TARGET ACCOUNT"
               END-READ
           END-IF
           
           * Store original account balance for potential rollback
           IF TRANSACTION-VALID
               MOVE TR-CUSTOMER-ID TO AM-CUSTOMER-ID
               READ ACCOUNT-MASTER-FILE
               MOVE AM-ACCOUNT-BALANCE TO WS-ORIGINAL-BALANCE
               MOVE TR-CUSTOMER-ID TO WS-SOURCE-ACCOUNT
               MOVE TR-TARGET-ACCOUNT TO WS-TARGET-ACCOUNT
               SET ROLLBACK-NOT-NEEDED TO TRUE
           END-IF.

       PROCESS-VALID-TRANSACTION.
           SET PHASE-PROCESSING TO TRUE
           
           * Handle different transaction types
           EVALUATE TRUE
               WHEN DEPOSIT-TRANSACTION
                   PERFORM PROCESS-DEPOSIT
               WHEN WITHDRAWAL-TRANSACTION
                   PERFORM PROCESS-WITHDRAWAL
               WHEN TRANSFER-TRANSACTION
                   PERFORM PROCESS-TRANSFER
               WHEN INQUIRY-TRANSACTION
                   PERFORM PROCESS-INQUIRY
               WHEN PAYMENT-TRANSACTION
                   PERFORM PROCESS-PAYMENT
           END-EVALUATE
           
           * Handle transaction commit or rollback
           IF ROLLBACK-NEEDED
               PERFORM ROLLBACK-TRANSACTION
           ELSE
               PERFORM COMMIT-TRANSACTION
           END-IF.
           
       COMMIT-TRANSACTION.
           SET PHASE-COMMIT TO TRUE
           * Update account in DB2
           PERFORM DB-UPDATE-ACCOUNT-BALANCE
           
           * If this was a successful commit, log it
           IF NOT ROLLBACK-NEEDED
               PERFORM LOG-TRANSACTION-SUCCESS
               
               * SQL Commit to make changes permanent
               EXEC SQL
                   COMMIT WORK
               END-EXEC
           END-IF.
           
       ROLLBACK-TRANSACTION.
           SET PHASE-ROLLBACK TO TRUE
           
           * Log the rollback attempt
           SET ERR-SYSTEM-ERROR TO TRUE
           PERFORM LOG-ERROR-WITH-CODE
               WITH "TRANSACTION ROLLBACK INITIATED"
           
           * Use DB2 ROLLBACK to undo all changes
           EXEC SQL
               ROLLBACK WORK
           END-EXEC
           
           * Check SQLCODE after rollback
           IF SQLCODE NOT = 0
               SET ERR-SYSTEM-ERROR TO TRUE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH "CRITICAL ERROR: DB2 ROLLBACK FAILED"
           END-IF
           
           * For transfers, may need to rollback target account changes too
           IF TRANSFER-TRANSACTION
               MOVE WS-TARGET-ACCOUNT TO AM-CUSTOMER-ID
               READ ACCOUNT-MASTER-FILE
                   INVALID KEY 
                       SET ERR-SYSTEM-ERROR TO TRUE
                       PERFORM LOG-ERROR-WITH-CODE
                           WITH "CRITICAL ERROR: ROLLBACK FAILED - CANNOT READ TARGET"
               END-READ
               
               * If we got this far with target, we need to subtract what was added
               SUBTRACT TR-AMOUNT FROM AM-ACCOUNT-BALANCE
               
               REWRITE ACCOUNT-RECORD
                   INVALID KEY 
                       SET ERR-SYSTEM-ERROR TO TRUE
                       PERFORM LOG-ERROR-WITH-CODE
                           WITH "CRITICAL ERROR: ROLLBACK FAILED - CANNOT REWRITE TARGET"
               END-REWRITE
           END-IF
           
           * Log the rollback
           PERFORM LOG-TRANSACTION-ROLLBACK.

       PROCESS-DEPOSIT.
           * Add deposit amount to account balance
           ADD TR-AMOUNT TO AM-ACCOUNT-BALANCE
           
           * Success path - don't need rollback
           SET ROLLBACK-NOT-NEEDED TO TRUE
           ADD 1 TO WS-SUCCESSFUL-TRANSACTIONS.

       PROCESS-WITHDRAWAL.
           * Check if sufficient balance exists
           IF TR-AMOUNT > AM-ACCOUNT-BALANCE
               SET ROLLBACK-NEEDED TO TRUE
               SET ERR-INSUF-FUNDS TO TRUE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH "INSUFFICIENT FUNDS FOR WITHDRAWAL"
               ADD 1 TO WS-FAILED-TRANSACTIONS
           ELSE
               * Process valid withdrawal
               SUBTRACT TR-AMOUNT FROM AM-ACCOUNT-BALANCE
               SET ROLLBACK-NOT-NEEDED TO TRUE
               ADD 1 TO WS-SUCCESSFUL-TRANSACTIONS
           END-IF.
           
       PROCESS-TRANSFER.
           * First check sufficient funds in source account
           IF TR-AMOUNT > AM-ACCOUNT-BALANCE
               SET ROLLBACK-NEEDED TO TRUE
               SET ERR-INSUF-FUNDS TO TRUE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH "INSUFFICIENT FUNDS FOR TRANSFER"
               ADD 1 TO WS-FAILED-TRANSACTIONS
           ELSE
               * Deduct from source account
               SUBTRACT TR-AMOUNT FROM AM-ACCOUNT-BALANCE
               
               * Save source account record
               REWRITE ACCOUNT-RECORD
                   INVALID KEY 
                       SET ROLLBACK-NEEDED TO TRUE
                       SET ERR-UPDATE-FAILED TO TRUE
                       PERFORM LOG-ERROR-WITH-CODE
                           WITH "TRANSFER FAILED - CANNOT UPDATE SOURCE ACCOUNT"
                       ADD 1 TO WS-FAILED-TRANSACTIONS
               END-REWRITE
               
               * If source account update successful, update target account
               IF NOT ROLLBACK-NEEDED
                   * Read target account
                   MOVE TR-TARGET-ACCOUNT TO AM-CUSTOMER-ID
                   READ ACCOUNT-MASTER-FILE
                       INVALID KEY 
                           SET ROLLBACK-NEEDED TO TRUE
                           SET ERR-INVALID-CUSTOMER TO TRUE
                           PERFORM LOG-ERROR-WITH-CODE
                               WITH "TRANSFER FAILED - TARGET ACCOUNT NOT FOUND"
                           ADD 1 TO WS-FAILED-TRANSACTIONS
                   END-READ
                   
                   * If target found, add the amount
                   IF NOT ROLLBACK-NEEDED
                       ADD TR-AMOUNT TO AM-ACCOUNT-BALANCE
                       
                       * Save target account
                       REWRITE ACCOUNT-RECORD
                           INVALID KEY 
                               SET ROLLBACK-NEEDED TO TRUE
                               SET ERR-UPDATE-FAILED TO TRUE
                               PERFORM LOG-ERROR-WITH-CODE
                                   WITH "TRANSFER FAILED - CANNOT UPDATE TARGET ACCOUNT"
                               ADD 1 TO WS-FAILED-TRANSACTIONS
                       END-REWRITE
                       
                       * If successful, update counters
                       IF NOT ROLLBACK-NEEDED
                           ADD 1 TO WS-SUCCESSFUL-TRANSACTIONS
                       END-IF
                   END-IF
               END-IF
           END-IF.
           
       PROCESS-INQUIRY.
           * No balance changes for inquiry
           * Just log the inquiry transaction
           PERFORM LOG-INQUIRY-TRANSACTION
           SET ROLLBACK-NOT-NEEDED TO TRUE
           ADD 1 TO WS-SUCCESSFUL-TRANSACTIONS.
           
       PROCESS-PAYMENT.
           * Similar to withdrawal, but with specific payment handling
           IF TR-AMOUNT > AM-ACCOUNT-BALANCE
               SET ROLLBACK-NEEDED TO TRUE
               SET ERR-INSUF-FUNDS TO TRUE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH "INSUFFICIENT FUNDS FOR PAYMENT"
               ADD 1 TO WS-FAILED-TRANSACTIONS
           ELSE
               * Process valid payment
               SUBTRACT TR-AMOUNT FROM AM-ACCOUNT-BALANCE
               SET ROLLBACK-NOT-NEEDED TO TRUE
               ADD 1 TO WS-SUCCESSFUL-TRANSACTIONS
           END-IF.

       PROCESS-INVALID-TRANSACTION.
           * Log invalid transaction to error log
           IF WS-ERROR-CODE = ZERO
               SET ERR-INVALID-TRANS-TYPE TO TRUE
               PERFORM LOG-ERROR-WITH-CODE
                   WITH "INVALID TRANSACTION DETECTED"
           END-IF
           
           * Increment failed transaction counter
           ADD 1 TO WS-FAILED-TRANSACTIONS.
           
       LOG-ERROR-WITH-CODE.
           ENTRY USING ERROR-MESSAGE-PARAM.
           
           * Copy current timestamp to error record
           MOVE WS-CURRENT-DATE-FIELDS TO ER-TIMESTAMP
           
           * Add error code and message
           MOVE WS-ERROR-CODE TO ER-ERROR-CODE
           MOVE TR-CUSTOMER-ID TO ER-CUSTOMER-ID
           MOVE ERROR-MESSAGE-PARAM TO ER-ERROR-MESSAGE
           
           * Write to error log
           WRITE ERROR-RECORD.
           
       LOG-TRANSACTION-SUCCESS.
           * Log successful transaction to report file with timestamp
           MOVE WS-CURRENT-DATE-FIELDS TO RP-TIMESTAMP
           MOVE TR-CUSTOMER-ID TO RP-CUSTOMER-ID
           MOVE TR-TRANSACTION-TYPE TO RP-TRANSACTION-TYPE
           MOVE TR-AMOUNT TO RP-AMOUNT
           MOVE "SUCCESS" TO RP-STATUS
           MOVE WS-TRANSACTION-ID TO RP-TRANSACTION-ID
           MOVE TR-TARGET-ACCOUNT TO RP-TARGET-ACCOUNT
           WRITE REPORT-RECORD.
           
       LOG-TRANSACTION-ROLLBACK.
           * Log rollback transaction to report file with timestamp
           MOVE WS-CURRENT-DATE-FIELDS TO RP-TIMESTAMP
           MOVE TR-CUSTOMER-ID TO RP-CUSTOMER-ID
           MOVE TR-TRANSACTION-TYPE TO RP-TRANSACTION-TYPE
           MOVE TR-AMOUNT TO RP-AMOUNT
           MOVE "ROLLBACK" TO RP-STATUS
           MOVE WS-TRANSACTION-ID TO RP-TRANSACTION-ID
           MOVE TR-TARGET-ACCOUNT TO RP-TARGET-ACCOUNT
           WRITE REPORT-RECORD.
           
       LOG-INQUIRY-TRANSACTION.
           * Log inquiry transaction to report file with timestamp
           MOVE WS-CURRENT-DATE-FIELDS TO RP-TIMESTAMP
           MOVE TR-CUSTOMER-ID TO RP-CUSTOMER-ID
           MOVE TR-TRANSACTION-TYPE TO RP-TRANSACTION-TYPE
           MOVE ZERO TO RP-AMOUNT
           MOVE "INQUIRY" TO RP-STATUS
           MOVE WS-TRANSACTION-ID TO RP-TRANSACTION-ID
           MOVE SPACES TO RP-TARGET-ACCOUNT
           WRITE REPORT-RECORD.

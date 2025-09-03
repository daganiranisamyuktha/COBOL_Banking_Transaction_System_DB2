      *================================================================*
      * DB2 SQL Communication Area                                   *
      * SQLCA is used for DB2 database communication                 *
      *================================================================*
           EXEC SQL INCLUDE SQLCA END-EXEC.

      *================================================================*
      * SQL Declarations for Transaction data                          *
      *================================================================*
           EXEC SQL DECLARE TRANSACTION_TABLE TABLE
              (CUSTOMER_ID       CHAR(10) NOT NULL,
               TRANSACTION_TYPE  CHAR(1) NOT NULL,
               AMOUNT            DECIMAL(9,2) NOT NULL,
               TARGET_ACCOUNT    CHAR(10),
               REFERENCE_ID      CHAR(20),
               CREATION_DATE     DATE NOT NULL,
               CREATION_TIME     TIME NOT NULL,
               STATUS            CHAR(10) DEFAULT 'PENDING')
           END-EXEC.

      *================================================================*
      * SQL Declarations for Account data                              *
      *================================================================*
           EXEC SQL DECLARE ACCOUNT_TABLE TABLE
              (CUSTOMER_ID       CHAR(10) NOT NULL PRIMARY KEY,
               ACCOUNT_BALANCE   DECIMAL(12,2) NOT NULL,
               ACCOUNT_STATUS    CHAR(10) NOT NULL)
           END-EXEC.

      *================================================================*
      * SQL Host Variables for Record Retrieval                        *
      *================================================================*
       01  SQL-TRANSACTION-RECORD.
           05  SQL-TR-CUSTOMER-ID         PIC X(10).
           05  SQL-TR-TRANSACTION-TYPE    PIC X.
           05  SQL-TR-AMOUNT              PIC S9(7)V99.
           05  SQL-TR-TARGET-ACCOUNT      PIC X(10).
           05  SQL-TR-REFERENCE-ID        PIC X(20).
           05  SQL-TR-CREATION-DATE       PIC X(10).
           05  SQL-TR-CREATION-TIME       PIC X(8).
           05  SQL-TR-STATUS              PIC X(10).

       01  SQL-ACCOUNT-RECORD.
           05  SQL-AM-CUSTOMER-ID         PIC X(10).
           05  SQL-AM-ACCOUNT-BALANCE     PIC S9(10)V99.
           05  SQL-AM-ACCOUNT-STATUS      PIC X(10).

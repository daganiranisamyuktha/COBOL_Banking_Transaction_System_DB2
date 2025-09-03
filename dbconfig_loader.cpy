      *================================================================*
      * Database Configuration Loader                                *
      * This copybook contains routines to load database config      *
      *================================================================*
       
       LOAD-DATABASE-CONFIG.
           * This routine reads configuration from external file
           OPEN INPUT CONFIG-FILE
           
           READ CONFIG-FILE
               AT END 
                   MOVE "ERROR: Empty configuration file" TO ER-ERROR-MESSAGE
                   WRITE ERROR-RECORD
                   PERFORM ABNORMAL-TERMINATION
           END-READ
           
           * Parse configuration settings from the file
           MOVE CF-SERVER-NAME TO DB-SERVER-NAME
           MOVE CF-PORT TO DB-PORT
           MOVE CF-DATABASE-NAME TO DB-NAME
           MOVE CF-USERNAME TO DB-USER
           MOVE CF-PASSWORD TO DB-PASSWORD
           MOVE CF-SCHEMA TO DB-SCHEMA
           
           CLOSE CONFIG-FILE
           
           * Display connection information (without password)
           DISPLAY "DB2 CONNECTION PARAMETERS LOADED"
           DISPLAY "Server: " DB-SERVER-NAME
           DISPLAY "Port: " DB-PORT
           DISPLAY "Database: " DB-NAME
           DISPLAY "User: " DB-USER
           DISPLAY "Schema: " DB-SCHEMA.

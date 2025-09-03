      *================================================================*
      * Environment Variable Based Configuration Loader                *
      * Alternative approach using environment variables               *
      *================================================================*
       
       LOAD-DATABASE-CONFIG-FROM-ENV.
           * This routine reads configuration from environment variables
           
           * Get DB2 server from environment variable
           CALL "C$GETENV" USING "DB2_SERVER", DB-SERVER-NAME
           
           * Get DB2 port from environment variable
           CALL "C$GETENV" USING "DB2_PORT", WS-PORT-STRING
           IF WS-PORT-STRING NOT = SPACES
               MOVE FUNCTION NUMVAL(WS-PORT-STRING) TO DB-PORT
           END-IF
           
           * Get DB2 database name from environment variable
           CALL "C$GETENV" USING "DB2_DATABASE", DB-NAME
           
           * Get DB2 credentials from environment variables
           CALL "C$GETENV" USING "DB2_USER", DB-USER
           CALL "C$GETENV" USING "DB2_PASSWORD", DB-PASSWORD
           
           * Get DB2 schema from environment variable
           CALL "C$GETENV" USING "DB2_SCHEMA", DB-SCHEMA
           
           * Check if required config is available
           IF DB-SERVER-NAME = SPACES OR 
              DB-NAME = SPACES OR 
              DB-USER = SPACES
               DISPLAY "ERROR: Required DB2 environment variables not set"
               DISPLAY "Please set DB2_SERVER, DB2_DATABASE, and DB2_USER"
               PERFORM ABNORMAL-TERMINATION
           END-IF
           
           * Display connection information (without password)
           DISPLAY "DB2 CONNECTION PARAMETERS LOADED FROM ENVIRONMENT"
           DISPLAY "Server: " DB-SERVER-NAME
           DISPLAY "Port: " DB-PORT
           DISPLAY "Database: " DB-NAME
           DISPLAY "User: " DB-USER
           DISPLAY "Schema: " DB-SCHEMA.

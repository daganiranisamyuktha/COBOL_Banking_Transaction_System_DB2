      *================================================================*
      * DB2 Connection Configuration Parameters                       *
      * This copybook contains DB2 connection information             *
      *================================================================*
       01  DB-CONFIG.
           05  DB-SERVER-NAME          PIC X(30) VALUE SPACES.
           05  DB-PORT                 PIC 9(5) VALUE ZEROS.
           05  DB-NAME                 PIC X(30) VALUE SPACES.
           05  DB-USER                 PIC X(30) VALUE SPACES.
           05  DB-PASSWORD             PIC X(30) VALUE SPACES.
           05  DB-SCHEMA               PIC X(30) VALUE SPACES.
           05  DB-CONNECTION-STATUS    PIC XX VALUE SPACES.
               88  DB-CONNECTION-OK    VALUE "00".

      *================================================================*
      * DB2 Configuration File Definition                            *
      *================================================================*
       FD  CONFIG-FILE.
       01  CONFIG-RECORD.
           05  CF-KEYWORD              PIC X(20).
           05  CF-VALUE                PIC X(60).

       WORKING-STORAGE SECTION.
       01  CONFIG-ITEMS.
           05  CF-SERVER-NAME          PIC X(30).
           05  CF-PORT                 PIC 9(5).
           05  CF-DATABASE-NAME        PIC X(30).
           05  CF-USERNAME             PIC X(30).
           05  CF-PASSWORD             PIC X(30).
           05  CF-SCHEMA               PIC X(30).

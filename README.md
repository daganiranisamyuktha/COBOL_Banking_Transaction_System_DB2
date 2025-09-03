# COBOL Banking Transaction System with DB2 Integration

This repository contains a COBOL-based banking transaction processing system that demonstrates how to handle financial transactions against customer accounts with DB2 database integration.

## System Components

The system consists of these main components:

1. **banking_main.cbl**: The main program that drives the transaction processing workflow
2. **transaction_processing.cbl**: A module containing transaction-specific business logic
3. **db2declarations.cpy**: Contains SQL declarations for DB2 database interaction
4. **db2config.cpy**: DB2 connection parameter structure
5. **dbconfig_loader.cpy**: Routines to load external DB2 configuration
6. **DB2CONFIG.template**: Template for the required configuration file (actual config file not included for security)

## Configuration Setup

**IMPORTANT**: Before running this application, you must set up your DB2 configuration:

1. Copy the `DB2CONFIG.template` file to create a new file named `DB2CONFIG`
   ```
   copy DB2CONFIG.template DB2CONFIG
   ```

2. Edit the `DB2CONFIG` file to add your actual DB2 connection parameters:
   ```
   SERVER       your-actual-db2-server.example.com
   PORT         50000
   DATABASE     YOUR_ACTUAL_DB
   USER         your_username
   PASSWORD     your_password
   SCHEMA       your_schema
   ```

3. Ensure the `DB2CONFIG` file is in the same directory as the executable
   
4. **Security Note**: Never commit your actual `DB2CONFIG` file with real credentials to version control

## Program Structure

### Banking Main Program (banking_main.cbl)

This is the main driver program that:

- Sets up the processing environment
- Loads database configuration from external file
- Connects to DB2 database
- Reads transactions from DB2 tables
- Validates customer accounts
- Updates account balances
- Generates reports and logs
- Provides error handling

Key components:
- **IDENTIFICATION DIVISION**: Defines the program as BANKING-TRANSACTION-PROCESSOR
- **ENVIRONMENT DIVISION**: Configures file associations and database connections
- **DATA DIVISION**: Defines data structures and includes SQL declarations
- **PROCEDURE DIVISION**: Contains the program logic with DB2 integration

The main processing flow consists of:
1. Initialization and DB connection
2. Transaction processing loop using DB2
3. Summary report generation
4. Database disconnect and cleanup

### Transaction Processing Module (transaction_processing.cbl)

This module contains the core business logic for processing different types of banking transactions:

- **VALIDATE-TRANSACTION**: Verifies customer ID exists in the DB2 account table
- **PROCESS-VALID-TRANSACTION**: Routes transactions to appropriate handlers
- **PROCESS-DEPOSIT**: Handles deposit transactions
- **PROCESS-WITHDRAWAL**: Handles withdrawal transactions with funds verification
- **PROCESS-TRANSFER**: Handles transfers between accounts
- **PROCESS-INQUIRY**: Handles account inquiries
- **PROCESS-PAYMENT**: Handles bill payment transactions
- **ROLLBACK-TRANSACTION**: Provides DB2 transaction rollback capabilities
- **COMMIT-TRANSACTION**: Commits DB2 transactions when successful

## Database Integration

### DB2 Tables

1. **TRANSACTION_TABLE**:
   - Contains pending and processed transactions
   - Fields:
     - CUSTOMER_ID (CHAR 10)
     - TRANSACTION_TYPE (CHAR 1): 'D' for deposit, 'W' for withdrawal, etc.
     - AMOUNT (DECIMAL 9,2)
     - TARGET_ACCOUNT (CHAR 10) - For transfers
     - REFERENCE_ID (CHAR 20)
     - CREATION_DATE (DATE)
     - CREATION_TIME (TIME)
     - STATUS (CHAR 10): 'PENDING', 'PROCESSED', etc.

2. **ACCOUNT_TABLE**:
   - Stores customer account information
   - Fields:
     - CUSTOMER_ID (CHAR 10) - Primary key
     - ACCOUNT_BALANCE (DECIMAL 12,2)
     - ACCOUNT_STATUS (CHAR 10): 'ACTIVE' or 'SUSPENDED'

### Output Files (Still File-Based)

1. **DAILYREPORT** (Daily Report File):
   - Line sequential organization
   - Records all processed transactions with status and timestamps
   - Enhanced with transaction IDs and target account information

2. **ERRORLOG** (Error Log File):
   - Line sequential organization
   - Contains detailed error messages with timestamps and error codes
   - Includes customer ID and transaction information

### External Configuration

1. **DB2CONFIG** File:
   - Contains connection parameters for DB2
   - Format: KEYWORD VALUE
   - Parameters include:
     - SERVER (hostname or IP)
     - PORT (typically 50000 for DB2)
     - DATABASE (name of DB2 database)
     - USER (DB2 username)
     - PASSWORD (DB2 password)
     - SCHEMA (DB2 schema)

## Processing Logic

### Database Connection
- External configuration loaded at startup
- DB2 connection established using parameters
- Connection errors trigger abnormal termination

### Transaction Processing
- Transactions read from DB2 TRANSACTION_TABLE
- Account information retrieved from DB2 ACCOUNT_TABLE
- Processed transactions marked as "PROCESSED" in DB2

### Transaction Types
- **Deposits**: Add funds to account
- **Withdrawals**: Remove funds from account with balance check
- **Transfers**: Move funds between accounts
- **Inquiries**: Check account information
- **Payments**: Process bill payments

### Error Handling and Recovery
- Granular error codes for specific error conditions
- DB2 transaction rollback support for failed transactions
- Enhanced logging with timestamps and transaction IDs
- SQLCODE values captured and logged for DB2 errors

### Compile and Deployment Requirements
- DB2 precompiler required for processing EXEC SQL statements
- DB2 client libraries for runtime
- Proper DB2 permissions for the configured user account

## Usage

To use this system:

1. Prepare the ACCTMASTER indexed file with customer account data
2. Create a TRANSIN file with transaction records
3. Compile the programs:
   ```
   cobc -x banking_main.cbl
   ```
4. Run the main program:
   ```
   ./banking_main
   ```
5. Check DAILYREPORT for transaction results and ERRORLOG for any errors

## Example Transaction Records

```
1000000001D0001000.00  (Customer 1000000001, Deposit, $1,000.00)
1000000002W0000500.50  (Customer 1000000002, Withdrawal, $500.50)
```

## Program Features

- Transaction type determination using 88-level condition names
- Account status checking (active vs. suspended)
- Comprehensive error handling
- Transaction statistics tracking
- Proper file handling and status checking
- Modular program design with separate transaction processing logic

## Technical Notes

- Uses COBOL indexed files for account data
- Implements dynamic access for account lookups
- Employs condition names (88-level) for readable code
- Utilizes COPY statement for modular code organization
- Implements proper error handling with DECLARATIVES

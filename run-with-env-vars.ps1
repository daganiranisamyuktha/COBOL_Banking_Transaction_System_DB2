# Sample launch script for setting DB2 connection environment variables
# IMPORTANT: Customize with your own values before using!

# Set DB2 connection parameters as environment variables
$env:DB2_SERVER = "your-db2-server.example.com"
$env:DB2_PORT = "50000"
$env:DB2_DATABASE = "YOUR_DATABASE_NAME"
$env:DB2_USER = "your_username"
$env:DB2_PASSWORD = "your_password"
$env:DB2_SCHEMA = "your_schema"

# Run the COBOL program
./banking_main

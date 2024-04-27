       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-TRANSACTION-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT ACCOUNT-FILE
                ASSIGN TO "../ACCOUNT-FILE.txt"
                FILE STATUS  MYFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS ACCOUNT-RECORD.
       01 ACCOUNT-RECORD.
           05 ACCOUNT-NUMBER-FILE PIC 9(6).
           05 BALANCE-FILE PIC S9(7)V99 VALUE 0.

       WORKING-STORAGE SECTION.
       01 ACCOUNT-NUMBER PIC 9(6).
       01 BALANCE PIC S9(7)V99 VALUE 0.
       01 TRANSACTION-TYPE PIC X(1).
       01 TRANSACTION-AMOUNT PIC S9(7)V99 VALUE 0.
       01 WS-ACCOUNT-FOUND PIC X VALUE 'N'.
       01 EOF PIC X VALUE 'N'.


       01 MYFILE-STATUS   PIC X(2).
           88 MYFILE-ALREADY-OPEN   VALUE '41'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN I-O ACCOUNT-FILE.
           IF MYFILE-STATUS <> '00'
               DISPLAY "Error opening file. Exiting program."
               CLOSE ACCOUNT-FILE
               STOP RUN
           END-IF.

           DISPLAY "Welcome to the Bank Transaction System".
           PERFORM UNTIL TRANSACTION-TYPE = 'X'
               DISPLAY "Enter your account number: "
               ACCEPT ACCOUNT-NUMBER
               IF ACCOUNT-NUMBER = 000000
                   DISPLAY "Invalid account number. Please try again."
               ELSE
                   PERFORM CHECK-ACCOUNT
               END-IF
           END-PERFORM.

       CHECK-ACCOUNT.
           OPEN INPUT ACCOUNT-FILE
           MOVE 'N' TO WS-ACCOUNT-FOUND
           MOVE 'N' TO EOF
           PERFORM UNTIL WS-ACCOUNT-FOUND = 'Y' OR EOF ='Y'
               READ ACCOUNT-FILE INTO ACCOUNT-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF ACCOUNT-NUMBER = ACCOUNT-NUMBER-FILE
                           MOVE 'Y' TO WS-ACCOUNT-FOUND
                           MOVE BALANCE-FILE TO BALANCE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE
           IF WS-ACCOUNT-FOUND = 'N'
               PERFORM CREATE-ACCOUNT
           END-IF
           PERFORM TRANSACTION-OPERATION.

       CREATE-ACCOUNT.
           OPEN EXTEND ACCOUNT-FILE
           MOVE ACCOUNT-NUMBER TO ACCOUNT-NUMBER-FILE
           MOVE 0 TO BALANCE-FILE
           WRITE ACCOUNT-RECORD
           CLOSE ACCOUNT-FILE.


       TRANSACTION-OPERATION.
           DISPLAY "Choose transaction type: ".
           DISPLAY "D - Deposit, W - Withdrawal, B - Check Balance,"
           "X - EXIT".
           ACCEPT TRANSACTION-TYPE.
           IF TRANSACTION-TYPE = 'D'
               PERFORM DEPOSIT
           ELSE IF TRANSACTION-TYPE = 'W'
               PERFORM WITHDRAWAL
           ELSE IF TRANSACTION-TYPE = 'B'
               PERFORM CHECK-BALANCE
           ELSE IF TRANSACTION-TYPE = 'X'
               DISPLAY "Thank you for using the Bank Transaction System"
               STOP RUN
           ELSE
               DISPLAY "Invalid transaction type. Please try again."
           END-IF.

       DEPOSIT.
           DISPLAY "Enter deposit amount: ".
           ACCEPT TRANSACTION-AMOUNT.
           PERFORM CHECK-BALANCE.
           ADD TRANSACTION-AMOUNT TO BALANCE GIVING BALANCE.
           PERFORM UPDATE-BALANCE.
           DISPLAY "Deposit successful.".
           PERFORM CHECK-BALANCE.

       WITHDRAWAL.
           DISPLAY "Enter withdrawal amount: ".
           ACCEPT TRANSACTION-AMOUNT.
           PERFORM CHECK-BALANCE;
           IF TRANSACTION-AMOUNT > BALANCE
               DISPLAY "Insufficient funds. Transaction cancelled."
           ELSE
               SUBTRACT TRANSACTION-AMOUNT FROM BALANCE GIVING BALANCE.
               DISPLAY "Withdrawal successful."
           PERFORM UPDATE-BALANCE.
           PERFORM CHECK-BALANCE.

       CHECK-BALANCE.
           OPEN INPUT ACCOUNT-FILE
           MOVE 'N' TO WS-ACCOUNT-FOUND
           PERFORM UNTIL WS-ACCOUNT-FOUND = 'Y' OR EOF = 'Y'
           READ ACCOUNT-FILE INTO ACCOUNT-RECORD
           AT END
               MOVE 'Y' TO EOF
           NOT AT END
               IF ACCOUNT-NUMBER = ACCOUNT-NUMBER-FILE
                   MOVE 'Y' TO WS-ACCOUNT-FOUND
                   MOVE BALANCE-FILE TO BALANCE
               END-IF
           END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE
           IF WS-ACCOUNT-FOUND = 'N'
               DISPLAY "Account not found."
           ELSE
               DISPLAY "Your balance is: " BALANCE
           END-IF.

       UPDATE-BALANCE.
           OPEN I-O ACCOUNT-FILE.
           PERFORM UNTIL EOF = 'Y'
               READ ACCOUNT-FILE INTO ACCOUNT-RECORD
               AT END
                   MOVE 'Y' TO EOF
               NOT AT END
                   IF ACCOUNT-NUMBER-FILE = ACCOUNT-NUMBER
                       MOVE BALANCE TO BALANCE-FILE
                       REWRITE ACCOUNT-RECORD
                   MOVE 'Y' TO WS-ACCOUNT-FOUND
                       EXIT PERFORM
               END-IF
               END-READ
           END-PERFORM.
           CLOSE ACCOUNT-FILE.

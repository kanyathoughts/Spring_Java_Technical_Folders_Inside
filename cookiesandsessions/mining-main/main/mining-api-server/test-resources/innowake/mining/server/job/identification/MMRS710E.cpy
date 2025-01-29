       01  MMRS-COMMON-TEXT-FIELDS.
           05  MMRS-ASK-FOR-LOGIN          PIC X(79) VALUE
           'Please enter login credentials '.
           05  MMRS-FEEDBACK-OK            PIC X(79) VALUE
           'Login OK'.
           05  MMRS-FEEDBACK-NOK           PIC X(79) VALUE
           'Login not successful'.
           05  MMRS-FEEDBACK-END           PIC X(79) VALUE
           'Application ended'.
           05  MMRS-LOGIN-USER-NOK         PIC X(79) VALUE
           'invalid user id '.
           05  MMRS-LOGIN-PASS-NOK         PIC X(79) VALUE
           'invalid password '.

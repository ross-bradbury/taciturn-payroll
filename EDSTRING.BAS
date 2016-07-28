'EDSTRING.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  EDSTRING.BI contains necessary declarations for insertion

DEFINT A-Z
DECLARE FUNCTION Valid% (Entry AS STRING)
DECLARE FUNCTION GetKey$ (Flash)
DECLARE FUNCTION EditString$ (InString$, Length%, NextField%)

CONST FALSE = 0
CONST TRUE = NOT FALSE

' Define names similar to keyboard names with their equivalent key codes.
CONST LEFT_75 = 75, ESC = 27, SPACE_32 = 32
CONST DOWN = 80, DEL = 83, ENTER = 13, UP = 72
CONST HOME = 71, NULL = 0, RIGHT_77 = 77, PgUp = 73
CONST ENDK = 79, INS = 82, TABKEY = 9, PgDn = 81
CONST CTRLD = 4, CTRLG = 7, CTRLS = 19, CTRLV = 22
CONST CTRLE = 5, CTRLH = 8, CTRLX = 24, SHIFTTAB = 15
CONST F1 = 59, F2 = 60, F3 = 61, F4 = 62, F5 = 63, F6 = 64
CONST F7 = 65, F8 = 66, F9 = 67, F10 = 68, F11 = 133, F12 = 134

'$INC_LUDE: 'INITIALS.BI'

' Definitions of routines in other modules
DECLARE SUB Click ()

'EditString___________________________________________________________FUNCTION
'  Creates or modifies a string of specified length with standard editing
'  commands. LEFT_75, RIGHT_77, BKSP, DEL, INS, ESC, ENTER, TAB, SHIFTTAB, UP, DOWN
'  are all valid editing commands along with their CTRL key equivalents.
'
'USES
'  SUB Click
'  FUNCTION GetKey
'  FUNCTION Valid
'  SHARED Pref AS INITIALS
'  CONST TRUE, FALSE
'  CONST LEFT_75 = 75, ESC = 27, SPACE_32 = 32
'  CONST DOWN = 80, DEL = 83, ENTER = 13, UP = 72
'  CONST HOME = 71, NULL = 0, RIGHT_77 = 77, PgUp = 73
'  CONST ENDK = 79, INS = 82, TABKEY = 9, PgDn = 81
'  CONST CTRLD = 4, CTRLG = 7, CTRLS = 19, CTRLV = 22
'  CONST CTRLD = 4, CTRLG = 7, CTRLS = 19, CTRLV = 22
'
'PARAMETERS
'  InString$ : String to modify, can be NULL to create new          -> STRING
'  Length    : Maximum length of the string                         -> INTEGER
'  NextField : Determines if there is more than one field           -> INTEGER
'
'OPTIONS ([NextField])
'  ENTERS
'    TRUE (-1): Multiple fields exist
'    FALSE (0): Only one field
'  EXITS
'          (1): Edit previous field
'    FALSE (0): Stop editing and accept record
'    TRUE (-1): Edit next field
'
'OUTPUT
'  SCREEN : Displays string as edited and "_" for the remainder of field
'
'VARIABLES
'  Choice$   : Key-press                                            -> INTEGER
'  Column    : Column of screen that cursor was on when called      -> INTEGER
'  FirstTime : Determines whether this is the first key-stroke      -> INTEGER
'  Insert    : TRUE if INSERT on, FALSE if off                STATIC-> INTEGER
'  MaxP      : Last used position in string                         -> INTEGER
'  P         : Current position in string                           -> INTEGER
'  Row       : Row of screen that cursor was on when called         -> INTEGER
'  Work$     : Name of string being modified durind function call   -> INTEGER
'
'LABELS
'  StartEditString
'
FUNCTION EditString$ (InString$, Length, NextField)
''''  SHARED Pref AS INITIALS
  STATIC Insert

  IF Pref.Click THEN CALL Click

  'Initialize and underline maximum length of string
  Row = CSRLIN: Column = POS(0)

StartEditString:

  IF Valid(InString$) THEN
    Work$ = RTRIM$(InString$)
  ELSE
    Work$ = ""
  END IF

  FirstTime = TRUE
  P = LEN(Work$): MaxP = P
  PRINT STRING$(Length, "_");

  'Since Insert is STATIC, its value is maintained from one call to the
  'next. Insert is 0 (FALSE) the first time the function is called.
  IF Insert THEN
    LOCATE Row, Column, 1, 7, 7
  ELSE
    LOCATE Row, Column, 1, 4, 7
  END IF

  COLOR Pref.HiFore, Pref.HiBack
  PRINT Work$;
  IF Column + P < 80 THEN
    LOCATE , Column + P
  ELSE
    LOCATE , 80
  END IF

  ' Process keys until either TAB or ENTER is pressed.
  DO

    'Get a key -- either a one-byte ASCII code or a two-byte extended code
    Choice$ = GetKey(TRUE)

    ' Translate two-byte extended codes to the one meaningful byte.
    IF LEN(Choice$) = 2 THEN
      Choice$ = RIGHT$(Choice$, 1)
      SELECT CASE Choice$
        ' Translate extended codes to ASCII control codes.
        CASE CHR$(LEFT_75)
          Choice$ = CHR$(CTRLS)
        CASE CHR$(RIGHT_77)
          Choice$ = CHR$(CTRLD)
        CASE CHR$(INS)
          Choice$ = CHR$(CTRLV)
        CASE CHR$(DEL)
          Choice$ = CHR$(CTRLG)
        CASE CHR$(UP)
          Choice$ = CHR$(CTRLE)
        CASE CHR$(DOWN)
          Choice$ = CHR$(TABKEY)
        CASE CHR$(SHIFTTAB)
          Choice$ = CHR$(CTRLE)
        CASE CHR$(F6)
          Choice$ = CHR$(NULL)
          NextField = FALSE
          EXIT DO
        CASE CHR$(F1) TO CHR$(F5), CHR$(F7) TO CHR$(F10), CHR$(F11), CHR$(F12)
          NextField = ASC(Choice$)
          Choice$ = CHR$(NULL)
          EXIT DO

        ' Because HOME and END keys don't have control codes, NULL
        ' is used as a signal to ignore.
        CASE CHR$(HOME)
          P = 0
          Choice$ = CHR$(NULL)
        CASE CHR$(ENDK)
          P = MaxP
          Choice$ = CHR$(NULL)

        ' Make other key choices invalid.
        CASE ELSE
          Click
          Choice$ = CHR$(NULL)
      END SELECT
    END IF

    ' Handle one-byte ASCII codes.
    SELECT CASE ASC(Choice$)

      ' If it is null, ignore it.
      CASE NULL

      ' Accept field (and record if NextField is used).
      CASE ENTER
        IF NextField THEN
          EXIT DO
        ELSE
          NextField = FALSE
          EXIT DO
        END IF

      ' Accept the field if NextField is TRUE, otherwise TAB, DOWN, invalid
      CASE TABKEY
        IF NextField THEN
          EXIT DO
        ELSE
          Click
        END IF

      'Go up one field if NextField is TRUE, otherwise UP, CTRLE invalid
      CASE CTRLE
        IF NextField THEN
          NextField = 1
          EXIT DO
        ELSE
          Click
        END IF
               
      ' Restore the original string if NOT FirstTime, otherwise accept record
      CASE ESC
        IF FirstTime THEN
          IF NextField THEN
            NextField = FALSE
          ELSE
            NextField = TRUE
          END IF
          EXIT DO
        ELSE
          LOCATE Row, Column
          GOTO StartEditString
        END IF

      ' CTRL+S or LEFT_75 arrow moves cursor to left.
      CASE CTRLS
        IF P > 0 THEN
          P = P - 1
          LOCATE , Column + P
        ELSE
          Click
        END IF

      ' CTRL+D or RIGHT_77 arrow moves cursor to right.
      CASE CTRLD
        IF P < MaxP THEN
          P = P + 1
          IF Column + P < 80 THEN
            LOCATE , Column + P
          ELSE
            LOCATE , 80
          END IF
        ELSE
          Click
        END IF

      ' CTRL+G or DEL deletes character under cursor.
      CASE CTRLG
        IF FirstTime THEN
          LOCATE , Column
          COLOR Pref.Fore, Pref.Back
          PRINT SPACE$(MaxP);
          LOCATE , Column
          P = 0: MaxP = P
          Work$ = ""
        ELSE
          IF P < MaxP THEN
            Work$ = LEFT$(Work$, P) + RIGHT$(Work$, MaxP - P - 1)
            MaxP = MaxP - 1
          ELSE
            Click
          END IF
        END IF

      ' CTRL+H or BKSP deletes character to left of cursor.
      CASE CTRLH, 127
        IF FirstTime THEN
          LOCATE , Column
          COLOR Pref.Fore, Pref.Back
          PRINT SPACE$(MaxP);
          LOCATE , Column
          P = 0: MaxP = P
          Work$ = ""
        ELSE
          IF P > 0 THEN
            Work$ = LEFT$(Work$, P - 1) + RIGHT$(Work$, MaxP - P)
            P = P - 1
            MaxP = MaxP - 1
          ELSE
            Click
          END IF
        END IF

      ' CTRL+V or INS toggles between insert and overwrite modes.
      CASE CTRLV
          Insert = NOT Insert
          IF Insert THEN
              LOCATE , , , 7, 7
          ELSE
              LOCATE , , , 4, 7
          END IF

      ' Echo ASCII characters to screen.
      CASE IS >= SPACE_32

        ' <<<<<<< This section Modifiable for user preference >>>>>>>>
        ' Clear the field if first keystroke a character
        IF FirstTime THEN
          LOCATE , Column
          COLOR Pref.Fore, Pref.Back
          PRINT SPACE$(MaxP);
          LOCATE , Column
          P = 0: MaxP = P
          Work$ = ""
        END IF

        IF Insert THEN
          ' If insert mode and string less than max length, insert character
          IF MaxP < Length THEN
            Work$ = LEFT$(Work$, P) + Choice$ + RIGHT$(Work$, MaxP - P)
            MaxP = MaxP + 1
            P = P + 1
          ELSE
            Click
          END IF
        ELSE
          ' If overwrite mode and cursor at end, insert character.
          IF P = MaxP THEN
            IF MaxP < Length THEN
              Work$ = Work$ + Choice$
              MaxP = MaxP + 1
              P = P + 1
            ELSE
              Click
            END IF
          ELSE
            ' If overwrite mode and before end, overwrite character.
            MID$(Work$, P + 1, 1) = Choice$
            P = P + 1
          END IF
        END IF

      ' Consider other key choices invalid.
      CASE ELSE
        Click
    END SELECT
     
    ' Print the modified string.
    COLOR Pref.Fore, Pref.Back
    LOCATE Row, Column, 0
    PRINT STRING$(Length, "_");
    LOCATE Row, Column, 0
    PRINT Work$;
    IF Column + P < 80 THEN
      LOCATE , Column + P
    ELSE
      LOCATE , 80
    END IF
   
    FirstTime = FALSE

  LOOP

  ' Print the final string and assign it to function name.
  COLOR Pref.Fore, Pref.Back
  LOCATE Row, Column, 0
  PRINT SPACE$(Length);
  LOCATE Row, Column, 0
  PRINT Work$;
  EditString$ = Work$
  LOCATE Row, Column

END FUNCTION

'Valid________________________________________________________________FUNCTION
'  Determines whether fixed or variable length string contains characters
'  other than NULL and SPACE_32
'
'USES
'  CONST FALSE
'  CONST TRUE
'  CONST NULL
'
'PARAMETERS
'  Entry : String to test for validity                              -> STRING
'
'VARIABLES
'  i : Loop counter                                                 -> INTEGER
'  V : Temporary variable                                           -> INTEGER
'
FUNCTION Valid% (Entry AS STRING)

  V = FALSE
  DO
    i = i + 1
    IF MID$(Entry, i, 1) <> CHR$(NULL) AND MID$(Entry, i, 1) <> " " THEN
      V = TRUE
    END IF
  LOOP UNTIL V OR i = LEN(Entry)
  Valid = V

END FUNCTION


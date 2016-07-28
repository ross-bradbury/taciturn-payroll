'EDNUMBER.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  EDNUMBER.BI contains necessary declarations for insertion

DEFINT A-Z
DECLARE FUNCTION EditNum! (InNum AS SINGLE, Length1%, Length2%, NextField%)
DECLARE FUNCTION GetKey$ (Flash)
DECLARE FUNCTION Round! (Number AS SINGLE, Places%)

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

'EditNum______________________________________________________________FUNCTION
'  Creates or modifies a number of specified length with standard editing
'  commands. LEFT_75, RIGHT_77, BKSP, DEL, INS, ESC, ENTER, TAB, SHIFTTAB, UP, DOWN
'  are all valid editing commands along with their CTRL key equivalents.
'
'  IMPORTANT: Although this FUNCTION usually returns a SINGLE precision real,
'  it can be assigned to an INTEGER and the result will be rounded in the
'  conversion.
'
'USES
'  SUB Click
'  FUNCTION GetKey
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
'  InNum     : Number to modify, can be zero to create new          -> SINGLE
'  Length1   : Maximum length of integer portion                    -> INTEGER
'  Length2   : Maximum length of fraction portion                   -> INTEGER
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
'  SCREEN : Displays number as edited
'
'VARIABLES
'  Choice$   : Key-press                                            -> INTEGER
'  Column    : Column of screen that cursor was on when called      -> INTEGER
'  FirstTime : Determines whether this is the first key-stroke      -> INTEGER
'  Half      : Active portion of number                             -> INTEGER
'  Insert    : TRUE if INSERT on, FALSE if off                STATIC-> INTEGER
'  Length()  : Length of each half of number                   ARRAY-> INTEGER
'  MaxP      : Last used position in string                         -> INTEGER
'  Num$()    : String used to edit each half of number         ARRAY-> STRING
'  P         : Current position in string                           -> INTEGER
'  Row       : Row of screen that cursor was on when called         -> INTEGER
'
'LABELS
'  StartEditNum
'
FUNCTION EditNum (InNum AS SINGLE, Length1 AS Integer, Length2 AS Integer, NextField AS Integer) as Single
''''  SHARED Pref AS INITIALS
  STATIC Insert
  DIM Num$(1 TO 2), Length(1 TO 2)

  IF Pref.Click THEN CALL Click

  ' Initialize variables and clear field to its maximum length.
  Row = CSRLIN: Column = POS(0)

StartEditNum:

  Half = 1
  Length(1) = Length1
  Length(2) = Length2
  IF Length2 > 0 THEN
    Num$(1) = LTRIM$(STR$(INT(InNum)))
  ELSE
    Num$(1) = LTRIM$(STR$(Round(InNum, 0)))
  END IF
  IF Num$(1) = "0" THEN Num$(1) = ""
  IF INSTR(STR$(InNum), ".") THEN
    Num$(2) = RIGHT$(STR$(InNum), LEN(STR$(InNum)) - INSTR(STR$(InNum), "."))
    IF LEN(Num$(2)) > Length(2) THEN
      InNum = Round(InNum, Length(2))
      Num$(2) = RIGHT$(STR$(InNum), LEN(STR$(InNum)) - INSTR(STR$(InNum), "."))
    END IF
  END IF
  FirstTime = TRUE
  MaxP = LEN(Num$(1))
  P = 0
 
  ' Since Insert is STATIC, its value is maintained from one call to the
  ' next. Insert is 0 (FALSE) the first time the function is called.
  IF Insert THEN
    LOCATE Row, Column, 1, 7, 7
  ELSE
    LOCATE Row, Column, 1, 4, 7
  END IF

  ' Reverse video on entry.
  COLOR Pref.HiFore, Pref.HiBack
  PRINT STRING$(Length(1) - LEN(Num$(1)), "_"); Num$(1);
  IF Length(2) THEN
    PRINT "." + Num$(2) + STRING$(Length(2) - LEN(Num$(2)), "0");
  END IF
  IF Half = 1 THEN
    IF MaxP = P THEN
      LOCATE , Column + Length(Half), 1
    ELSE
      LOCATE , Column + Length(Half) - MaxP + P, 1
    END IF
  ELSE
    LOCATE , Column + Length(Half) + 1 + P, 1
  END IF
  COLOR Pref.Fore, Pref.Back

  ' Process keys until either TAB or ENTER is pressed.
  DO

    ' Get a key -- either a one-byte ASCII code or a two-byte extended code.
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
          Half = 1
          MaxP = LEN(Num$(Half))
          P = 0
          Choice$ = CHR$(NULL)
        CASE CHR$(ENDK)
          IF Length(2) > 0 THEN
            Half = 2
          END IF
          MaxP = LEN(Num$(Half))
          P = MaxP
          Choice$ = CHR$(NULL)

        ' Make other key choices invalid.
        CASE ELSE
          Choice$ = CHR$(NULL)
      END SELECT
    END IF

    ' Handle one-byte ASCII codes.
    SELECT CASE ASC(Choice$)

      ' If it is null, ignore it.
       CASE NULL

      ' Accept field (and card if NextField is used).
      CASE ENTER
        IF NextField THEN
          EXIT DO
        ELSE
          NextField = FALSE
          EXIT DO
        END IF

      ' Accept the field unless NextField is used. If NextField
      ' is cleared, TAB is invalid.
      CASE TABKEY
        IF NextField THEN
          EXIT DO
        ELSE
          Click
        END IF

      'Go up one record
      CASE CTRLE
        IF NextField THEN
          NextField = 1
          EXIT DO
        ELSE
          Click
        END IF
        
      ' Restore the original string.
      CASE ESC
        IF FirstTime THEN
          IF NextField THEN
            NextField = FALSE
          ELSE
            NextField = TRUE
          END IF
          EXIT DO
        ELSE
          COLOR Pref.Fore, Pref.Back
          LOCATE Row, Column
          GOTO StartEditNum
        END IF

      ' CTRL+S or LEFT_75 arrow moves cursor to left.
      CASE CTRLS
        IF P > 0 THEN
          P = P - 1
          IF Half = 1 THEN
            LOCATE , Column + Length(Half) - P
          ELSE
            LOCATE , Column + Length(Half) + P
          END IF
        ELSEIF Half = 2 THEN
          Half = 1
          MaxP = LEN(Num$(Half))
          P = MaxP
        ELSE
          Click
        END IF

      ' CTRL+D or RIGHT_77 arrow moves cursor to right.
      CASE CTRLD
        IF P < MaxP THEN
          P = P + 1
          IF Half = 1 THEN
            LOCATE , Column + Length(Half) - P
          ELSE
            LOCATE , Column + Length(Half) + P
          END IF
        ELSEIF Half = 1 AND Length(2) > 0 THEN
          Half = 2
          MaxP = LEN(Num$(Half))
          P = 0
        ELSE
          Click
        END IF

      ' CTRL+G or DEL deletes character under cursor.
      CASE CTRLG
        IF FirstTime THEN
          LOCATE , Column
          COLOR Pref.Fore, Pref.Back
          PRINT SPACE$(Length(1) + Length(2) + 1);
          LOCATE , Column
          P = 0: MaxP = P
          Num$(1) = ""
          Num$(2) = ""
        ELSE
          IF P < MaxP THEN
            Num$(Half) = LEFT$(Num$(Half), P) + RIGHT$(Num$(Half), MaxP - P - 1)
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
          PRINT SPACE$(Length(1) + Length(2) + 1);
          LOCATE , Column
          P = 0: MaxP = P
          Num$(1) = ""
          Num$(2) = ""
        ELSE
          IF P > 0 THEN
            Num$(Half) = LEFT$(Num$(Half), P - 1) + RIGHT$(Num$(Half), MaxP - P)
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

      ' PERIOD goes to next half.
      CASE ASC(".")
        IF Half = 1 AND Length2 > 0 THEN
          Num$(Half) = LEFT$(Num$(Half), P)
          Half = 2
        END IF
        MaxP = LEN(Num$(Half))
        P = 0

      ' Echo ASCII characters to screen.
      CASE 48 TO 57

'        ' <<<<<<<This section Modifiable for user preference>>>>>>>>     '
'        ' Clear the field if this is first keystroke, then               '
'        ' start from the beginning.                                      '
'        IF FirstTime THEN                                                '
'          LOCATE , Column                                                '
'          COLOR Pref.Fore, Pref.Back                                   '
'          PRINT SPACE$(MaxP);                                            '
'          LOCATE , Column                                                '
'          P = 0: MaxP = P                                                '
'          Num$(Half) = ""                                                '
'        END IF                                                           '
       
        IF Length(Half) = 1 THEN
           LOCATE , Column
           COLOR Pref.Fore, Pref.Back
           PRINT SPACE$(MaxP);
           LOCATE , Column
           P = 0: MaxP = P
           Num$(Half) = ""
        END IF

        ' If insert mode and cursor not beyond end, insert character.
        IF Insert THEN
          IF MaxP < Length(Half) THEN
            Num$(Half) = LEFT$(Num$(Half), P) + Choice$ + RIGHT$(Num$(Half), MaxP - P)
            MaxP = MaxP + 1
            P = P + 1
          ELSE
            Click
          END IF
        ELSE
          ' If overwrite mode and cursor at end (but not beyond),
          ' insert character.
          IF P = MaxP THEN
            IF MaxP < Length(Half) THEN
              Num$(Half) = Num$(Half) + Choice$
              MaxP = MaxP + 1
              P = P + 1
            ELSE
              Click
            END IF
          ' If overwrite mode and before end, overwrite character.
          ELSE
            MID$(Num$(Half), P + 1, 1) = Choice$
            P = P + 1
          END IF
        END IF

      ' Consider other key choices invalid.
      CASE ELSE
        Click
   
    END SELECT
    
    ' Print the modified string.
    COLOR Pref.Fore, Pref.Back
    LOCATE , Column, 0
    PRINT STRING$(Length(1) - LEN(Num$(1)), "_"); Num$(1);
    IF Length(2) THEN
      PRINT "." + Num$(2) + STRING$(Length(2) - LEN(Num$(2)), "0");
    END IF
    IF Column + P + Length(1) * (Half - 1) < 80 THEN
      IF Half = 1 THEN
        IF MaxP = P THEN
          LOCATE , Column + Length(1), 1
        ELSE
          LOCATE , Column + Length(1) - MaxP + P, 1
        END IF
      ELSE
        LOCATE , Column + Length(1) + 1 + P, 1
      END IF
    ELSE
      LOCATE Row, 80, 1
    END IF
    FirstTime = FALSE

  LOOP

  ' Print the final string and assign it to function name.
  Num$(1) = LTRIM$(STR$(VAL(Num$(1))))

  COLOR Pref.Fore, Pref.Back
  LOCATE Row, Column, 0
  PRINT SPACE$(Length(1) + Length(2) + 1);
  LOCATE Row, Column, 0
  PRINT SPACE$(Length(1) - LEN(Num$(1))); Num$(1);
  IF Length(2) THEN PRINT "." + Num$(2) + STRING$(Length(2) - LEN(Num$(2)), "0");
   
  EditNum = VAL(Num$(1) + "." + Num$(2))
  LOCATE Row, Column

END FUNCTION

'Round________________________________________________________________FUNCTION
'  Rounds a number to the specified number of places
'
'  IMPORTANT: Although this FUNCTION usually returns a SINGLE precision real,
'  it can be assigned to an INTEGER and the result will be rounded in the
'  conversion.
'
'PARAMETERS
'  Number : Incoming number to be rounded                           -> SINGLE
'  Places : Number of places to round to, can be negative or zero   -> INTEGER
'
FUNCTION Round! (Number AS SINGLE, Places)
 
  IF Places >= 0 THEN
    Round = CLNG(Number * 10 ^ Places) / (10 ^ Places)
  ELSE
    Round = CLNG(Number / 10 ^ -Places) * 10 ^ -Places
  END IF

END FUNCTION


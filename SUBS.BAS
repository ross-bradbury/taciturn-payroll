DECLARE SUB Pause (Alert%)
DECLARE SUB Hint (h$)
DECLARE FUNCTION GetKey$ (Flash)
'SUBS.BAS_______________________________________________________________MODULE
'                                                                 Version 1.00

DEFINT A-Z
DECLARE FUNCTION ListSelection% (Choice%, m$(), Length%, NextField%)
DECLARE SUB Click ()

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
CONST F7 = 65, F8 = 66, F9 = 67, F10 = 68

'$INC_LUDE: 'INITIALS.BI'
'$INC_LUDE: 'TEXTTOOL.BI'

' Definitions of routines in other modules


DEFSNG A-Z
'Directory_________________________________________________________________SUB
'         Display a DOS Directory Page by Page, Sorted by Extension
'
'PROCEDURES USED
'  SUB DisplayTime
'
'OUTPUT
'  Directory displayed on screen
'
SUB Directory

CLS
SHELL "DIR /OE/P"
'SHELL "TREE /F|MORE"
LOCATE 25, 1
PRINT "Press any key to continue . . ."
CALL Pause(FALSE)

END SUB

DEFINT A-Z
FUNCTION ListSelection (Choice, m$(), Length, NextField)
''''  SHARED Pref AS INITIALS
DIM L$(51), L(51)

a$ = " "
Row = CSRLIN: Column = POS(0)
IF Length > 0 THEN
  Click
  NextField = TRUE
END IF
FOR i = 1 TO 50
  IF m$(i) <> "" THEN
    Last = Last + 1
    L$(Last) = m$(i)
    L(Last) = i
    IF Choice = i THEN Choice = Last
    m$(i) = ""
  END IF
NEXT

IF Choice = 0 THEN Choice = 1
OriginalChoice = Choice
FirstTime = TRUE

DO WHILE Length > 0 AND NOT ASC(a$) = ENTER
  IF FirstTime AND NextField OR Last = 1 THEN
    Hint "Page Up or Page Down modifies Ä Enter, Tab, ShiftTab, " + CHR$(24) + ", " + CHR$(25) + " accepts¯³Esc Exits   "
  ELSE
    Hint "Page Up or Page Down modifies Ä Enter, Tab, ShiftTab, " + CHR$(24) + ", " + CHR$(25) + " accepts¯³Esc Restores"
  END IF
  LOCATE Row, Column
  COLOR 15, 4
  PRINT L$(Choice); SPACE$(Length - LEN(L$(Choice)));
  COLOR 10
  IF Last = 1 THEN
    PRINT CHR$(254);
  ELSE
    SELECT CASE Choice
      CASE 1
        PRINT CHR$(25);
      CASE Last
        PRINT CHR$(24);
      CASE ELSE
        PRINT CHR$(18);
    END SELECT
  END IF
  COLOR 15
  a$ = GetKey(FALSE)
  IF LEN(a$) = 2 THEN
    a$ = RIGHT$(a$, 1)
  END IF
  SELECT CASE ASC(a$)
    'PageUp Key
    CASE PgUp
      IF Choice > 1 THEN
        Choice = Choice - 1
      ELSE
        Click
      END IF

    'Page Down Key
    CASE PgDn
      IF Choice < Last THEN
        Choice = Choice + 1
      ELSE
        Click
      END IF

    ' Accept field on Enter.
    CASE ENTER
      NextField = TRUE
      EXIT DO

    ' TAB Accepts the field when NextField is TRUE, otherwise TAB is invalid.
    CASE TABKEY, DOWN
      IF NextField THEN
        EXIT DO
      ELSE
        Click
      END IF

    'Go up one field
    CASE SHIFTTAB, CTRLE, UP
      IF NextField THEN
        NextField = 1
        EXIT DO
      ELSE
        Click
      END IF
        
    CASE ESC
      IF FirstTime THEN
        IF NextField THEN
          NextField = FALSE
        ELSE
          NextField = TRUE
        END IF
        EXIT DO
      ELSE
        Choice = OriginalChoice
        FirstTime = TRUE
      END IF

    'Other keys invalid
    CASE ELSE
      Click

  END SELECT
  IF ASC(a$) <> ESC THEN
    FirstTime = FALSE
  END IF
LOOP

ListSelection = L(Choice)
LOCATE Row, Column
COLOR Pref.Fore, Pref.Back
PRINT L$(Choice); SPACE$(ABS(Length) - LEN(L$(Choice)) + 1);
Hint ""

END FUNCTION


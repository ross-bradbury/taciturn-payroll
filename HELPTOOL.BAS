'HELPTOOL.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  HELPTOOL.BI contains necessary declarations for insertion

DEFINT A-Z
DECLARE SUB Pointer (y%, x%)
DECLARE SUB Hint (H$)

CONST FALSE = 0
CONST TRUE = NOT FALSE

'$INCLUDE: 'INITIALS.BI'

' Definitions of routines in other modules
DECLARE SUB ClearLine (y%, z%)

'Hint______________________________________________________________________SUB
'  Displays text on the bottom line of the screen in yellow on red
'
'USES
'  SUB ClearLine
'  SHARED Pref AS INITIALS
'  CONST TRUE
'
'PARAMETERS
'  h$ : Text to display                                             -> STRING
'
'OPTIONS ([h$])
'  It the "¯" character (175) is in the string, everything to the right of
'   it will be against the right margin
'
'OUTPUT
'  SCREEN : Bottom line displays the Hint
'
'VARIABLES
'  h1$   : First half of hint                                       -> STRING
'  h2$   : Second half of hint                                      -> STRING
'  i     : Loop counter                                             -> INTEGER
'  L     : Line cursor was on                                       -> INTEGER
'  P     : Column cursor was on                                     -> INTEGER
'  Split : True if hint is in two parts                             -> BOOLEAN
'
SUB Hint (H$)
  SHARED Pref AS INITIALS

  L = CSRLIN
  P = POS(0)

  ClearLine 25, 25

  FOR i = 1 TO LEN(H$)
    IF MID$(H$, i, 1) = "¯" THEN
      h1$ = LEFT$(H$, i - 1)
      h2$ = RIGHT$(H$, LEN(H$) - i)
      Split = TRUE
    END IF
  NEXT

  IF NOT Split THEN h1$ = H$

  IF H$ = "" THEN
    COLOR Pref.Fore, Pref.Back
  ELSE
    COLOR 14, 4
  END IF
  LOCATE 25, 1
  PRINT h1$; SPC(80 - (LEN(h1$) + LEN(h2$))); h2$;
  COLOR Pref.Fore, Pref.Back
  LOCATE L, P

END SUB

'Pointer___________________________________________________________________SUB
'  Displays a flashing red ">" at the specified coordinates
'
'USES
'  SHARED Pref AS INITIALS
'
'PARAMETERS
'  y : Column to display pointer on                                 -> INTEGER
'  z : Row to display pointer on                                    -> INTEGER
'
'OUTPUT
'  SCREEN : Displays a flashing red ">"
'
'VARIABLES
'  L     : Line cursor was on                                       -> INTEGER
'  P     : Column cursor was on                                     -> INTEGER
'
SUB Pointer (y, x)
  SHARED Pref AS INITIALS

  L = CSRLIN
  P = POS(0)
  LOCATE y, x
  COLOR 28
  PRINT ">";
  COLOR Pref.Fore
  LOCATE L, P

END SUB


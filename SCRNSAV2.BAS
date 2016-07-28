'SCRNSAVE.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  SCRNSAVE.BI contains necessary declarations for insertion

DEFINT A-Z
DECLARE SUB RememberScreen (Opt%, Page%, y1%, x1%, y2%, x2%)

CONST FALSE = 0
CONST TRUE = NOT FALSE

'$INCLUDE: 'INITIALS.BI'

'RememberScreen____________________________________________________________SUB
'  "Remember" the Screen for Display After Screen-Saver
'
'PARAMETERS
'  Opt : Capture or re-display screen                               -> INTEGER
'  y1  : First row to remember                                      -> INTEGER
'  x1  : First column to remember                                   -> INTEGER
'  y2  : Last row to remember                                       -> INTEGER
'  x2  : Last column to remember                                    -> INTEGER
'
'OPTIONS ([Opt])
'  1 : Capture
'  2 : Display
'
'OUTPUT
'  SCREEN : Displays screen after Screen-Saver
'
'VARIABLES
'  b         : Color for Pref.Back                                  -> INTEGER
'  c         : Color for Pref.Fore                                  -> INTEGER
'  E         : Compensate for an odd number of columns              -> INTEGER
'  x         : Loop counter (FOR..NEXT)                             -> INTEGER
'  y         : Loop counter (FOR..NEXT)                             -> INTEGER
'  Screen1() : Character at screen coordinates                 ARRAY-> INTEGER
'  Screen2() : Color attributes at screen coordinates          ARRAY-> INTEGER
'
SUB RememberScreen (Opt, Page, y1, x1, y2, x2) STATIC
  DIM Buffer(0 TO 3) AS STRING * 4000
  DIM InUse(0 TO 4)

  SELECT CASE Opt
    ' Store screen in arrays
    CASE 1
      i = 0
      WHILE InUse(i) AND i <= 4
        i = i + 1
      WEND
      Page = i
      InUse(Page) = TRUE
     
      'New Code to Save Screen
      CALL PutScrn(Buffer(Page))
     
    ' Restore screen from arrays
    CASE 2
      i = 0
      WHILE InUse(i + 1) AND i + 1 <= 4
        i = i + 1
      WEND
      Page = i
     
      'New Code to Save Screen
      CALL GetScrn(Buffer(Page))
     
      InUse(Page) = 0
  END SELECT

END SUB

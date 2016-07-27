DECLARE SUB Bounce ()
'SCRNSAVE.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  SCRNSAVE.BI contains necessary declarations for insertion

DEFINT A-Z
DECLARE SUB ScreenSaver ()
DECLARE SUB RememberScreen (Opt%, Page%, y1%, x1%, y2%, x2%)
DECLARE SUB TextBounce (ChangeModes%)

CONST FALSE = 0
CONST TRUE = NOT FALSE

'$INCLUDE: 'CAPSLOCK.BI'
'$INCLUDE: 'SSBOUNCE.BI'
'$INCLUDE: 'INITIALS.BI'

' Error handler to determine screen mode to use
TextScreenError:
  SELECT CASE Rows
    CASE 60
      Rows = 50
    CASE 50
      Rows = 43
    CASE 43
      Rows = 30
    CASE 30
      Rows = 25
  END SELECT
  RESUME

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

'ScreenSaver_______________________________________________________________SUB
'  Screen Saver Routine
'
'USES
'  SHARED Pref AS INITIALS
'  SUB Bounce
'  SUB TextBounce
'
'OPTIONS
'  All options are set by the required INTIALS
'
SUB ScreenSaver
  SHARED Pref AS INITIALS

  IF Pref.SSGraphics OR Pref.SSChangeModes THEN
    LOCATE 1, 70, 0
    COLOR 30, 4
    PRINT "SWAPPING!!!"
    CALL RememberScreen(1, Page, 1, 1, 25, 80)
    IF Pref.SSGraphics THEN
      CALL Bounce
    ELSE
      CALL TextBounce(Pref.SSChangeModes)
    END IF
    SCREEN 0, 1, 0, 0
    WIDTH 80, 25
    COLOR 0, 0
    CLS
    CALL RememberScreen(2, Page, 1, 1, 25, 80)
    COLOR Pref.Fore, Pref.Back
  ELSE
    TextBounce (Pref.SSChangeModes)
  END IF

END SUB

'TextBounce________________________________________________________________SUB
'  Simple text-based screen saver. Allows quick swaps and no graphics
'  capability required.
'
'USES
'  SHARED Pref as INITIALS
'
'PARAMETERS
'  ChangeModes : Whether to remember screen and change modes        -> INTEGER
'
'OPTIONS ([ChangeModes])
'  TRUE  : Remember, run saver with max rows, and re-display screen
'  FALSE : Change screen page, run saver, and return to page 0
'
'OUTPUT
'  SCREEN : Displays colored blocks to "save" screen
'
'VARIABLES
'  a$         : Determines if a key has been pressed                -> STRING
'  c          : Sets the active color of three                      -> INTEGER
'  i          : Loop counter to determine when to change colors     -> INTEGER
'  Rows       : Number of rows in the current screen mode     SHARED-> INTEGER
'  StartTime# : Temporary variable for speed check                  -> LONG
'  W          : Speed check loop counter                            -> INTEGER
'  Warp       : Speed Check                                         -> INTEGER
'
SUB TextBounce (ChangeModes)
  SHARED Rows, Pref AS INITIALS

  IF ChangeModes THEN
    ' Determine maximum rows of text than can be displayed
    ON ERROR GOTO TextScreenError
      Rows = 60
      WIDTH 80, Rows
    ON ERROR GOTO 0
  ELSE
    ' Standard 25 rows if quick changes prefered
    Rows = 25
    SCREEN 0, 1, 1, 1
  END IF

  RANDOMIZE TIMER
  COLOR Pref.Fore, 0
  CLS
 
  StartShift$ = ShiftState
  IF MID$(StartShift$, 5, 1) = "1" THEN
    PRINT "Release the Alt key"
    DO
      StartShift$ = ShiftState
    LOOP WHILE MID$(StartShift$, 5, 1) = "1"
  END IF
  DEF SEG = 0
  Shift = PEEK(&H417)
  COLOR Pref.Back, 0
  CLS

  ' Slow down fast processors
  StartTime# = TIMER
  DO
    W = W + 1
  LOOP UNTIL TIMER > StartTime# + .1
  Warp = W * .5

  DO
    i = 0
    c = c + 1
    IF c = 4 THEN c = 1
    SELECT CASE c
      CASE 1
        COLOR Pref.Back, Pref.Fore
      CASE 2
        COLOR Pref.Fore, Pref.Back
      CASE 3
        COLOR 0
    END SELECT
    DO
      i = i + 1
      LOCATE INT(RND(1) * Rows) + 1, INT(RND(1) * 80) + 1
      PRINT CHR$(219);
      a$ = INKEY$
      ' Slow down fast processors
      W = 0
      DO
        W = W + 1
      LOOP UNTIL W >= Warp
    LOOP WHILE i < 80 * Rows * 1.15 AND a$ = "" AND Shift = PEEK(&H417)
  LOOP WHILE a$ = "" AND Shift = PEEK(&H417)

  SCREEN 0, 1, 0, 0
  COLOR Pref.Fore, Pref.Back
 
  IF ABS(PEEK(&H417) - Shift) > 8 THEN POKE &H417, Shift

END SUB


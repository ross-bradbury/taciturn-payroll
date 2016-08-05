'TIMETOOL.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  TIMETOOL.BI contains necessary declarations for insertion
'
'REQUIRED MODULES
'  MODULE SCRNSAVE.BAS
'  LIBRARY Q4T

DEFINT A-Z
DECLARE SUB AutoPause (Length AS SINGLE)
DECLARE SUB Click ()
DECLARE SUB Pause (Alert%)
DECLARE SUB Pause.Alert ()
DECLARE SUB PleaseWait ()
DECLARE SUB Time ()
DECLARE FUNCTION Rand% (Min%, Max%)

CONST FALSE = 0
CONST TRUE = NOT FALSE

''$INCLUDE: 'SCRNSAVE.BI'
'$INCLUDE: 'INITIALS.BI'

'AutoPause_________________________________________________________________SUB
'  Waits for specified number of seconds (max accuracy is .01 sec)
'
'PARAMETERS
'  Length : Number of seconds to wait, accurate to 0.01             -> INTEGER
'
'VARIABLES
'  StartTime : Time that AutoPause was called, from TIMER           -> LONG
'
SUB AutoPause (Length AS SINGLE)

  StartTime# = TIMER
  WHILE TIMER < StartTime# + Length
  WEND

END SUB

'Click_____________________________________________________________________SUB
'  Plays a sound at the PC SPEAKER
'
'OUTPUT
'  SPEAKER : Play a "Click"
'
SUB Click

  SOUND 3500, .1

END SUB

'Pause_____________________________________________________________________SUB
'  Pauses and waits for a key to be pressed and then discards key stroke
'
'USES
'  SUB Pause.Alert
'  SUB Time
'  SUB ScreenSaver
'
'PARAMETERS
'  Alert : Alert user to press a key in place of time               -> BOOLEAN
'
'OPTIONS ([Alert])
'  TRUE, -1 : Display "PRESS A KEY" in place of time
'  FALSE, 0 : Display current time
'
'OUTPUT
'  SCREEN; Alert = TRUE  : Flashes "PRESS A KEY"  
'  SCREEN; Alert = FALSE : Displays current time in top right corner
'
'VARIABLES
'  a$ : Temporary variable to determine if a key was pressed        -> STRING
'  s  : Number of seconds since Pause was called                    -> LONG
'
SUB Pause (Alert)
  DIM s AS LONG

  IF Alert THEN CALL Pause.Alert
  s = INT(TIMER)

  DO
'    CALL MouseClick(Left, 0, Right)
    a$ = INKEY$
    IF NOT Alert THEN CALL Time
    'Engage screen saver if delay has elapsed or Alt+s entered
    IF TIMER > s + (2 * 60) OR a$ = CHR$(0) + CHR$(31) THEN
'      CALL ScreenSaver
      s = INT(TIMER)
      a$ = ""
      IF Alert THEN CALL Pause.Alert
    END IF
  LOOP UNTIL a$ <> "" OR Left OR Right

END SUB

'Pause.Alert_______________________________________________________________SUB
'  SUB required by Pause. Flashes "PRESS A KEY" in place of time
'
'USES
'  SHARED Pref AS INITIALS
'
'OUTPUT
'  SCREEN : Flashes "PRESS A KEY"
'
'VARIABLES
'  L : Line cursor was on                                           -> INTEGER
'  P : Column cursor was on                                         -> INTEGER
'
SUB Pause.Alert
  SHARED Pref AS INITIALS
  
  L = CSRLIN
  P = POS(0)
  COLOR 28, Pref.Back
  LOCATE 1, 70, 0
  PRINT "PRESS A KEY";
  COLOR Pref.Fore, Pref.Back
  LOCATE L, P, 0

END SUB

'PleaseWait________________________________________________________________SUB
'  Alerts user to wait for a delay; "PLEASE WAIT" takes the place of the time
'
'USES
'  SHARED Pref AS INITIALS
'
'OUTPUT
'  SCREEN : Flashes "PLEASE WAIT" in upper right corner
'
'VARIABLES
'  L : Line cursor was on                                           -> INTEGER
'  P : Column cursor was on                                         -> INTEGER
'
SUB PleaseWait
  SHARED Pref AS INITIALS

  L = CSRLIN
  P = POS(0)
  COLOR 12, Pref.Back
  LOCATE 1, 70, 0
  PRINT "PLEASE WAIT";
  COLOR Pref.Fore, Pref.Back
  LOCATE L, P, 0

END SUB

'Rand_________________________________________________________________FUNCTION
'  Creates a random integer between [Min] and [Max] inclusive
'
'PARAMETERS
'  Min : Minimum value of random integer                            -> INTEGER
'  Max : Maximum value of random integer                            -> INTEGER
'
FUNCTION Rand (Min, Max)

  RANDOMIZE TIMER
  Rand = INT((Max - Min + 1) * RND + Min)

END FUNCTION

'Time______________________________________________________________________SUB
'  Displays the current formatted time in the top right corner of the screen
'
'USES
'  SHARED Pref AS INITIALS
'
'OUTPUT
'  SCREEN : Displays current time in top right corner
'
'VARIABLES
'  L     : Line cursor was on                                       -> INTEGER
'  P     : Column cursor was on                                     -> INTEGER
'  t$    : Formatted time                                           -> STRING
'  Time1 : Number of seconds past Midnight                          -> LONG
'
SUB Time STATIC
  SHARED Pref AS INITIALS
  DIM Time1 AS LONG

  L = CSRLIN
  P = POS(0)
  COLOR Pref.Fore, Pref.Back
   
  ' INT(TIMER) used to speed up routine
  IF Time1 < INT(TIMER) THEN
    'Convert time to 12 hour format and add "AM" or "PM"
    t$ = TIME$
    IF LEFT$(t$, 2) > "12" THEN
      hoursAsInt = VAL(LEFT$(t$, 2)) + .1
      t$ = STR$(hoursAsInt - 12) + MID$(t$, 3, 6) + " PM"
    ELSE
      t$ = t$ + " AM"
    END IF
    IF LEFT$(t$, 2) = "00" THEN MID$(t$, 1, 2) = "12"
    IF LEN(t$) = 11 THEN t$ = " " + t$
    LOCATE 1, 69, 0
    PRINT t$;
    Time1 = INT(TIMER)
  END IF
  LOCATE L, P

END SUB


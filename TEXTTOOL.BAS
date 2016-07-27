DECLARE SUB Time ()
DECLARE SUB ScreenSaver ()
DECLARE FUNCTION ShortString$ (Text AS STRING)
'TEXTTOOL.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  TEXTTOOL.BI contains necessary declarations for insertion
'
'REQUIRES
'  MODULE SCRNSAVE.BAS
'  MODULE TIMETOOL.BAS
'  LIBRARY Q4T

DEFINT A-Z
DECLARE SUB Center (Row%, Text$, Fore%, Back%)
DECLARE SUB ClearLine (y%, z%)
DECLARE SUB Header (Title$, Company%, UseDate%)
DECLARE SUB HighLight (y%, x%, Length%)
DECLARE FUNCTION GetKey$ (Flash%)
DECLARE FUNCTION ShortString$ (Text AS STRING)
DECLARE FUNCTION StringLength% (Text AS STRING)

CONST FALSE = 0
CONST TRUE = NOT FALSE

'$INCLUDE: 'TIMETOOL.BI'
'$INCLUDE: 'SCRNSAVE.BI'

'$INCLUDE: 'INITIALS.BI'

DATA "  LIMITED  "," CAPACITY  "
DATA " SHAREWARE ","  VERSION  ","  PLEASE   "," REGISTER  ","    TO     "
DATA "  RECIEVE  "
DATA "   FULL    ","  VERSION  ","    AND    ","    BE     "," NOTIFIED  "
DATA "    OF     ","  PROGRAM  ","  UPDATES  "

'Center____________________________________________________________________SUB
'  Center a String on any Line and in any Color
'
'USES
'  SHARED Pref AS INITIALS
'
'PARAMETERS
'  Row   : Row [Text$] is to be centered on                         -> INTEGER
'  Text$ : The string to print                                      -> STRING
'  Fore  : Color for ForeGround                                     -> INTEGER
'  Back  : Color for BackGround                                     -> INTEGER
'
'OUTPUT
'  The string [Text$] centered on [Row] in COLOR [Fore, Back]
'
SUB Center (Row, Text$, Fore, Back)
  SHARED Pref AS INITIALS

  LOCATE Row, (80 - LEN(Text$)) \ 2
  COLOR Fore, Back
  PRINT Text$;
  COLOR Pref.Fore, Pref.Back

END SUB

'ClearLine_________________________________________________________________SUB
'  Clears the specified lines on an 80 column screen and returns to line [y]
'
'PARAMETERS
'  y : First line to be cleared                                     -> INTEGER
'  z : Last line to be cleared                                      -> INTEGER
'                 
'OUTPUT
'  Blank lines from line [y] to line [z]
'
'VARIABLES
'  i : Loop counter                                                 -> INTEGER
'
SUB ClearLine (y, z)

  FOR i = y TO z
    LOCATE i, 1
    PRINT SPACE$(80);
  NEXT
  LOCATE y, 1

END SUB

'GetKey_______________________________________________________________FUNCTION
'  Calls Time SUB and returns key press or mouse use
'  Runs Screen Saver after Pref.SSTimeOut min. or when 'Alt' + 's' is Pressed
'
'USES
'  SUB Time          IN TIMETOOL.BAS
'
'
'
'  SUB ScreenSaver   IN SCRNSAVE.BAS
'  SUB MouseClick    IN Q4T.QLB
'  SUB MouseLocate   IN Q4T.QLB
'  SUB MousePosition IN Q4T.QLB
'  SUB MouseStatus   IN Q4T.QLB
'  SHARED Pref AS INITIALS
'
'PARAMETERS
'  Flash : Chooses flashing cursor or no cursor                     -> BOOLEAN
'
'OPTIONS ([Flash])
'  TRUE, -1 : Display a flashing cursor at current coordinates
'  FALSE, 0 : Does not display a cursor
'
'OUTPUT
'  SCREEN : Displays current time in top right corner
'
'VARIABLES
'  a$    : Temporary variable to determine if a key was pressed     -> STRING
'  L     : Line cursor was on                                       -> INTEGER
'  P     : Column cursor was on                                     -> INTEGER
'  s     : Number of seconds since GetKey was called                -> LONG
'  ox    : Old Mouse X Coordinate                                   -> INTEGER
'  oy    : Old Mouse Y Coordinate                                   -> INTEGER
'  x     : New Mouse X Coordinate                                   -> INTEGER
'  y     : New Mouse Y Coordinate                                   -> INTEGER
'  Mb1   : TRUE if Left Mouse Button Pressed                        -> INTEGER
'  Mb2   : TRUE if Right Mouse Button Pressed                       -> INTEGER
'                                                                        
FUNCTION GetKey$ (Flash)
  SHARED Pref AS INITIALS
  DIM s AS LONG
  STATIC Share AS SINGLE
  STATIC ShareStage

  CALL MouseStatus(Mouse)
  y = 12
  x = 40
  ' Save old mouse position
  IF Mouse THEN CALL MousePosition(oy, ox, 1)
  ' Set mouse to center of screen
  IF Mouse THEN CALL MouseLocate(y, x, 1)

  L = CSRLIN
  P = POS(0)
  COLOR Pref.Fore, Pref.Back
  s = INT(TIMER)

  DO
    IF Mouse THEN CALL MousePosition(y, x, 1)
    IF Mouse THEN CALL MouseClick(Mb1, 0, Mb2)
    a$ = INKEY$
    IF ShareStage = 0 THEN CALL Time
   
    'Engage screen saver if delay has elapsed or Alt+s entered
    IF TIMER > s + (Pref.SSTimeOut * 60) AND Pref.SSActive OR a$ = CHR$(0) + CHR$(31) THEN
      CALL ScreenSaver
      s = INT(TIMER)
      a$ = ""
    END IF

'    ' ShareWare Version ONLY
'    IF TIMER > Share THEN
'      Share = TIMER + .5
'      ShareStage = ShareStage + 1
'      LOCATE 1, 70
'      COLOR 14, 4
'      SELECT CASE ShareStage
'        CASE 1 TO 15
'          READ Message$
'          PRINT Message$
'        CASE 16
'          READ Message$
'          PRINT Message$
'          RESTORE
'        CASE 17
'          ShareStage = 0
'          Share = TIMER + 15
'      END SELECT
'      COLOR Pref.Fore, Pref.Back
'    END IF



    IF Flash THEN
      LOCATE L, P, 1
    ELSE
      LOCATE L, P, 0
    END IF
  LOOP UNTIL a$ <> "" OR ABS(12 - y) > 2 OR ABS(40 - x) > 2 OR Mb1 OR Mb2
  
  IF a$ = "" THEN
    IF y > 12 THEN
      a$ = CHR$(0) + "P"      'DOWN
    ELSEIF y < 12 THEN
      a$ = CHR$(0) + "H"      'UP
    ELSE
      IF x > 40 THEN
        a$ = CHR$(0) + "M"    'RIGHT
      ELSEIF x < 40 THEN
        a$ = CHR$(0) + "K"    'LEFT
      END IF
    END IF
    IF Mb1 THEN a$ = CHR$(13) 'ENTER
    IF Mb2 THEN a$ = CHR$(27) 'Esc
  END IF

  GetKey$ = a$
  ' Put mouse back
  IF Mouse THEN CALL MouseLocate(oy, ox, 1)

END FUNCTION

'Header____________________________________________________________________SUB
'  Makes a 1 or 2 line header at the top of the screen with a border
'
'USES
'  SHARED Pref AS INITIALS
'  SUB Time IN TIMETOOL.BAS
'
'PARAMETERS
'  Title$  : The Title of the screen                                -> STRING
'  Company : Print Company name on first line if TRUE               -> INTEGER
'  UseDate : Print date in top left corner if TRUE                  -> INTEGER
'               
'OUTPUT
'  Header of 2 to 3 lines on thetop of the screen
'
'VARIABLES
'  C$ : Temporary variable for Company or Title                     -> STRING
'  d$ : Converted Date                                              -> STRING
'
SUB Header (Title$, Company, UseDate)
  SHARED Pref AS INITIALS

  Title$ = ShortString(Title$)
  IF Company THEN
    C$ = Pref.Company
  ELSE
    C$ = Title$
  END IF
  LOCATE 1, 1
  PRINT SPACE$(80);
  LOCATE 1, 1
  IF UseDate THEN
    IF StringLength(C$) < 58 THEN
      d$ = DATE$
    ELSE
      d$ = LEFT$(DATE$, 6) + RIGHT$(DATE$, 2)
    END IF
    PRINT d$;
    CALL Time
  END IF
  IF Company THEN
    LOCATE 1, (80 - StringLength(C$)) \ 2
    PRINT LEFT$(C$, StringLength(C$));
  END IF
  IF Company AND Title$ <> "" THEN
    LOCATE 2, 1
    PRINT SPACE$(80);
    LOCATE 2, 1
    CALL Center(2, Title$, Pref.Fore, Pref.Back)
  ELSE
    CALL Center(1, Title$, Pref.Fore, Pref.Back)
  END IF
  PRINT STRING$(80, 223);

END SUB

'HighLight_________________________________________________________________SUB
'  Converts text on screen to highlighted text
'
'USES
'  SHARED Pref AS INITIALS
'
'PARAMETERS
'  y      : Y Coordinate of starting point                          -> INTEGER
'  x      : X Coordinate of starting point                          -> INTEGER
'  Length : Length on the hilight                                   -> INTEGER
'             
'OUTPUT
'  Highlighted Screen Text
'
'VARIABLES
'  i : Loop Counter                                                 -> INTEGER
'
SUB HighLight (y, x, Length)
  SHARED Pref AS INITIALS

  COLOR Pref.HiFore, Pref.HiBack
  FOR i = 0 TO Length - 1
    LOCATE y, x + i
    PRINT CHR$(SCREEN(y, x + i));
  NEXT
  COLOR Pref.MenuFore, Pref.MenuBack

END SUB

'ShortString__________________________________________________________FUNCTION
'  Returns the left part of the string without spaces
'
'PARAMETERS
'  Text : Text to find the length of                                -> STRING
'         
'VARIABLES
'  i : Loop Counter                                                 -> INTEGER
'  L : Temporary Length                                             -> INTEGER
'
FUNCTION ShortString$ (Text AS STRING)
              
  i = 1
  WHILE i <= LEN(Text)
    IF MID$(Text, i, 1) <> CHR$(0) AND MID$(Text, i, 1) <> " " THEN
      L = i
    END IF
    i = i + 1
  WEND
  ShortString = LEFT$(Text$, L)

END FUNCTION

'StringLength_________________________________________________________FUNCTION
'  Returns the used length of a fixed length string
'
'PARAMETERS
'  Text : Text to find the length of                                -> STRING
'           
'VARIABLES
'  i : Loop Counter                                                 -> INTEGER
'  L : Temporary Length                                             -> INTEGER
'
FUNCTION StringLength (Text AS STRING)
               
  i = 1
  WHILE i <= LEN(Text)
    IF MID$(Text, i, 1) <> CHR$(0) AND MID$(Text, i, 1) <> " " THEN
      L = i
    END IF
    i = i + 1
  WEND
  StringLength = L

END FUNCTION


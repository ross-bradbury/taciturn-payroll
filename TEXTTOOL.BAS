'TEXTTOOL.BAS___________________________________________________________MODULE
'                                                                 Version 1.00
'  TEXTTOOL.BI contains necessary declarations for insertion
'
'REQUIRES
'  MODULE SCRNSAVE.BAS
'  MODULE TIMETOOL.BAS

DEFINT A-Z
DECLARE SUB Center (Row%, Text$, Fore%, Back%)
DECLARE SUB ClearLine (y%, z%)
DECLARE SUB Header (Title$, Company%, UseDate%)
DECLARE SUB HighLight (y%, x%, Length%)
DECLARE FUNCTION ShortString$ (Text AS STRING)
DECLARE FUNCTION StringLength% (Text AS STRING)
DECLARE SUB DisplayTime()

DIM SHARED Pref AS INITIALS

CONST FALSE = 0
CONST TRUE = NOT FALSE

'$INC_LUDE: 'TIMETOOL.BI'
'$INC_LUDE: 'SCRNSAVE.BI'

'$INC_LUDE: 'INITIALS.BI'

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
''''  SHARED Pref AS INITIALS

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
'  Calls Time SUB and returns key press
'  Runs Screen Saver after Pref.SSTimeOut min. or when 'Alt' + 's' is Pressed
'
'USES
'  SUB DisplayTime          IN TIMETOOL.BAS
'
'
'
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
'                                                                        
FUNCTION GetKey$ (Flash)
''''  SHARED Pref AS INITIALS

  y = 12
  x = 40

  L = CSRLIN
  P = POS(0)
  COLOR Pref.Fore, Pref.Back

  DO
    a$ = INKEY$
    CALL DisplayTime()
   
    IF Flash THEN
      LOCATE L, P, 1
    ELSE
      LOCATE L, P, 0
    END IF
  LOOP UNTIL a$ <> "" OR ABS(12 - y) > 2 OR ABS(40 - x) > 2
  
  IF a$ = "" THEN
    IF y > 12 THEN
      a$ = CHR$(0) + "P"      'DOWN
    ELSEIF y < 12 THEN
      a$ = CHR$(0) + "H"      'UP
    ELSE
      IF x > 40 THEN
        a$ = CHR$(0) + "M"    'RIGHT_77
      ELSEIF x < 40 THEN
        a$ = CHR$(0) + "K"    'LEFT_75
      END IF
    END IF
  END IF

  GetKey$ = a$

END FUNCTION

'Header____________________________________________________________________SUB
'  Makes a 1 or 2 line header at the top of the screen with a border
'
'USES
'  SHARED Pref AS INITIALS
'  SUB DisplayTime IN TIMETOOL.BAS
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
''''  SHARED Pref AS INITIALS

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
    CALL DisplayTime()
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
''''  SHARED Pref AS INITIALS

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


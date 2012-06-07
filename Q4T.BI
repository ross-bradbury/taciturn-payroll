'----------------------------------------------------------------------------
'                            **    Q4T.BI    **
'     The $INCLUDE file needed for the operation of Q4T.QLB and Q4T.LIB
'                 Written by R. J. Crouch  -  April 1990
'                     Copyright  -  1990  -  CareWare
'----------------------------------------------------------------------------

DECLARE FUNCTION Ctr! (strg$)
DECLARE SUB Delay (lenght!, Forced%)

TYPE RegType
   ax    AS INTEGER
   bx    AS INTEGER
   cx    AS INTEGER
   dx    AS INTEGER
   bp    AS INTEGER
   si    AS INTEGER
   di    AS INTEGER
   flags AS INTEGER
   ds    AS INTEGER
   es    AS INTEGER
END TYPE


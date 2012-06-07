'TEXTTOOL.INC__________________________________________________________INCLUDE
'  Include file for TEXTTOOL.BAS                                  Version 1.00
DECLARE SUB Center (Row%, Text$, Fore%, Back%)
DECLARE SUB ClearLine (y%, z%)
DECLARE SUB Header (Title$, Company%, UseDate%)
DECLARE SUB HighLight (y%, x%, Length%)
DECLARE FUNCTION GetKey$ (Flash%)
DECLARE FUNCTION ShortString$ (Text AS STRING)
DECLARE FUNCTION StringLength% (Text AS STRING)


  TYPE DeductionType
    Active AS INTEGER
    DType  AS INTEGER
    DName  AS STRING * 18
    Amount AS SINGLE
    AType  AS INTEGER
    Limit  AS SINGLE
    LType  AS INTEGER
    Total  AS SINGLE
  END TYPE

  TYPE EmployeeType
    Ref          AS STRING * 8
    EmpType      AS STRING * 1
    EmpName      AS STRING * 40
    Address      AS STRING * 40
    Town         AS STRING * 40
    State        AS STRING * 2
    Zip          AS STRING * 5
    Social       AS STRING * 11
    USHolding    AS STRING * 1
    USAllowances AS INTEGER
    OHHolding    AS STRING * 1
    OHAllowances AS INTEGER
    Wage         AS SINGLE
    Note         AS STRING * 80
    Deduction1   AS DeductionType
    Deduction2   AS DeductionType
    Deduction3   AS DeductionType
    Deduction4   AS DeductionType
  END TYPE

  DIM SHARED EmployeeInfo AS EmployeeType
  DIM SHARED Deduct(4)    AS DeductionType

    'Example to assign the DeductionInfo to EmployeeType------------
    'EmployeeInfo.Deduction1=Deduct(1)
    'EmployeeInfo.Deduction2=Deduct(2)
    'EmployeeInfo.Deduction3=Deduct(3)
    'EmployeeInfo.Deduction4=Deduct(4)


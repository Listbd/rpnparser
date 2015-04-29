The RpnParser written in C#.Net parses a string expression and generates an output value.
Purpose: Evaluation string expressions returning a single result value.
  * upported Arithmetic Operators: **+ - /****%**
  * upported Comparison Operators: **= == != <> < > <= >=**
  * upported Logical Operators: **and && or ||**
  * upported Operands: **Long, Decimal, Bool, Null, String, Date Time, Variable**
  * upport for Variables: Yes, via delegate
  * upport for functions: Yes
  * upport for function parameters: No
  * upport for datetime parsing: No
  * sage: ` new RPNParser().Evaluate("4+4", sender, delegate) `
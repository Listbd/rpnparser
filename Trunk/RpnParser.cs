using System;
using System.Collections;
using System.Text;
using System.Text.RegularExpressions;
//-------------------------------------------------------------------------------
// Unit: RpnParser
// Date: 04/22/2005
// Purpose: Evaluation string expressions returning a single result value.
// Supported Arithmetic Operators: + - / * %
// Supported Comparison Operators: = == != <> < > <= >=
// Supported Logical Operators: and && or ||
// Supported Operands: Long, Decimal, Bool, Null, String, DateTime, Variable
// Support for Variables: Yes, via delegate
// Support for functions: No
// Support for datetime parsing: No
// Usage: new RPNParser().Evaluate("4+4", sender, delegate)
//-------------------------------------------------------------------------------
//Changes
//1. Regular expression splits by operand rather than operator
//2. Removed the formula boolean
//3. Variables are set via a delegate call which is passed in.
//4. Can mix variables and literals
//5. Don't have to define the expressions type up front
//6. Accepts strings
//7. Added IVariableOperand interface for handling unknown types
//8. Removed "name" from IOperand
//9. Removed unecessary class, Token (just used for storing the token string)
//10. Added decimal
//11. Added DateTime
//Todo
//1. Change the value delegate to accept parameters so functions can be passed.
//2. Change IOperand.Compatible to IOperand.ResultOprand(operand)
//3. Along with 2, allow morphable types, an int should become a long in a long operation.

namespace Dalldorf.Expressions
{
    public delegate object GetValueDelegate(object sender, string name);

	#region RPN

	/// <summary>
	/// Summary description for RPNParser.
	/// </summary>
	public class RpnParser
	{
        private ArrayList _compiledExpression;

        public object Evaluate(string expression)
        {
            Compile(expression);
            return EvaluateRpn(null, null);
        }

		public object Evaluate(string expression, object sender, GetValueDelegate getValueMethod)
		{
			Compile(expression);
			return EvaluateRpn(sender, getValueMethod);
		}

		#region RPN_Parser
		/// <summary>
		/// Algo of GetPostFixNotation (source : Expression Evaluator : using RPN by lallous 
		/// in the C++/MFC section of www.CodeProject.com.
		/// 1.	Initialize an empty stack (string stack), prepare input infix expression and clear RPN string 
		///	2.	Repeat until we reach end of infix expression 
		///		I.	Get token (operand or operator); skip white spaces 
		///		II.	If token is: 
		///			a.	Left parenthesis: Push it into stack 
		///			b.	Right parenthesis: Keep popping from the stack and appending to 
		///				RPN string until we reach the left parenthesis.
		///				If stack becomes empty and we didn't reach the left parenthesis 
		///				then break out with error "Unbalanced parenthesis" 
		///			c.	Operator: If stack is empty or operator has a higher precedence than 
		///				the top of the stack then push operator into stack. 
		///				Else if operator has lower precedence then we keep popping and 
		///				appending to RPN string, this is repeated until operator in stack 
		///				has lower precedence than the current operator. 
		///			d.	An operand: we simply append it to RPN string. 
		///		III.	When the infix expression is finished, we start popping off the stack and 
		///				appending to RPN string till stack becomes empty. 
		///
		/// </summary>
		/// <param name=expression></param>
		/// 		
		private void Compile(string expression)
		{
			Stack operatorStack = new Stack();
			_compiledExpression = new ArrayList();
			string result = "";

			Tokenizer tokenizer = new Tokenizer(expression);

			foreach(string token in tokenizer) {
				string strToken = token.Trim();
				if (strToken.Length == 0)
					continue;
				if (!OperatorHelper.IsOperator(strToken)) {
					_compiledExpression.Add(OperandHelper.CreateOperand(strToken));
					result += strToken;
					continue;
				}
				string strOperator = strToken;
				if (strOperator == "(") {
					operatorStack.Push(strOperator);
                }
				else if (strOperator == ")")
				{
					string szTop;
					while((szTop = (string)operatorStack.Pop()) != "(")
					{
						IOperator oprtr = OperatorHelper.CreateOperator(szTop);
						_compiledExpression.Add(oprtr);

						result += szTop;

						if (operatorStack.Count == 0)
							throw new RPN_Exception("Unmatched braces!");
					}
				}
				else
				{
					if (operatorStack.Count == 0 || (string)operatorStack.Peek() == "("
						|| OperatorHelper.IsHigherPrecOperator(strOperator, (string)operatorStack.Peek()))
					{
						operatorStack.Push(strOperator);
					}
					else
					{
						while(operatorStack.Count != 0)
						{
							if (OperatorHelper.IsLowerPrecOperator(strOperator, (string)operatorStack.Peek())
								|| OperatorHelper.IsEqualPrecOperator(strOperator, (string)operatorStack.Peek()))
							{
								string szTop = (string)operatorStack.Peek();
								if (szTop == "(")
									break;
								szTop = (string)operatorStack.Pop();

								IOperator oprtr = OperatorHelper.CreateOperator(szTop);
								_compiledExpression.Add(oprtr);

								result += szTop;
							}
							else
								break;
						}
						operatorStack.Push(strOperator);
					}
				}
			}
			while(operatorStack.Count != 0)
			{
				string szTop = (string)operatorStack.Pop();
				if (szTop == "(")
					throw new RPN_Exception("Unmatched braces");

				IOperator oprtr = OperatorHelper.CreateOperator(szTop);
				_compiledExpression.Add(oprtr);

				result += szTop;
			}
		}

		#endregion

		private string Convert2String(ArrayList arrExpr) 
		{
            StringBuilder builder = new StringBuilder();
            foreach (object obj in arrExpr)
                builder.Append(obj);
			return builder.ToString();
		}

		#region RPN_Evaluator

		/// <summary>
		/// Algo of EvaluateRPN (source : Expression Evaluator : using RPN by lallous 
		/// in the C++/MFC section of www.CodeProject.com.
		/// 1.	Initialize stack for storing results, prepare input postfix (or RPN) expression. 
		///	2.	Start scanning from left to right till we reach end of RPN expression 
		///	3.	Get token, if token is: 
		///		I.	An operator: 
		///			a.	Get top of stack and store into variable op2; Pop the stack 
		///			b.	Get top of stack and store into variable op1; Pop the stack 
		///			c.	Do the operation expression in operator on both op1 and op2
		///			d.	Push the result into the stack
		///		II.	An operand: stack its numerical representation into our numerical stack.
		///	4.	At the end of the RPN expression, the stack should only have one value and
		///	that should be the result and can be retrieved from the top of the stack.
		/// </summary>
		/// <param name=expression>Expression to be evaluated in RPNotation with
		/// single character variables</param>
		/// <param name=htValues>Values for each of the variables in the expression</param>
		///
		public object EvaluateRpn(object sender, GetValueDelegate getValueMethod) //todo - add delegate
		{
			// initialize stack (integer stack) for results
			Stack stPad = new Stack();
			// begin loop : scan from left to right till end of RPN expression
			foreach(object token in _compiledExpression)
			{
				IOperand op1 = null;
				IOperand op2 = null;
				IOperator oprtr = null;
				// Get token
				// if token is
				if (token is IOperand)
				{
					// Operand : push onto top of numerical stack
					stPad.Push(token);
				}
				else if (token is IOperator)
				{
					// Operator :
					//		Pop top of stack into token 1 - op2 first as top of stack is rhs
					op2 = (IOperand)stPad.Pop();
                    UpdateValue(op2, sender, getValueMethod);
                    //todo load variable value op2.value
					//		Pop top of stack into token 2
					op1 = (IOperand)stPad.Pop();
                    UpdateValue(op1, sender, getValueMethod);
					//		Do operation exp for 'this' operator on var1 and var2
					oprtr = (IOperator)token;
					IOperand opRes = oprtr.Eval(op1, op2);
					//		Push results onto stack
					stPad.Push(opRes);
				}
			}
			//	end loop
			// stack ends up with single value with final result
            IOperand op = ((IOperand)stPad.Pop());
            UpdateValue(op, sender, getValueMethod);
			return op.Value;
		}

        private void UpdateValue(IOperand op, object sender, GetValueDelegate getValueMethod)
        {
            IVariableOperand vop = (op as IVariableOperand);
            if (vop != null) {
                if (getValueMethod == null)
                    throw new RPN_Exception("GetValueMethod is required when using variables");
                op.Value = getValueMethod(sender, vop.Name);
            }
        }
		#endregion
	}
	#endregion

	#region UtilClasses
	/// <summary>
	/// Represents each token in the expression
	/// </summary>
	public class Token
	{
		public Token(string strValue)
		{
			_strValue = strValue;
		}
        
		public string Value
		{
			get
			{
				return _strValue;
			}
		}
		string _strValue;
	}
	/// <summary>
	/// Is the tokenizer which does the actual parsing of the expression.
	/// </summary>
	public class Tokenizer : IEnumerable
	{
		private string[] _tokens;
        private const string _splitExpression = "("
            + @"""[^""]*""" //string
            + @"|[0-9]{1,8}\.[0-9]{1,8}" //float (no exponenent)
            + @"|[0-9]{1,8}" //integer (eight digit integer)
            + @"|[a-zA-Z][a-zA-Z0-9._]*" //variable / function
            + @"|\(|\)" //parenthesis, these are included since they can be adjacent to operators
            + ")";

		public Tokenizer(string expression)
		{
			_tokens = Regex.Split(expression, _splitExpression);
		}

		public IEnumerator GetEnumerator()
		{
			return new TokenEnumerator(_tokens);
		}
	}

	/// <summary>
	/// Enumerator to enumerate over the tokens.
	/// </summary>
	public class TokenEnumerator : IEnumerator
	{
		string _token;
		int _cursor;
		string[] _tokens;

		public TokenEnumerator(string[] tokens)
		{
			_tokens = tokens;
			Reset();
		}

		public object Current
		{
			get	{ return _token; }
		}

		public bool MoveNext()
		{
			if (_cursor >= _tokens.Length)
				return false;
			_token = _tokens[_cursor++];
			if (_token.StartsWith("\"")) 
			{
				while ((_cursor < _tokens.Length-1) && (_tokens[_cursor].StartsWith("\"") || (_tokens[_cursor] == "")))
				{
					if (!(_tokens[_cursor] == "")) 
						_token += _tokens[_cursor].Remove(0, 1);
					_cursor++;
				}
			}
			return true;
		}

		public void Reset()
		{
			_cursor = 0;
		}
	}
	#region Exceptions
	/// <summary>
	/// For the exceptions thrown by this module.
	/// </summary>
	public class RPN_Exception : ApplicationException
	{
		public RPN_Exception()
		{
		}
		public RPN_Exception(string szMessage):base(szMessage)
		{
		}
		public RPN_Exception(string szMessage, Exception innerException):base(szMessage, innerException)
		{
		}
	}
	#endregion
	#endregion

	#region Interfaces

	public interface IOperand
    {
        object Value { get; set; }
        string ToString();
        void Parse(string s);
        bool Compatible(object value);
    }

    public interface IVariableOperand : IOperand
    {
        string Name { get; set; }
    }

	public interface IOperator
	{
		IOperand Eval(IOperand lhs, IOperand rhs);
	}

	public interface IArithmeticOperations
	{
		// to support {"+", "-", "*", "/", "%"} operators
		IOperand Plus(IOperand rhs);
		IOperand Minus(IOperand rhs);
		IOperand Multiply(IOperand rhs);
		IOperand Divide(IOperand rhs);
		IOperand Modulo(IOperand rhs);
	}

	public interface IComparisonOperations
	{
		// to support {"==", "!=","<", "<=", ">", ">="} operators
		IOperand EqualTo(IOperand rhs);
		IOperand NotEqualTo(IOperand rhs);
		IOperand LessThan(IOperand rhs);
		IOperand LessThanOrEqualTo(IOperand rhs);
		IOperand GreaterThan(IOperand rhs);
		IOperand GreaterThanOrEqualTo(IOperand rhs);
	}

	public interface ILogicalOperations
	{
		// to support {"||", "&&" } operators
		IOperand OR(IOperand rhs);
		IOperand AND(IOperand rhs);
	}

	#endregion

	#region Operands
	/// <summary>
	/// Base class for all Operands.  Provides datastorage
	/// </summary>
	public abstract class Operand : IOperand
	{
		public Operand(string strValue)
		{
			Parse(strValue);
		}

        public Operand()
        {
        }

		public abstract object Value { get; set; }

		public override string ToString()
		{
            if (Value == null)
                return null;
            else
			    return Value.ToString();
		}

		public abstract void Parse(string strValue);

        public abstract bool Compatible(object value);
	}
	/// <summary>
	/// Operand corresponding to the Long (Int32/Int64) datatypes.
	/// </summary>

    #region LongOperand
	public class LongOperand : Operand, IArithmeticOperations, IComparisonOperations
	{
        private long _value;
        
		public LongOperand(string strValue) : base(strValue)
		{
		}

        public LongOperand() : base()
        {
        }

		public override object Value
		{
			get { return _value; }
			set	{ _value = Convert.ToInt64(value); }
		}

		public override void Parse(string strValue)
		{
			_value = Convert.ToInt64(strValue);
		}

        public override bool Compatible(object value)
        {
            return ((value is Int32) || (value is Int64));
        }

		/// IArithmeticOperations methods.  Return of these methods is again a LongOperand
		public IOperand Plus(IOperand rhs)
		{
            CheckLongOperand(rhs, "+");
			IOperand result = new LongOperand();
			result.Value = (long)this.Value + (long)rhs.Value;
			return result;
		}

		public IOperand Minus(IOperand rhs)
		{
            CheckLongOperand(rhs, "-");
			LongOperand result = new LongOperand();
			result.Value = (long)this.Value - (long)rhs.Value;
			return result;
		}

		public IOperand Multiply(IOperand rhs)
		{
            CheckLongOperand(rhs, "*");
			LongOperand result = new LongOperand();
			result.Value = (long)this.Value * (long)rhs.Value;
			return result;
		}

		public IOperand Divide(IOperand rhs)
		{
            CheckLongOperand(rhs, "/");
			LongOperand result = new LongOperand();
			result.Value = (long)this.Value / (long)rhs.Value;
			return result;
		}

		public IOperand Modulo(IOperand rhs)
		{
            CheckLongOperand(rhs, "%");
			LongOperand result = new LongOperand();
			result.Value = (long)this.Value % (long)rhs.Value;
			return result;
		}

		/// IComparisonOperators methods.  Return values are always BooleanOperands type
		public IOperand EqualTo(IOperand rhs)
		{
            CheckLongOperand(rhs, "==");
			BoolOperand result = new BoolOperand();
			result.Value = (long)this.Value == (long)rhs.Value;
			return result;
		}

		public IOperand NotEqualTo(IOperand rhs)
		{
            CheckLongOperand(rhs, "!=");
			BoolOperand result = new BoolOperand();
			result.Value = ((long)this.Value != (long)rhs.Value);
			return result;
		}

		public IOperand LessThan(IOperand rhs)
		{
            CheckLongOperand(rhs, "<");
			BoolOperand result = new BoolOperand();
			result.Value = ((long)this.Value < (long)rhs.Value);
			return result;
		}

		public IOperand LessThanOrEqualTo(IOperand rhs)
		{
            CheckLongOperand(rhs, "<=");
			BoolOperand result = new BoolOperand();
			result.Value = ((long)this.Value <= (long)rhs.Value);
			return result;
		}

		public IOperand GreaterThan(IOperand rhs)
		{
            CheckLongOperand(rhs, ">");
			BoolOperand result = new BoolOperand();
			result.Value = ((long)this.Value > (long)rhs.Value);
			return result;
		}

		public IOperand GreaterThanOrEqualTo(IOperand rhs)
		{
            CheckLongOperand(rhs, ">=");
			BoolOperand result = new BoolOperand();
			result.Value = ((long)this.Value >= (long)rhs.Value);
			return result;
		}

        private void CheckLongOperand(IOperand operand, string strOperator)
        {
            if ((operand == null) || (!Compatible(operand.Value)))
    		    throw new RPN_Exception("Invalid LongOperand for '"+strOperator+"' operator.");
        }
	}
    #endregion

    #region DecimalOperand
	public class DecimalOperand : Operand, IArithmeticOperations, IComparisonOperations
	{
        private double _value;

		public DecimalOperand(string strValue) : base(strValue)
		{
		}

        public DecimalOperand() : base()
        {
        }

		public override object Value
		{
			get { return _value; }
			set	{ _value = Convert.ToDouble(value); }
		}

		public override void Parse(string strValue)
		{
			_value = Convert.ToDouble(strValue);
		}

        public override bool Compatible(object value)
        {
            return (value is double);
        }

		/// IArithmeticOperations methods.  Return of these methods is again aDecimalOperand
		public IOperand Plus(IOperand rhs)
		{
            CheckLongOperand(rhs, "+");
			IOperand result = new DecimalOperand();
			result.Value = (double)this.Value + (double)rhs.Value;
			return result;
		}

		public IOperand Minus(IOperand rhs)
		{
            CheckLongOperand(rhs, "-");
			DecimalOperand result = new DecimalOperand();
			result.Value = (double)this.Value - (double)rhs.Value;
			return result;
		}

		public IOperand Multiply(IOperand rhs)
		{
            CheckLongOperand(rhs, "*");
			DecimalOperand result = new DecimalOperand();
			result.Value = (double)this.Value * (double)rhs.Value;
			return result;
		}

		public IOperand Divide(IOperand rhs)
		{
            CheckLongOperand(rhs, "/");
			DecimalOperand result = new DecimalOperand();
			result.Value = (double)this.Value / (double)rhs.Value;
			return result;
		}

		public IOperand Modulo(IOperand rhs)
		{
            CheckLongOperand(rhs, "%");
			DecimalOperand result = new DecimalOperand();
			result.Value = (double)this.Value % (double)rhs.Value;
			return result;
		}

		/// IComparisonOperators methods.  Return values are always BooleanOperands type
		public IOperand EqualTo(IOperand rhs)
		{
            CheckLongOperand(rhs, "==");
			BoolOperand result = new BoolOperand();
			result.Value = (double)this.Value == (double)rhs.Value;
			return result;
		}

		public IOperand NotEqualTo(IOperand rhs)
		{
            CheckLongOperand(rhs, "!=");
			BoolOperand result = new BoolOperand();
			result.Value = ((double)this.Value != (double)rhs.Value);
			return result;
		}

		public IOperand LessThan(IOperand rhs)
		{
            CheckLongOperand(rhs, "<");
			BoolOperand result = new BoolOperand();
			result.Value = ((double)this.Value < (double)rhs.Value);
			return result;
		}

		public IOperand LessThanOrEqualTo(IOperand rhs)
		{
            CheckLongOperand(rhs, "<=");
			BoolOperand result = new BoolOperand();
			result.Value = ((double)this.Value <= (double)rhs.Value);
			return result;
		}

		public IOperand GreaterThan(IOperand rhs)
		{
            CheckLongOperand(rhs, ">");
			BoolOperand result = new BoolOperand();
			result.Value = ((double)this.Value > (double)rhs.Value);
			return result;
		}

		public IOperand GreaterThanOrEqualTo(IOperand rhs)
		{
            CheckLongOperand(rhs, ">=");
			BoolOperand result = new BoolOperand();
			result.Value = ((double)this.Value >= (double)rhs.Value);
			return result;
		}

        private void CheckLongOperand(IOperand operand, string strOperator)
        {
            if ((operand == null) || (!Compatible(operand.Value)))
    		    throw new RPN_Exception("Invalid LongOperand for '"+strOperator+"' operator.");
        }
	}
    #endregion

    #region NullOperand
    public class NullOperand : Operand, IComparisonOperations
    {
        object _value;

        public override object Value
        {
            get { return _value; }
            set {
                if (value is System.DBNull)
                    _value = null;
                else
                    _value = value;
            }
        }

		public override void Parse(string strValue)
		{
            if (strValue.CompareTo("null") == 0)
                _value = null;
            else
                throw new RPN_Exception("NullOperand unable to parse, "+strValue);
        }

        public override bool Compatible(object value)
        {
            return true;
        }

		/// IComparisonOperators methods.  Return values are always BooleanOperands type
		public IOperand EqualTo(IOperand rhs)
		{
            BoolOperand result = new BoolOperand();
            result.Value = (rhs.Value == _value);
            return result;
		}

		public IOperand NotEqualTo(IOperand rhs)
		{
            BoolOperand result = new BoolOperand();
            result.Value = (rhs.Value != _value);
            return result;
		}

		public IOperand LessThan(IOperand rhs)
		{
            InvalidOperator("<");
            return null;
		}

		public IOperand LessThanOrEqualTo(IOperand rhs)
		{
            InvalidOperator("<=");
            return null;
		}

		public IOperand GreaterThan(IOperand rhs)
		{
            InvalidOperator(">");
            return null;
		}

		public IOperand GreaterThanOrEqualTo(IOperand rhs)
		{
            InvalidOperator(">=");
            return null;
		}

        private void InvalidOperator(string strOperator)
        {
            if (_value == null)
                throw new RPN_Exception(strOperator + " not applicable on a null operand.");
            else
                throw new RPN_Exception(strOperator + " not applicable on unknown type, "+_value.GetType().ToString());
        }
    }
    #endregion

    #region BoolOperand
	public class BoolOperand : Operand, ILogicalOperations, IComparisonOperations
	{
        private bool _value;

		public BoolOperand(string strValue) : base(strValue)
		{
		}

        public BoolOperand() : base()
        {
        }

		public override object Value
		{
			get { return _value; }
			set	{ _value = Convert.ToBoolean(value); }
		}

		public override string ToString()
		{
            if (Value == null)
                return null;
            else
			    return Value.ToString();
		}

		public override void Parse(string strValue)
		{
			_value = Convert.ToBoolean(strValue);
		}

        public override bool Compatible(object value)
        {
            return (value is bool);
        }

		public IOperand AND(IOperand rhs)
		{
			if (!(rhs is BoolOperand))
				throw new RPN_Exception("Argument invalid in BoolOperand.&& : rhs");
			BoolOperand result = new BoolOperand();
			result.Value = ((bool)this.Value && (bool)rhs.Value);
			return result;
		}

		public IOperand OR(IOperand rhs)
		{
			if (!(rhs is BoolOperand))
				throw new RPN_Exception("Argument invalid in BoolOperand.|| : rhs");
			BoolOperand result = new BoolOperand();
			result.Value = ((bool)this.Value || (bool)rhs.Value);
			return result;
		}

		/// IComparisonOperators methods.  Return values are always BooleanOperands type
		public IOperand EqualTo(IOperand rhs)
		{
            CheckBoolOperand(rhs, "==");
			BoolOperand result = new BoolOperand();
			result.Value = (bool)this.Value == (bool)rhs.Value;
			return result;
		}

		public IOperand NotEqualTo(IOperand rhs)
		{
            CheckBoolOperand(rhs, "!=");
			BoolOperand result = new BoolOperand();
			result.Value = ((bool)this.Value != (bool)rhs.Value);
			return result;
		}

		public IOperand LessThan(IOperand rhs)
		{
            InvalidOperator("<");
			return null;
		}

		public IOperand LessThanOrEqualTo(IOperand rhs)
		{
            InvalidOperator("<=");
			return null;
		}

		public IOperand GreaterThan(IOperand rhs)
		{
            InvalidOperator(">");
			return null;
		}

		public IOperand GreaterThanOrEqualTo(IOperand rhs)
		{
            InvalidOperator(">=");
			return null;
		}

        private void CheckBoolOperand(IOperand operand, string strOperator)
        {
            if ((operand == null) || (!Compatible(operand.Value)))
    		    throw new RPN_Exception("Invalid LongOperand for '"+strOperator+"' operator.");
        }

        private void InvalidOperator(string strOperator)
        {
            throw new RPN_Exception(strOperator + " not applicable on a Boolean operand.");
        }
	}
    #endregion

    #region StringOperand
	public class StringOperand : Operand, IArithmeticOperations, IComparisonOperations
	{
        private string _value;

		public StringOperand(string strValue) : base(strValue)
		{
		}

        public StringOperand() : base()
        {
        }

		public override object Value
		{
			get { return _value; }
			set	{
                if (value == null)
                    _value = null;
                else
                    _value = value.ToString();
            }
		}

		public override string ToString()
		{
            if (Value == null)
                return null;
            else
			    return Value.ToString();
		}

		public override void Parse(string strValue)
		{
			_value = strValue;
		}

        public override bool Compatible(object value)
        {
            return true;
        }

		public IOperand Plus(IOperand rhs)
		{
			StringOperand result = new StringOperand();
			result.Value = this.ToString() + rhs.ToString();
			return result;
		}

		public IOperand Minus(IOperand rhs)
		{
            throw new RPN_Exception("Operation not supported: StringOperand.Minus");
		}

		public IOperand Multiply(IOperand rhs)
		{
            throw new RPN_Exception("Operation not supported: StringOperand.Multiply");
		}

		public IOperand Divide(IOperand rhs)
		{
            throw new RPN_Exception("Operation not supported: StringOperand.Divide");
		}

		public IOperand Modulo(IOperand rhs)
		{
            throw new RPN_Exception("Operation not supported: StringOperand.Modulo");
		}

		/// IComparisonOperators methods.  Return values are always BooleanOperands type
		public IOperand EqualTo(IOperand rhs)
		{
			BoolOperand result = new BoolOperand();
            if ((this.Value == null) && (rhs.Value == null))
                result.Value = true;
            else if ((this.Value == null) || (rhs.Value == null))
                result.Value = false;
            else
                result.Value = (((string)Value).CompareTo((string)(rhs.Value)) == 0);
			return result;
		}

		public IOperand NotEqualTo(IOperand rhs)
		{
            IOperand result = EqualTo(rhs);
            result.Value = !(bool)result.Value;
            return result; 
		}

		public IOperand LessThan(IOperand rhs)
		{
			BoolOperand result = new BoolOperand();
            result.Value = (String.Compare((string)this.Value, (string)rhs.Value, true) < 0);
            return result;
		}

		public IOperand LessThanOrEqualTo(IOperand rhs)
		{
			BoolOperand result = new BoolOperand();
            result.Value = (String.Compare((string)this.Value, (string)rhs.Value, true) <= 0);
            return result;
		}

		public IOperand GreaterThan(IOperand rhs)
		{
			BoolOperand result = new BoolOperand();
            result.Value = (String.Compare((string)this.Value, (string)rhs.Value, true) > 0);
            return result;
		}

		public IOperand GreaterThanOrEqualTo(IOperand rhs)
		{
			BoolOperand result = new BoolOperand();
            result.Value = (String.Compare((string)this.Value, (string)rhs.Value, true) >= 0);
            return result;
		}
	}
    #endregion

    #region DateTimeOperand
    public class DateTimeOperand : Operand, IComparisonOperations
    {
        private DateTime _value;
        private bool _isNull;

		public DateTimeOperand(string strValue) : base(strValue)
		{
		}

        public DateTimeOperand() : base()
        {
        }

		public override object Value
		{
			get {
                if (_isNull)
                    return null;
                return _value;
            }
			set	{
                _isNull = ((value == null) || Convert.IsDBNull(value));
                if (!_isNull)
                    _value = Convert.ToDateTime(value);
            }
		}

		public override string ToString()
		{
            if (Value == null)
                return null;
            else
			    return Value.ToString();
		}

		public override void Parse(string strValue)
		{
            _isNull = ((strValue == null) || (strValue == ""));
            if (!_isNull) 
			    _value = Convert.ToDateTime(strValue);
		}

        public override bool Compatible(object value)
        {
            return (value == null) || (value is DateTime);
        }

		/// IComparisonOperators methods.  Return values are always BooleanOperands type
		public IOperand EqualTo(IOperand rhs)
		{
            CheckDateTimeOperand(rhs, "==");
			BoolOperand result = new BoolOperand();
            if ((this.Value == null) && (rhs.Value == null))
                result.Value = true;
            else if ((this.Value == null) || (rhs.Value == null))
                result.Value = false;
            else
    			result.Value = (DateTime)this.Value == (DateTime)rhs.Value;
			return result;
		}

		public IOperand NotEqualTo(IOperand rhs)
		{
            CheckDateTimeOperand(rhs, "!=");
			BoolOperand result = new BoolOperand();
            if ((this.Value == null) && (rhs.Value == null))
                result.Value = false;
            else if ((this.Value == null) || (rhs.Value == null))
                result.Value = true;
            else
    			result.Value = ((DateTime)this.Value != (DateTime)rhs.Value);
			return result;
		}

		public IOperand LessThan(IOperand rhs)
		{
            CheckDateTimeOperand(rhs, "<");
			BoolOperand result = new BoolOperand();
            if ((this.Value == null) && (rhs.Value == null))
                result.Value = false;
            else if (this.Value == null)
                result.Value = true;
            else if (rhs.Value == null)
                result.Value = false;
            else
    			result.Value = ((DateTime)this.Value < (DateTime)rhs.Value);
			return result;
		}

		public IOperand LessThanOrEqualTo(IOperand rhs)
		{
            CheckDateTimeOperand(rhs, "<=");
			BoolOperand result = new BoolOperand();
            if ((this.Value == null) && (rhs.Value == null))
                result.Value = true;
            else if (this.Value == null)
                result.Value = true;
            else if (rhs.Value == null)
                result.Value = false;
            else
    			result.Value = ((DateTime)this.Value <= (DateTime)rhs.Value);
			return result;
		}

		public IOperand GreaterThan(IOperand rhs)
		{
            CheckDateTimeOperand(rhs, ">");
			BoolOperand result = new BoolOperand();
            if ((this.Value == null) && (rhs.Value == null))
                result.Value = false;
            else if (this.Value == null)
                result.Value = false;
            else if (rhs.Value == null)
                result.Value = true;
            else
    			result.Value = ((DateTime)this.Value > (DateTime)rhs.Value);
			return result;
		}

		public IOperand GreaterThanOrEqualTo(IOperand rhs)
		{
            CheckDateTimeOperand(rhs, ">=");
			BoolOperand result = new BoolOperand();
            if ((this.Value == null) && (rhs.Value == null))
                result.Value = true;
            else if (this.Value == null)
                result.Value = false;
            else if (rhs.Value == null)
                result.Value = true;
            else
    			result.Value = ((DateTime)this.Value >= (DateTime)rhs.Value);
			return result;
		}

        private void CheckDateTimeOperand(IOperand operand, string strOperator)
        {
            if ((operand == null) || (!Compatible(operand.Value)))
    		    throw new RPN_Exception("Invalid DateTimeOperand for '"+strOperator+"' operator.");
        }
    }
    #endregion

    #region VariableOperand
    public class VariableOperand : IVariableOperand, IArithmeticOperations, IComparisonOperations
    {
        private IOperand _valueOperand;
        private string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

		public object Value
		{
			get {
                if (_valueOperand == null)
                    throw new RPN_Exception("Variable value '"+_name+"' not set");
                return _valueOperand.Value;
            }
			set	{
                if (_valueOperand == null)
                    _valueOperand = OperandHelper.CreateOperandFromValue(value);
                else
                    _valueOperand.Value = value;
            }
		}

		public VariableOperand(string name) : base()
		{
            _name = name;
		}

		public override string ToString()
		{
            if (Value == null)
                return "";
            else
                return Value.ToString();
			//return _name;
		}

		public void Parse(string strValue)
		{
            if (_valueOperand == null)
                _valueOperand = OperandHelper.CreateOperand(strValue);
            else
                _valueOperand.Parse(strValue);
		}

        public bool Compatible(object value)
        {
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            return _valueOperand.Compatible(value);
        }

		/// IArithmeticOperations methods.  Return of these methods is again a LongOperand
		public IOperand Plus(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IArithmeticOperations))
                throw new RPN_Exception("Arithmetic operations not supported on '"+_name+"'.");
            return ((IArithmeticOperations)_valueOperand).Plus(rhs);
		}

		public IOperand Minus(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IArithmeticOperations))
                throw new RPN_Exception("Arithmetic operations not supported on '"+_name+"'.");
            return ((IArithmeticOperations)_valueOperand).Minus(rhs);
		}

		public IOperand Multiply(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IArithmeticOperations))
                throw new RPN_Exception("Arithmetic operations not supported on '"+_name+"'.");
            return ((IArithmeticOperations)_valueOperand).Multiply(rhs);
		}

		public IOperand Divide(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IArithmeticOperations))
                throw new RPN_Exception("Arithmetic operations not supported on '"+_name+"'.");
            return ((IArithmeticOperations)_valueOperand).Divide(rhs);
		}

		public IOperand Modulo(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IArithmeticOperations))
                throw new RPN_Exception("Arithmetic operations not supported on '"+_name+"'.");
            return ((IArithmeticOperations)_valueOperand).Modulo(rhs);
		}

		/// IComparisonOperators methods.  Return values are always BooleanOperands type
		public IOperand EqualTo(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IComparisonOperations))
                throw new RPN_Exception("Comparison operations not supported on '"+_name+"'.");
            return ((IComparisonOperations)_valueOperand).EqualTo(rhs);
		}

		public IOperand NotEqualTo(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IComparisonOperations))
                throw new RPN_Exception("Comparison operations not supported on '"+_name+"'.");
            return ((IComparisonOperations)_valueOperand).NotEqualTo(rhs);
		}

		public IOperand LessThan(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IComparisonOperations))
                throw new RPN_Exception("Comparison operations not supported on '"+_name+"'.");
            return ((IComparisonOperations)_valueOperand).LessThan(rhs);
		}

		public IOperand LessThanOrEqualTo(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IComparisonOperations))
                throw new RPN_Exception("Comparison operations not supported on '"+_name+"'.");
            return ((IComparisonOperations)_valueOperand).LessThanOrEqualTo(rhs);
		}

		public IOperand GreaterThan(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IComparisonOperations))
                throw new RPN_Exception("Comparison operations not supported on '"+_name+"'.");
            return ((IComparisonOperations)_valueOperand).GreaterThan(rhs);
		}

		public IOperand GreaterThanOrEqualTo(IOperand rhs)
		{
            if (_valueOperand == null)
                throw new RPN_Exception("Variable value '"+_name+"' not set");
            if (!(_valueOperand is IComparisonOperations))
                throw new RPN_Exception("Comparison operations not supported on '"+_name+"'.");
            return ((IComparisonOperations)_valueOperand).GreaterThanOrEqualTo(rhs);
		}
    }
    #endregion

	public class OperandHelper
	{
		/// <summary>
		/// Factory method to create corresponding Operands.
		/// Extended this method to create newer datatypes.
		/// </summary>
		/// <param name="name"></param>
		/// <param name="varType"></param>
		/// <param name="varValue"></param>
		/// <returns></returns>
        private static Regex _number;
        private static Regex _float;
        private static Regex _variable;

		static public IOperand CreateOperand(string strValue)
		{
            //No date literals - these should be created by function
            if ((strValue.CompareTo("true") == 0) || (strValue.CompareTo("yes") == 0))
                return new BoolOperand(strValue);
            if ((strValue.CompareTo("false") == 0) || (strValue.CompareTo("no") == 0))
                return new BoolOperand(strValue);
            if (strValue.CompareTo("null") == 0)
                return new NullOperand();
            if (IsStringLiteral(strValue))
                return new StringOperand(strValue.Substring(1, strValue.Length-2));
			if (IsFloatLiteral(strValue))
				return new DecimalOperand(strValue);
			if (IsNumberLiteral(strValue))
                return new LongOperand(strValue);
            if (IsVariable(strValue))
                return new VariableOperand(strValue);
            throw new RPN_Exception("Invalid identifer, '" + strValue + "'.  Expected: variable name, string literal (quoted), integer literal or float (expontent allowed).");
		}

        static public IOperand CreateOperandFromValue(object value)
        {
            IOperand result = null;
            if (value is string)
                result = new StringOperand();
            else if (value is Boolean)
                result = new BoolOperand();
            else if ((value is Int32) || (value is Int64))
                result = new LongOperand();
            else if (value is DateTime)
                result = new DateTimeOperand();
            else if ((value is decimal) || (value is float) || (value is double))
                result = new DecimalOperand();
            else
                result = new NullOperand();
            result.Value = value;
            return result;
        }

        static private bool IsStringLiteral(string strValue) //todo - improve this routine
        {
            if (strValue.StartsWith("\"")) {
                if (!strValue.EndsWith("\""))
			        throw new RPN_Exception("Invalid string literal: " + strValue);
                return true;
            }
            return false;
        }

        static private bool IsFloatLiteral(string strValue)
        {
            if (_float == null)
                _float = new Regex(@"^[0-9]{1,8}\.[0-9]{1,8}$");
            return _float.IsMatch(strValue);
        }

        static private bool IsNumberLiteral(string strValue)
        {
            if (_number == null)
                _number = new Regex(@"^[0-9]{1,8}$");
            return _number.IsMatch(strValue);
        }

        static private bool IsVariable(string strValue)
        {
            if (_variable == null)
                _variable = new Regex(@"^[a-zA-Z][a-zA-Z0-9._]*$");
            return _variable.IsMatch(strValue);
        }
	}
	#endregion

	#region Operators
	/// <summary>
	/// Base class of all operators.  Provides datastorage
	/// </summary>
	public abstract class Operator : IOperator
	{
		protected string _szOperator = "";
        
		public Operator(char cOperator)
		{
			_szOperator = new String(cOperator, 1);
		}

		public Operator(string strOperator)
		{
			_szOperator = strOperator;
		}

		public override string ToString()
		{
			return _szOperator;
		}

		public abstract IOperand Eval(IOperand lhs, IOperand rhs);

		public string Value
		{
			get	{ return _szOperator; }
			set	{ _szOperator = value; }
		}
	}
	/// <summary>
	/// Arithmetic Operator Class providing evaluation services for "+-/*%" operators.
	/// </summary>
	public class ArithmeticOperator : Operator
	{
		public ArithmeticOperator(char cOperator):base(cOperator)
		{
		}
		public ArithmeticOperator(string strOperator):base(strOperator)
		{
		}
		//bool bBinaryOperator = true;

		public override IOperand Eval(IOperand lhs, IOperand rhs)
		{
			if (!(lhs is IArithmeticOperations))
				throw new RPN_Exception("Argument invalid in ArithmeticOperator.Eval - Invalid Expression : lhs");
			switch(_szOperator)
			{
				case "+":
					return ((IArithmeticOperations)lhs).Plus(rhs);
				case "-":
					return ((IArithmeticOperations)lhs).Minus(rhs);
				case "*":
					return ((IArithmeticOperations)lhs).Multiply(rhs);
				case "/":
					return ((IArithmeticOperations)lhs).Divide(rhs);
				case "%":
					return ((IArithmeticOperations)lhs).Modulo(rhs);
			}
			throw new RPN_Exception("Unsupported Arithmetic operation " + _szOperator);
		}
	}
	/// <summary>
	/// Comparison Operator Class providing evaluation services for "==", "!=","<", "<=", ">", ">=" operators.
	/// </summary>
	public class ComparisonOperator : Operator
	{
		public ComparisonOperator(char cOperator):base(cOperator)
		{
		}
		public ComparisonOperator(string strOperator):base(strOperator)
		{
		}
		//bool bBinaryOperator = true;

		//{"==", "!=","<", "<=", ">", ">="}
		public override IOperand Eval(IOperand lhs, IOperand rhs)
		{
			if (!(lhs is IComparisonOperations))
				throw new RPN_Exception("Argument invalid in ComparisonOperator.Eval - Invalid Expression : lhs");
			switch(_szOperator)
			{
				case "==":
                case "=":
					return ((IComparisonOperations)lhs).EqualTo(rhs);
				case "!=":
                case "<>":
					return ((IComparisonOperations)lhs).NotEqualTo(rhs);
				case "<":
					return ((IComparisonOperations)lhs).LessThan(rhs);
				case "<=":
					return ((IComparisonOperations)lhs).LessThanOrEqualTo(rhs);
				case ">":
					return ((IComparisonOperations)lhs).GreaterThan(rhs);
				case ">=":
					return ((IComparisonOperations)lhs).GreaterThanOrEqualTo(rhs);
			}
			throw new RPN_Exception("Unsupported Comparison operation " + _szOperator);
		}
	}

	/// <summary>
	/// Logical Operator Class providing evaluation services for && and || operators.
	/// </summary>
	public class LogicalOperator : Operator
	{
		public LogicalOperator(char cOperator):base(cOperator)
		{
		}

		public LogicalOperator(string strOperator):base(strOperator)
		{
		}

		public override IOperand Eval(IOperand lhs, IOperand rhs)
		{
			if (!(lhs is ILogicalOperations))
				throw new RPN_Exception("Argument invalid in LogicalOperator.Eval - Invalid Expression : lhs");
			switch(_szOperator.ToLower())
			{
				case "&&":
                case "and":
					return ((ILogicalOperations)lhs).AND(rhs);
				case "||":
                case "or":
					return ((ILogicalOperations)lhs).OR(rhs);
			}
			throw new RPN_Exception("Unsupported Logical operation " + _szOperator);
		}
	}

	public class OperatorHelper
	{
		/// <summary>
		/// Factory method to create Operator objects.
		/// </summary>
		/// <param name="strOperator"></param>
		/// <returns></returns>
		static public IOperator CreateOperator(string strOperator)
		{
			if (OperatorHelper.IsArithmeticOperator(strOperator))
				return new ArithmeticOperator(strOperator);
			if (OperatorHelper.IsComparisonOperator(strOperator))
				return new ComparisonOperator(strOperator);
			if (OperatorHelper.IsLogicalOperator(strOperator))
				return new LogicalOperator(strOperator);
			throw new RPN_Exception("Unhandled Operator : " + strOperator);
		}

		static public IOperator CreateOperator(char cOperator)
		{
			return CreateOperator(new string(cOperator, 1));
		}

		/// Some helper functions.
		public static bool IsOperator(string currentOp)
		{
			return (Array.IndexOf(_AllOps, currentOp.Trim().ToLower()) >= 0);
		}

		public static bool IsArithmeticOperator(string currentOp)
		{
			return (Array.IndexOf(_AllArithmeticOps, currentOp.ToLower()) >= 0);
		}

		public static bool IsComparisonOperator(string currentOp)
		{
			return (Array.IndexOf(_AllComparisonOps, currentOp.ToLower()) >= 0);
		}

		public static bool IsLogicalOperator(string currentOp)
		{
			return (Array.IndexOf(_AllLogicalOps, currentOp.ToLower()) >= 0);
		}

		#region Precedence
		/// Precedence is determined by relative indices of the operators defined in
		/// in _AllOps variable

		/// <summary>
		/// Summary of IsLowerPrecOperator.
		/// </summary>
		/// <param name=allOps></param>
		/// <param name=currentOp></param>
		/// <param name=prevOp></param>
		/// 		
		public static bool IsLowerPrecOperator(string currentOp, string prevOp)
		{
			int nCurrIdx;
			int nPrevIdx;
			GetCurrentAndPreviousIndex(_AllOps, currentOp, prevOp, out nCurrIdx, out nPrevIdx);
			return (nCurrIdx < nPrevIdx);
		}

		/// <summary>
		/// Summary of IsHigherPrecOperator.
		/// </summary>
		/// <param name=allOps></param>
		/// <param name=currentOp></param>
		/// <param name=prevOp></param>
		///
		public static bool IsHigherPrecOperator(string currentOp, string prevOp)
		{
			int nCurrIdx;
			int nPrevIdx;
			GetCurrentAndPreviousIndex(_AllOps, currentOp, prevOp, out nCurrIdx, out nPrevIdx);
			return (nCurrIdx > nPrevIdx);
		}

		/// <summary>
		/// Summary of IsEqualPrecOperator.
		/// </summary>
		/// <param name=allOps></param>
		/// <param name=currentOp></param>
		/// <param name=prevOp></param>
		///
		public static bool IsEqualPrecOperator(string currentOp, string prevOp)
		{
			int nCurrIdx;
			int nPrevIdx;
			GetCurrentAndPreviousIndex(_AllOps, currentOp, prevOp, out nCurrIdx, out nPrevIdx);
			return (nCurrIdx == nPrevIdx);
		}

		/// <summary>
		/// Summary of GetCurrentAndPreviousIndex.
		/// </summary>
		/// <param name=allOps></param>
		/// <param name=currentOp></param>
		/// <param name=prevOp></param>
		/// <param name=nCurrIdx></param>
		/// <param name=nPrevIdx></param>
		///
		private static void GetCurrentAndPreviousIndex(string[] allOps, string currentOp, string prevOp,
			out int nCurrIdx, out int nPrevIdx)
		{
			nCurrIdx = -1;
			nPrevIdx = -1;
			for (int nIdx=0;nIdx < allOps.Length;nIdx++) {
				if (allOps[nIdx] == currentOp)
					nCurrIdx = nIdx;
				if (allOps[nIdx] == prevOp)
					nPrevIdx = nIdx;
				if (nPrevIdx != -1 && nCurrIdx != -1)
					break;
			}
			if (nCurrIdx == -1)
				throw new RPN_Exception("Unknown operator - " + currentOp);
			if (nPrevIdx == -1)
				throw new RPN_Exception("Unknown operator - " + prevOp);
		}
		#endregion

		public static string[] AllOperators
		{
			get	{ return _AllOps; }
		}

		/// <summary>
		/// All Operators supported by this module currently.
		/// Modify here to add more operators IN ACCORDANCE WITH their precedence.
		/// Additionally add into individual variables to support some helper methods above.
		/// </summary>
		static string[] _AllOps = { "||", "&&", "|", "^", "&", "==", "!=",
									   "<", "<=", ">", ">=", "+", "-", "*", "/", "%", "(", ")", ",", "or", "and", "=", "<>" };
		static string[] _AllArithmeticOps = { "+", "-", "*", "/", "%" };
		static string[] _AllComparisonOps = { "==", "!=","<", "<=", ">", ">=", "=", "<>" };
		static string[] _AllLogicalOps = { "&&", "||" , "or", "and" };
        static string[] _AllCommaOps = { "," };
	}

	#endregion

}

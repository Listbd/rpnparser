using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using Dalldorf.Expressions;

namespace Dalldorf.Expressions.UnitTests
{
    [TestFixture]
    public class IntegerTests
    {
        [Test]
        public void IntegerAdd()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(2, rpn.Evaluate("1+1"));
            Assert.AreEqual(3000, rpn.Evaluate("1000+2000"));
            Assert.AreEqual(21, rpn.Evaluate("1+2+3+4+5+6"));
        }

        [Test]
        public void IntegerAddNegative()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(1, rpn.Evaluate("-1+2"));
            Assert.AreEqual(-3, rpn.Evaluate("-1+-2"));
        }

        [Test]
        public void IntegerSub()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(0, rpn.Evaluate("1-1"));
            Assert.AreEqual(1000, rpn.Evaluate("2000-1000"));
        }

        [Test]
        public void IntegerSubNegative()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(-21, rpn.Evaluate("-6-5-4-3-2-1"));
            Assert.AreEqual(-3, rpn.Evaluate("-1-2"));
        }

        [Test]
        public void IntegerMultiply()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(6, rpn.Evaluate("2*3"));
            Assert.AreEqual(3000, rpn.Evaluate("50*60"));
        }

        [Test]
        public void IntegerDivide()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(3, rpn.Evaluate("6/2"));
            Assert.AreEqual(2, rpn.Evaluate("5/2"));
        }

        [Test]
        public void IntegerEqual()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(true, rpn.Evaluate("4=4"));
            Assert.AreEqual(false, rpn.Evaluate("5=4"));
            Assert.AreEqual(true, rpn.Evaluate("4==4"));
            Assert.AreEqual(false, rpn.Evaluate("5==4"));
            Assert.AreEqual(false, rpn.Evaluate("3+2==2+3"));
        }

        [Test]
        public void IntegerGreaterThan()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(true, rpn.Evaluate("4>2"));
            Assert.AreEqual(false, rpn.Evaluate("100>200"));
            Assert.AreEqual(true, rpn.Evaluate("100>=100"));
            Assert.AreEqual(false, rpn.Evaluate("100>=99"));
            Assert.AreEqual(true, rpn.Evaluate("10+10>10-5"));
        }

        [Test]
        public void IntegerLessThan()
        {
            RpnParser rpn = new RpnParser();
            Assert.AreEqual(false, rpn.Evaluate("4<2"));
            Assert.AreEqual(true, rpn.Evaluate("100<200"));
            Assert.AreEqual(false, rpn.Evaluate("100<=100"));
            Assert.AreEqual(true, rpn.Evaluate("100<=99"));
            Assert.AreEqual(false, rpn.Evaluate("10+10<10-5"));
        }
    }
}

package tlc2.overrides;
/*******************************************************************************
 * Copyright (c) 2019 Microsoft Research. All rights reserved. 
 *
 * The MIT License (MIT)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy 
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software. 
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Contributors:
 *   Markus Alexander Kuppe - initial API and implementation
 ******************************************************************************/
import tlc2.value.impl.IntValue;

public class Bitwise {

	@TLAPlusOperator(identifier = "And", module = "Bitwise", warn = false)
    public static IntValue And(IntValue x, IntValue y, IntValue n, IntValue m) {
        return IntValue.gen(x.val & y.val);
    }
	
	@TLAPlusOperator(identifier = "Or", module = "Bitwise", warn = false)
    public static IntValue Or(IntValue x, IntValue y, IntValue n, IntValue m) {
        return IntValue.gen(x.val | y.val);
    }

	@TLAPlusOperator(identifier = "Xor", module = "Bitwise", warn = false)
    public static IntValue Xor(IntValue x, IntValue y, IntValue n, IntValue m) {
        return IntValue.gen(x.val ^ y.val);
    }

	@TLAPlusOperator(identifier = "Not", module = "Bitwise", warn = false)
    public static IntValue NotR(IntValue x) {
        // The TLA+ definition of  Bitwise!Not  has no type to work with and, thus,
        // only flips the bits in (Integer.numberOfLeadingZeros(x.val), 0].  Thus,
        // correct Java's not (~) flipping the upper bits of an 32 bit integer
        // by and'ing mask.  
        final int mask = Integer.MAX_VALUE >> Integer.numberOfLeadingZeros(x.val) - 1;
        final int tmp = ~x.val & mask;
        return IntValue.gen(tmp);
    }

	@TLAPlusOperator(identifier = "shiftR", module = "Bitwise", warn = false)
	public static IntValue shiftR(final IntValue n, final IntValue pos) {
		return IntValue.gen(n.val >>> pos.val);
	}
}

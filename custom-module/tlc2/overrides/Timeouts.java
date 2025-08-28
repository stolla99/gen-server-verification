package tlc2.overrides;

import tlc2.value.impl.BoolValue;
import tlc2.value.impl.IntValue;
import tlc2.value.impl.StringValue;
import tlc2.value.impl.Value;

import java.util.Arrays;

public class Timeouts {
    private static final String[] TIMEOUTS = new String[] { "infinity", "never", "inf", "forever" };

	@TLAPlusOperator(identifier = "isReached", module = "Timeouts", warn = false)
    public static Value isReached(Value elem) {
        if (elem instanceof IntValue) {
            IntValue intElem = (IntValue) elem;
            return intElem.val <= 0
                    ? BoolValue.ValTrue
                    : BoolValue.ValFalse;
        } else if (elem instanceof StringValue) {
            StringValue stringElem = (StringValue) elem;
            return Arrays.asList(TIMEOUTS).contains(stringElem.toUnquotedString())
                    ? BoolValue.ValFalse
                    : BoolValue.ValTrue;
        } else {
            return IntValue.gen(-1);
        }
    }

    @TLAPlusOperator(identifier = "tryDecrease", module = "Timeouts", warn = false)
    public static Value tryDecrease(Value elem) {
        if (elem instanceof IntValue) {
            IntValue intElem = (IntValue) elem;
            if (intElem.val <= 0) {
                return IntValue.gen(0);
            } else {
                return IntValue.gen(intElem.val - 1);
            }
        } else {
            return elem;
        }
    }
}

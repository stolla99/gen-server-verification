package tlc2.overrides;

import tlc2.value.impl.BoolValue;
import tlc2.value.impl.RecordValue;
import tlc2.value.impl.Value;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;

public class Typecheck {
	@TLAPlusOperator(identifier = "isRecord", module = "Typecheck", warn = false)
    public static BoolValue isRecord(Value elem) {
        return (elem instanceof RecordValue) ? BoolValue.ValTrue : BoolValue.ValFalse;
    }

    @TLAPlusOperator(identifier = "isBool", module = "Typecheck", warn = false)
    public static BoolValue isBool(Value elem) {
        return (elem instanceof BoolValue) ? BoolValue.ValTrue : BoolValue.ValFalse;
    }

    @TLAPlusOperator(identifier = "isInt", module = "Typecheck", warn = false)
    public static BoolValue isInt(Value elem) {
        return (elem instanceof tlc2.value.impl.IntValue) ? BoolValue.ValTrue : BoolValue.ValFalse;
    }

    @TLAPlusOperator(identifier = "isString", module = "Typecheck", warn = false)
    public static BoolValue isString(Value elem) {
        return (elem instanceof tlc2.value.impl.StringValue) ? BoolValue.ValTrue : BoolValue.ValFalse;
    }

    @TLAPlusOperator(identifier = "isModelValue", module = "Typecheck", warn = false)
    public static BoolValue isModelValue(Value elem) {
        return (elem instanceof tlc2.value.impl.ModelValue) ? BoolValue.ValTrue : BoolValue.ValFalse;
    }

    @TLAPlusOperator(identifier = "isSet", module = "Typecheck", warn = false)
    public static BoolValue isSet(Value elem) {
        return (elem instanceof tlc2.value.impl.SetEnumValue) ? BoolValue.ValTrue : BoolValue.ValFalse;
    }

    @TLAPlusOperator(identifier = "isSeq", module = "Typecheck", warn = false)
    public static BoolValue isSeq(Value elem) {
        return (elem instanceof tlc2.value.impl.TupleValue) ? BoolValue.ValTrue : BoolValue.ValFalse;
    }

    @TLAPlusOperator(identifier = "looseEquality", module = "Typecheck", warn = false)
    public static BoolValue looseEquality(Value elem1, Value elem2) {
        List<Function<Value, BoolValue>> functions = List.of(
                Typecheck::isRecord,
                Typecheck::isBool,
                Typecheck::isInt,
                Typecheck::isString,
                Typecheck::isSet,
                Typecheck::isSeq
        );
        Optional<Function<Value, BoolValue>> c = functions.stream().filter(function -> function.apply(elem1).val && function.apply(elem2).val).findFirst();
        return c.isPresent() ? (elem1.equals(elem2) ? BoolValue.ValTrue : BoolValue.ValFalse) : BoolValue.ValFalse;
    }
}

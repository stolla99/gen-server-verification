------------------------------- MODULE Typecheck -------------------------------
LOCAL INSTANCE TLC
LOCAL INSTANCE Integers

isRecord(elem) ==
    TRUE

isBool(elem) ==
    TRUE

isInt(elem) ==
    TRUE

isString(elem) ==
    TRUE

isModelValue(elem) ==
    TRUE

isSet(elem) ==
    TRUE

isSeq(elem) ==
    TRUE

looseEquality(elem1, elem2) ==
    TRUE
=============================================================================
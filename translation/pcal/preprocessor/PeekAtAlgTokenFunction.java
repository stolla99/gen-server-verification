package pcal.preprocessor;

import pcal.exception.ParseAlgorithmException;

@FunctionalInterface
public interface PeekAtAlgTokenFunction {
    String apply(int tokNum) throws ParseAlgorithmException;
}


package pcal.preprocessor;

import pcal.exception.ParseAlgorithmException;

import java.util.stream.IntStream;

import static pcal.ParseAlgorithm.MustGobbleThis;

public class Gobbler {
    public static void consumeNext(int offset) {
        IntStream.range(0, offset).forEach(i -> {
            try {
                MustGobbleThis(".*", true, true);
            } catch (ParseAlgorithmException ex) {
                throw new RuntimeException(ex);
            }
        });
    }
}

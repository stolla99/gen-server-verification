package pcal.preprocessor;

@FunctionalInterface
public interface AnnotationFunction {
    Boolean apply(String token);
}

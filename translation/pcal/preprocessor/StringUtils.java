package pcal.preprocessor;

public class StringUtils {
    public static String capitalize(String target) {
        if (target == null || target.isEmpty()) {
            return target;
        } else {
            return target.substring(0, 1).toUpperCase() + target.substring(1);
        }
    }

    public static String decapitalize(String target) {
        if (target == null || target.isEmpty()) {
            return target;
        } else {
            return target.substring(0, 1).toLowerCase() + target.substring(1);
        }
    }
}

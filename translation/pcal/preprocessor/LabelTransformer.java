package pcal.preprocessor;

import pcal.AST;
import pcal.ParseAlgorithm;

import java.util.Vector;

public class LabelTransformer {
    public static String transformLabel(String label, String appendix) {
        if (label == null || label.trim().isBlank()) {
            return label;
        } else {
            System.out.println("Transforming label: " + label + " with appendix: " + appendix);
            return label + appendix;
        }
    }

    public static Vector<?> recursiveDescent(Vector<?> tokens, String appendix) {
        tokens.forEach(token -> {
            if (token instanceof VectorRepresentation vr) {
                vr.getVectors().forEach(v -> recursiveDescent(v, appendix));
            }
            if (token instanceof AST ast) {
                ast.lbl = transformLabel(ast.lbl, appendix);
            }
            if (token instanceof AST.Goto ggoto) {
                ggoto.to = ParseAlgorithm.dangerousLabels.contains(ggoto.to) ? transformLabel(ggoto.to, appendix) : ggoto.to;
            }
        });
        return tokens;
    }
}

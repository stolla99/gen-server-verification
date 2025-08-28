package pcal;

import kotlin.Pair;
import pcal.exception.TokenizerException;

import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import static pcal.PCalErlangConstants.PROCESS_VAR_ASSIGNMENT;
import static pcal.PCalErlangConstants.PROCESS_VAR_ASSIGNMENT_INPLACE;

public class PcalErlangGenUtil {

    public static List<TLAToken> TLAExprToList(TLAExpr expr) {
        ArrayList<TLAToken> exprList = new ArrayList<>();
        for (int i = 0; i < expr.tokens.size(); i++) {
            exprList.addAll(new ArrayList<>((Vector) expr.tokens.get(i)));
        }
        return exprList;
    }

    public static String formatVarAssignment(String left, String right, ErlangProcessContext context, boolean inPlace) {
        String prevStateVarName = context.getCurrentStateVarName();
        String currStateVarName = context.nextStateVarName();
        String stateRecName = context.getStateRecordName();
        if (inPlace) {
            return String.format(PROCESS_VAR_ASSIGNMENT_INPLACE, stateRecName, left, right) + ",";
        } else {
            return String.format(PROCESS_VAR_ASSIGNMENT, currStateVarName, prevStateVarName, stateRecName, left, right) + ",";
        }
    }

    public static String formatVarAssignment(String left, String right, ErlangProcessContext context) {
        return formatVarAssignment(left, right, context, false);
    }

    public static TLAExpr tokenizeTlaExpr(String tlaExpr) throws TokenizerException {
        Vector<String> inputVec = new Vector<>();
        inputVec.add(tlaExpr);
        PcalCharReader reader = new PcalCharReader(inputVec, 0, 0, 1, 0);
        Pair<TLAExpr, List<Pair<String, String>>> expOpt = Tokenize.TokenizeExpr(reader);
        return expOpt.getFirst();
    }
}
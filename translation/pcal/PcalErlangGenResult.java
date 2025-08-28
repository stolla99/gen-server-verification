package pcal;

import java.util.Vector;

public class PcalErlangGenResult {
    private final Vector<String> code;
    private final Vector<String> hrlCode;

    public PcalErlangGenResult(Vector<String> code, Vector<String> hrlCode) {
        this.code = code;
        this.hrlCode = hrlCode;
    }

    public Vector<String> getCode() {
        return code;
    }

    public Vector<String> getHrlCode() {
        return hrlCode;
    }
}

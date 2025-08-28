package pcal;

import pcal.exception.PcalErlangGenException;

import java.util.Vector;

public class PcalErlangGenerator {
	private PcalSymTab st;
    private AST ast;
	private Vector macros;
	private boolean skipStartFunctions;
    
    public PcalErlangGenerator(PcalSymTab st, AST ast, Vector macros, boolean skipStartFunctions) {
    	this.st = st;
    	this.ast = ast;
		this.macros = macros;
		this.skipStartFunctions = skipStartFunctions;
    }
    
	public PcalErlangGenResult translate() throws PcalErlangGenException, NoSuchFieldException {
		PcalErlangGen erlangGenerator = new PcalErlangGen();
        return erlangGenerator.generate(this.ast, this.st, this.macros, skipStartFunctions);
	}
}

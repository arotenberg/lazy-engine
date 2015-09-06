package org.lazyengine.runtime;

/**
 * Common supertype for algebraic data types. The structure of this class is deliberately designed
 * to resemble that of java.lang.Enum.
 */
public abstract class AlgebraicDataType extends Node {
	private final String ctorName;
	private final int ordinal;
	
	public AlgebraicDataType(String ctorName, int ordinal) {
		this.ctorName = ctorName;
		this.ordinal = ordinal;
	}
	
	public int ordinal() {
		return ordinal;
	}
	
	@Override
	public String toString() {
		return ctorName;
	}
}

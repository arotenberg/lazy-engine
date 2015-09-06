package org.lazyengine.runtime;

public final class Cell extends Node {
	Node field1, field2;
	
	private Cell() {}
	
	public static Cell makeHole() {
		return new Cell();
	}
	
	public void updateToHole() {
		field1 = null;
		field2 = null;
	}
	
	public void updateToIndirection(Node source) {
		field1 = source;
		field2 = null;
	}
	
	public void updateToAp(Node f, Node x) {
		field1 = f;
		field2 = x;
	}
	
	@Override
	public String toString() {
		if (field1 == null)
			return "?";  // hole
		else if (field2 == null)
			return "#[" + field1 + "]";  // indirection
		else
			return "(" + field1 + ") @ (" + field2 + ")";  // application
	}
}

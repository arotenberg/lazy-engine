package org.lazyengine.runtime;

import java.lang.invoke.MethodHandle;

public abstract class Node {
	Node() {}
	
	@Override
	public abstract String toString();
	
	public static Node makeSupercombinator(String globalName, int paramCount, MethodHandle body) {
		// If the supercombinator is a CAF, secretly give the caller an indirection to a
		// Supercombinator node instead of returning a Supercombinator node directly. That way, the
		// CAF can be updated in place to a shared value the first time it is evaluated.
		Node scNode = new Supercombinator(globalName, paramCount, body);
		if (paramCount == 0) {
			Cell indirNode = Cell.makeHole();
			indirNode.updateToIndirection(scNode);
			return indirNode;
		} else {
			return scNode;
		}
	}
}

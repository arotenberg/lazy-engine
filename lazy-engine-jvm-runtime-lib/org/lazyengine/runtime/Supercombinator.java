package org.lazyengine.runtime;

import java.lang.invoke.MethodHandle;

final class Supercombinator extends Node {
	final String globalName;
	final int paramCount;
	final MethodHandle body;
	
	Supercombinator(String globalName, int paramCount, MethodHandle body) {
		this.globalName = globalName;
		this.paramCount = paramCount;
		this.body = body;
	}
	
	@Override
	public String toString() {
		return globalName;
	}
}

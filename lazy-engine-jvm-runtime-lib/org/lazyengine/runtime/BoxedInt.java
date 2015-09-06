package org.lazyengine.runtime;

public final class BoxedInt extends Node {
	public static BoxedInt valueOf(int value) {
		return new BoxedInt(value);
	}
	
	private final int value;
	
	private BoxedInt(int value) {
		this.value = value;
	}
	
	public int getValue() {
		return value;
	}
	
	@Override
	public String toString() {
		return "box[" + value + "]";
	}
}

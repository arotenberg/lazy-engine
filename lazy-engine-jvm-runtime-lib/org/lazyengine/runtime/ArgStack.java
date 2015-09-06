package org.lazyengine.runtime;

final class ArgStack {
	private static final int INITIAL_SIZE = 1 << 7;
	
	/**
	 * Grows from the end towards the beginning for fast indexing.
	 */
	private Cell[] values = new Cell[INITIAL_SIZE];
	private int topOfStack = INITIAL_SIZE;
	
	void push(Cell value) {
		int topOfStack = this.topOfStack;
		topOfStack--;
		if (topOfStack < 0) {
			int valuesLength = values.length;
			topOfStack += valuesLength;
			int newLength = valuesLength << 1;
			if (newLength <= 0)  // Integer overflow
				throw new StackOverflowError();
			Cell[] newValues = new Cell[newLength];
			System.arraycopy(values, 0, newValues, valuesLength, valuesLength);
			values = newValues;
		}
		values[topOfStack] = value;
		this.topOfStack = topOfStack;
	}
	
	Cell pop() {
		int topOfStack = this.topOfStack;
		Cell poppedValue = values[topOfStack];
		values[topOfStack] = null;  // Remove reference to allow GC to reclaim memory.
		topOfStack++;
		int valuesLength = values.length;
		if (valuesLength > INITIAL_SIZE) {
			int newLength = valuesLength >> 1;
			if (topOfStack >= newLength + (valuesLength >> 2)) {
				topOfStack -= newLength;
				Cell[] newValues = new Cell[newLength];
				System.arraycopy(values, newLength, newValues, 0, newLength);
				values = newValues;
			}
		}
		this.topOfStack = topOfStack;
		return poppedValue;
	}
	
	Cell get(int index) {
		return values[topOfStack + index];
	}
	
	int size() {
		return values.length - topOfStack;
	}
}

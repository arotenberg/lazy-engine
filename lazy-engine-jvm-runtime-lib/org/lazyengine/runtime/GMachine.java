package org.lazyengine.runtime;

import java.lang.invoke.MethodHandle;

public final class GMachine {
	private final ArgStack argStack = new ArgStack();
	
	public GMachine() {}
	
	public Node eval(Node initialRoot) {
		// The JVM stack acts as the dump when we call eval() recursively, so we need to save our
		// depth in the argument stack so that we can return at the appropriate point.
		int argStackBase = argStack.size();
		Node tip = initialRoot;
		while (true) {
			// Unwind the spine of the expression.
			Cell last = null;
			while (tip instanceof Cell) {
				Cell cellTip = (Cell) tip;
				// The expression is an application or indirection.
				if (cellTip.field2 != null) {
					// The expression is an application, so push it on the stack.
					argStack.push(cellTip);
				}
				last = cellTip;
				tip = cellTip.field1;
			}

			if (!(tip instanceof Supercombinator)) {
				if (tip != null) {
					/*
					 * The expression is a boxed value; it is in WHNF and (assuming our original
					 * code type-checked) there are no arguments on the argument stack. Therefore,
					 * we can return immediately.
					 * 
					 * Note that we return the redex root, which may be different than the initial
					 * root if we have traversed indirection nodes in the process of unwinding the
					 * stack. eval() guarantees that the returned node will not be an indirection
					 * node.
					 */
					return tip;
				} else {
					// Tried to reduce a black hole.
					throw new RuntimeException("Infinite loop detected.");
				}
			}
			
			// The expression is a supercombinator.
			Supercombinator supercombinator = (Supercombinator) tip;
			int paramCount = supercombinator.paramCount;
			int argCount = argStack.size() - argStackBase;
			if (argCount < paramCount) {
				// Expression is in WHNF - pop unused arguments off the argument stack and return.
				Node redexRoot = tip;
				for (int i = 0; i < argCount; i++)
					redexRoot = argStack.pop();
				return redexRoot;
			}
			
			// Find the redex root so we can update it. If we are reducing a supercombinator with
			// at least one argument, the redex root is the application cell for the last argument.
			// However, if we are reducing a CAF, the cell to update is the indirection cell
			// created for the CAF by Node.makeSupercombinator().
			Cell redexRoot;
			if (paramCount > 0)
				redexRoot = argStack.get(paramCount - 1);
			else
				redexRoot = last;
			
			try {
				supercombinator.body.invokeExact(this, redexRoot);
			} catch (RuntimeException | Error ex) {
				// Re-throw unchecked exceptions immediately to avoid wrapping them a ridiculous
				// number of times when they are thrown from a deep stack depth.
				throw ex;
			} catch (Throwable ex) {
				throw new RuntimeException(ex);
			}
			
			tip = redexRoot;
		}
	}
	
	/**
	 * Invocation of this method is analogous to the "rearranging the stack" step commonly seen in
	 * descriptions of the G-machine.
	 */
	public Node popArg() {
		Cell poppedCell = argStack.pop();
		return poppedCell.field2;
	}
}

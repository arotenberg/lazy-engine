package example;

import org.lazyengine.runtime.GMachine;
import org.lazyengine.runtime.Node;

public final class Main {
	private Main() {}

	public static void main(String[] args) {
		Node initialRoot = ExampleGeneratedModule.shared$main;
		Node reducedRoot = new GMachine().eval(initialRoot);
		System.out.println(reducedRoot);
	}
}

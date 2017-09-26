program t{
}

class A{
	var b: Int;
	var c: Int[];

	def foo(d: Int): Int = {
		c = new Int[4];
		c[0] = 2; 
		return 0;
	} 
}

class B extends A{
	var t: String;
	def bar(d: Int[], r: String): Int = {
		return 1;
	}
}

program Traps {
	do(new Test().shortCircuit());
	do(new Test().equality());
	do(new Test().prints());
	do(new Test().ArrayTest(2));
	do(new Test().doTest());
	do(42); 			//should do nothing
}

//class definitions for the tests.
 
class Bools {
	def printFoo(): Bool = {
	  println("foo");
	  return false;
	}

	def printBar(): Bool = {
	  println("bar");
	  return true;
	}
}

class A{}
class B{}

class Test{

	//short-circuiting tests.
	def shortCircuit(): Int = {
      		var b: Bool;
		b = (new Bools().printFoo() && new Bools().printBar()); //should print "foo"
		b = (new Bools().printBar() || new Bools().printFoo()); //should print "bar"
		return 0;
	}
				
	//equality tests
	def equality(): Int = {
			var b: Bool;
      		b = (42 == 42);
      		println(b);                  // true
			b = (new A() == new A());
			println(b);        	     // false
      		b = (new A() == new B());
        	println(b);                  // false
			//"foo" == 42;               // Type Error...
			b = ("f" + "oo" == "fo" + "o");
      		println(b);  		     // false
			return 0;
	}
	

	//ArrayTest
	def ArrayTest(size: Int): Int = {
		var arr: Int[];
		var b: Bool;
		arr = new Int[size];
		b = (size == arr.length);
		println(b);			//should be true.
		arr[0] = 41;
		b = (arr[0] == 41);
		println(b);			//should be true.
		return 0;	
	}

	//print tests.
	def prints(): Int = {
		println("foo" + 3); // "foo3"
		println(3 + "foo"); // "3foo"
		println(3 + 3);     // 6
		return 0;
	}
	
	//to launch with the do() construct.
	def doTest(): Bool = {
  		println("Hello world!");
 	 	return true;
	}
}

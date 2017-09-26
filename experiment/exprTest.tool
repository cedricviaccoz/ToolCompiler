program Factorial {
}

class Fact {
    def computeFactorial(num : Int) : Int = {
        var num_aux : Int;
        if (num < 1)
            num_aux = 1;
        else
            num_aux = num;
        return num_aux;
    }

   def alo(x: Int): Int = {
	return computeFactorial(x); 
   }
}
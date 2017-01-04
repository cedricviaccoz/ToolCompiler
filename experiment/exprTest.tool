program Factorial {
}

class Fact {
    def computeFactorial(num : Int) : Int = {
        var num_aux : Int;
        if (num < 1)
            num_aux = 1;
        else
            num_aux = 2;
        return num_aux;
    }
}
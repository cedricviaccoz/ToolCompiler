program Avrg {
   do(new Devi().init(10).compute().print());
}

class Devi{

     var array : Int[];
     var avrg: Double;

     //arbitrarely filling the array bc why not.
     def init(size: Int): Devi = {
           var i : Int;
           var acc : Int;
           array = new Int[size];
           i = 0;
           acc = size/2; 
           while(i < size){
           		array[i] = acc;
           		if((i * 7) < 42){
           			acc = acc - 1 + size/2;
           		}else{
           			acc = acc + 42;
           		}
           		i = i+ 1;
           }
           return this;
      }

      def compute(): Devi = {
      		var i: Int;
      		var acc: Int;
      		i = 0;
      		acc = 0;
      		while(i < array.length){
      			acc = acc + array[i];
      			i = i+ 1;
      		}
      		avrg = acc/array.length;
      		return this;
      }	

      def print(): Boolean = {
      		var i: Int;
      		i = 0;
      		while(i < array.length){
      			println("element " + i + " : " + array[i]);
      			println("moyenne : "+ avrg);
      			i = i + 1;
      		}
      		return true;
      }
}
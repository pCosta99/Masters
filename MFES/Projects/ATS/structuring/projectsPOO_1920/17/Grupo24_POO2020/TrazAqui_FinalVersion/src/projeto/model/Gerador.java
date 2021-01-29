package projeto.model;

import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class Gerador {

	// Função que gera strings (alfanuméricas) aleatórias -> usado para criar emails e senhas
		public static String stringGenerator() {
		    int minlimit = 48; // número zero
		    int maxlimit = 122; // letra Z
		    int maximolength = 10;
		    Random random = new Random();
		 
		    String resultado = random.ints(minlimit, maxlimit + 1)
		      .filter(i -> (i <= 57 || i >= 65) && (i <= 90 || i >= 97))
		      .limit(maximolength)
		      .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
		      .toString();
		 
		    //System.out.println(generatedString);
		    return resultado;
		}
		
		
		public static double doubleCoordenadasGenerator(){
			double random = ThreadLocalRandom.current().nextDouble(-80,80);   
			return random;
		}
		
		public static double doubleGenerator(){
			double random = ThreadLocalRandom.current().nextDouble(0,10);   
			return random;
		}
		
		public static int nifGenerator() {
		    int randomNum = ThreadLocalRandom.current().nextInt(100000000, 999999999);

		    return randomNum;
		}
		
		public static int intGenerator() {
			int randomNum = ThreadLocalRandom.current().nextInt(2, 60);
			return randomNum;
		}
		
		public static boolean booleanGenerator() {
			Random rd = new Random(); // cria Random object
		    return rd.nextBoolean();
		}
}

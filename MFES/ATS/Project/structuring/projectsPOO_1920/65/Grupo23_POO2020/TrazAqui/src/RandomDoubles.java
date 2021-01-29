import java.util.Random;

/**
 * Classe que retorna valores random
 */
public class RandomDoubles {
    public static double generateDoubles() {
        Random rd = new Random();
        return rd.nextDouble();
    }

    public static int generateInts() {
        Random rand = new Random();

        // Obtain a number between [0 - 200].
        return rand.nextInt(200);
    }
}

import java.util.Random;
public class RandomCoordenates {
    
    public static double randomInterval(int s, int i) {
        Random rd = new Random();
        return rd.nextInt(s - i + 1) + i;
    }
}
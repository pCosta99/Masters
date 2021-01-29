package Model;

import java.time.LocalDateTime;
import java.util.Random;

/**
 * Classe criada para gerar uma certa aleatoriedade
 */

public class Transito {
    public double getAtrasoTransito(double delay) {
        int a = LocalDateTime.now().getHour();
        Random b = new Random();
        if(a == 8 || a == 12 || a==18) //hora de ponta
            return (b.nextDouble() % 0.6) + (delay % 0.2);
        if(a > 1 && a < 6)
            return (b.nextDouble() % 0.1) + (delay % 0.2);
        return (b.nextDouble() % 0.3) + (delay % 0.2);
    }
}

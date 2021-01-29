import java.io.Serializable;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class RandomEvents implements Serializable {
    public double clima;
    public double transito;

    /** Método que gera random events num intervalo*/
    public RandomEvents (){
        this.clima = ThreadLocalRandom.current().nextDouble(1, 100);
        this.transito = ThreadLocalRandom.current().nextDouble(1, 100);
    }

    /** Método que gera uma seed para o clima
     *
     * @param a Coordenadas
     */
    public double getSeedC (Coordenadas a){
        Random generator = new Random((long)(a.getX()*a.getY()* this.clima));
        return generator.nextInt(100);
    }

    /** Método que gera uma seed para o trânsito
     *
     * @param a Coordenadas
     */
    public double getSeedT (Coordenadas a){
        Random generator = new Random((long)(a.getX()*a.getY()* this.transito));
        return generator.nextInt(100);
    }
}

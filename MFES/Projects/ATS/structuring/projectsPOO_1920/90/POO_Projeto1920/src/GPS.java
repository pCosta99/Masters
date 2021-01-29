import java.io.Serializable;

/**
 * Classe GPS -> guarda coordenadas
 */
public class GPS implements Serializable {
    private double x;
    private double y;

    /**
     * Construtores para objetos da classe GPS
     */
    public GPS() {
        this.x = this.y = 0.0;
    }

    public GPS(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public GPS(GPS gps) {
        this.x = gps.getX();
        this.y = gps.getY();
    }


    /**
     * Metodos de instancia
     */
    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public void setX(double x) {
        this.x = x;
    }

    public void setY(double y) {
        this.y = y;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GPS gps = (GPS) o;
        return Double.compare(gps.getX(), x) == 0 &&
                Double.compare(gps.getY(), y) == 0;
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("GPS{");
        sb.append("x=").append(x);
        sb.append(", y=").append(y);
        sb.append('}');
        return sb.toString();
    }

    public GPS clone() {
        return new GPS(this);
    }


    /**
     * Calcula a distância entre duas coordenadas
     * @param gps1 Coordenadas
     * @param gps2 Coordenadas
     * @return distância calculada
     */
    public static double dist(GPS gps1, GPS gps2) {
        double x1, x2, y1, y2, aux;
        x1 = gps1.getX();
        y1 = gps1.getY();
        x2 = gps2.getX();
        y2 = gps1.getY();

        aux = Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2);

        return Math.sqrt(aux);
    }


    /**
     * Verifica se uma encomenda está dentro do raio de uma transportadora
     * @param Tranportadora Coordenadas da transportadora
     * @param Loja Coordenadas da loja
     * @param DestinoEncomenda Coordenadas do utilizador que encomendou
     * @param raioTransportadora Raio de ação da tranportadora
     * @return distância calculada
     */
    public static boolean estaDentroDoRaio(GPS Tranportadora, GPS Loja, GPS DestinoEncomenda, double raioTransportadora) {
        return dist(Tranportadora, Loja) <= raioTransportadora && dist(Tranportadora, DestinoEncomenda) <= raioTransportadora;

    }


}




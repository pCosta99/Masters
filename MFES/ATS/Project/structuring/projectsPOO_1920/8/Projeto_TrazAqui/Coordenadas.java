import java.io.Serializable;

public class Coordenadas implements Serializable {

    // Variáveis de instância
    private double latitude;
    private double longitude;

    /**
     * Construtor por omissão.
     * @param
     * @return
     */

    public Coordenadas(){
        this.latitude = 0;
        this.longitude = 0;
    }

    /**
     * Construtor parametrizado.
     * @param lat
     * @param longi
     * @return
     */

    public Coordenadas(double lat, double longi) {
        setLongitude(lat);
        setLatitude(longi);
    }

    /**
     * Construtor de cópia da classe Coordenadas.
     * @param c
     * @return
     */

    public Coordenadas(Coordenadas c) {
        this.longitude = c.getLongitude();
        this.latitude = c.getLatitude();

    }

    /**
     * Devolve a longitude
     * @param
     * @return longitude
     */

    public double getLongitude() {
        return longitude;
    }

    /**
     * Devolve a latitude
     * @param
     * @return latitude
     */

    public double getLatitude() {
        return latitude;
    }

    /**
     * Atualiza a latitude.
     * @param latitude
     * @return
     */

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Atualiza a longitude.
     * @param longitude
     * @return
     */

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Método que calcula a distancia entre dois pontos
     * @param c1
     * @return
     */
    public double distancia_Coordenadas(Coordenadas c1)
    {
       double x1=c1.getLatitude();
       double y1=c1.getLongitude();
       double x2=this.getLatitude();
       double y2=this.getLongitude();

        return Math.sqrt(Math.pow((y2 - y1),2)  + Math.pow((x2 - x1),2));
    }

    /**
     * Método que faz uma cópia da classe Coordenadas.
     * Para tal invoca o construtor de cópia.
     * @param
     * @return User clone da classe Coordenadas
     */

    public Coordenadas clone() {
        return new Coordenadas(this);
    }

    /**
     * Método que devolve a representação em String da classe Coordenadas.
     * @param
     * @return String
     */

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Coordenadas:\n").append("\tLatitude:").append(this.getLatitude()).append("\n")
                .append("\tLongitude:").append(this.getLongitude()).append("\n");
        return sb.toString();
    }

    /**
     * Método que verifica se um Object é igual à classe Coordenadas atual.
     * @param o
     * @return boolean
     */

    public boolean equals(Object o) {
        if (o == this) return true;
        if ((o == null) || (o.getClass() != this.getClass())) return false;

        Coordenadas c = (Coordenadas) o;

        return this.longitude == c.getLongitude() && this.latitude == c.getLongitude();
    }

    public int hashCode(){
        int hash = 5;
        long aux1,aux2;
        aux1 = Double.doubleToLongBits(this.latitude);
        hash = 31*hash + (int)(aux1 ^ (aux1 >>> 32));
        aux2 = Double.doubleToLongBits(this.longitude);
        hash = 31*hash + (int)(aux2 ^ (aux2 >>> 32));
        return hash;
    }
}
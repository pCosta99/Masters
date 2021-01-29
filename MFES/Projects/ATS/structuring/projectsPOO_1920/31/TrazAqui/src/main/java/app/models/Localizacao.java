package app.models;

import java.io.Serializable;
import java.util.Objects;

public class Localizacao implements Serializable {

    /**
    *
    */
    private static final long serialVersionUID = 8061619903190810711L;
    // #region variables
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private double latitude;
    private double longitude;
    // #endregion

    // #region Construtores

    public Localizacao() {
        this.latitude = 0.0;
        this.longitude = 0.0;
    }

    /**
     * @param latitude
     * @param longitude
     */
    public Localizacao(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * @param l
     */
    public Localizacao(Localizacao l) {
        this.latitude = l.latitude;
        this.longitude = l.longitude;
    }
    // #endregion

    // #region getters setter
    /**
     * @return the longitude
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * @param longitude the longitude to set
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the latitude
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * @param latitude the latitude to set
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }
    // #endregion

    // #region Overrrides

    @Override
    public boolean equals(Object o) {
        // self check
        if (this == o) {
            return true;
        }
        // null check
        if (o == null) {
            return false;
        }
        // type check and cast
        if (getClass() != o.getClass()) {
            return false;
        }
        Localizacao localizacao = (Localizacao) o;
        // field comparison
        return Objects.equals(this.latitude, localizacao.latitude)
                && Objects.equals(this.longitude, localizacao.longitude);

    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Latitude : ");
        sb.append(this.latitude);
        sb.append('\n');
        sb.append("Longitude :");
        sb.append(this.longitude);
        sb.append('\n');
        return sb.toString();

    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    public Localizacao clone() {
        return new Localizacao(this);
    }

    // #endregion

    // #region Methods

    public double distancia(Localizacao loc) {
        if ((this.latitude == loc.latitude) && (this.longitude == loc.longitude)) {
            return 0;
        } else {
            double theta = this.longitude - loc.longitude;
            double dist =
                    Math.sin(Math.toRadians(this.latitude)) * Math.sin(Math.toRadians(loc.latitude))
                            + Math.cos(Math.toRadians(this.latitude))
                                    * Math.cos(Math.toRadians(loc.latitude))
                                    * Math.cos(Math.toRadians(theta));
            dist = Math.acos(dist);
            dist = Math.toDegrees(dist);
            dist = dist * 60 * 1.1515;
            dist = dist * 1.609344;
            return (dist);
        }
    }
    // #endregion

}

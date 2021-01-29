package interfaces;

import java.util.Map;

public interface IEntregas {
    String getCode();
    void setCode(String code);
    String getNome();
    void setNome(String nome);
    double getLatitude();
    double getLongitude();
    void setLongitude(double lon);
    double getRaio();
    double getVelocidade();
    boolean aceitoTransporteMedicamentos();
    void aceitaMedicamentos(boolean state);
    Double getRating();
    void setRating(double rate);
    boolean isLivre();
    void setLivre(boolean state);
    Map<String,IEncomenda> getEncomendas();
    void setEncomendas(Map<String,IEncomenda> le);
    void addEncomenda(IEncomenda e);
    double distancia(Object o);
    boolean inRaio(Object o);
    int compareTo(IEntregas e);
}

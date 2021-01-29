package interfaces;

import java.util.Map;

public interface IUser {
    String getNome();
    double getLatitude();
    double getLongitude();
    String getCode();
    Map<String,IEncomenda> getEncomendas();
    void setEncomendas(Map<String,IEncomenda> t);
    void addEncomenda(IEncomenda e);
    String toString();
    int compareTo(IUser u);
    IUser clone();
}

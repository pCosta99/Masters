package Model.Encomendas;

import Model.Catalogos.ICatalogoProds;

import java.time.LocalTime;
import java.util.ArrayList;

public interface IEncomenda {
    String getEncomendaID();
    String getUserID();
    String getLojaID();
    double getPesoTotal();
    boolean getMedicamentos();
    boolean getCongelados();

    ArrayList<LinhaEncomenda> getProds() ;
    void setEncomendaID(String encomendaID);
    void setUserID(String userID);
    void setLojaID(String lojaID);
    void setPesoTotal(double pesoTotal);
    void setProds(ArrayList<LinhaEncomenda> prods);
    void setMedicamentos(boolean medicamentos);
    void setCongelados(boolean congelados);


    LocalTime getHoraInicial();
    void setHoraInicial(LocalTime horaInicial);
    void criaEncomenda (String aux, ICatalogoProds catalogoProds);
}

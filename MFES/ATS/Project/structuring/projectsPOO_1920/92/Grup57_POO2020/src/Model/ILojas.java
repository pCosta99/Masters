package Model;

import java.util.ArrayList;
import java.util.List;

public interface ILojas {

    void put(Loja l);

    void put(String cod, Loja l);

    String extractNameByUserName(String userName);

    boolean checkExisteLoja(String codLoja);

    void adicionaEncomendaFilaDeEspera(String codLoja, Encomenda e);

    Loja extraiLoja(String codLoja);

    List<String> exportKeyLojas();

    List<Integer> encomendaPronta(String codLoja);

    void changeAceitaFila(String loja, boolean aceita);
}

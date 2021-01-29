package Model;

import java.util.List;

public interface ITransportadoras {

    void put(Transportadora t);

    void put(String cod, Transportadora t);

    String extractNameByUserName(String userName);

    List<String> extractTransportadoras();

    Transportadora extractTransportadora(String codTransportadora);

    boolean checkExisteTransportadora(String codTransportadora);

    List<String> exportTransportadorasDentroRaio(double xUtilizador, double yUtilizador);

    double extractCustoKM(String transportadora);

    void addClassificacao(String transportadoras, int classificacao);

    double exportClassMediaTransportadora(String transportadora);

    void changeAceita(String transportadora, boolean aceita);
}

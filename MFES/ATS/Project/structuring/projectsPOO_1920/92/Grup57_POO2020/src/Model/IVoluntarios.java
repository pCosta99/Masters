package Model;

import java.util.List;

public interface IVoluntarios {

    void put(Voluntario v);

    void put(String cod, Voluntario v);

    String extractNameByUserName(String userName);

    List<String> extractVoluntarios();

    Voluntario extractVoluntario(String voluntario);

    boolean checkExisteVoluntario(String codVoluntario);

    List<String> exportVoluntariosDentroRaio(double xUtilizador, double yUtilizador);

    void addClassificacao(String voluntario, int classificacao);

    double exportClassMediaVoluntario(String voluntario);

    void changeAceita(String voluntario, boolean aceita);
}

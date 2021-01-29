package interfaces;

import java.io.IOException;

public interface ILogin {
    boolean loginUtilizador(String user, String password);
    boolean loginVoluntario(String volunts, String password);
    boolean loginEmpresa(String empresas, String password);
    boolean loginLoja(String lojas, String password);
    boolean addUtilizador(String user, String password);
    boolean addVoluntario(String user, String password);
    boolean addEmpresa(String user, String password);
    boolean addLoja(String user, String password);
    void gravarDados() throws IOException;
    ILogin lerDados() throws IOException, ClassNotFoundException;
    ILogin clone();
}

package Projeto.Interfaces;
import java.io.Serializable;

public interface IUtilizador extends IEntidade, Serializable {
    void setNIF(String nif);
    String getNIF();
    boolean clienteComprouEmpresa(String idEmpresa);
    IUtilizador clone();
}

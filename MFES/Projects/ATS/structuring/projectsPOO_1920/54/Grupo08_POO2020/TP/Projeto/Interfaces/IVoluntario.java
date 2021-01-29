package Projeto.Interfaces;
import java.io.Serializable;
import java.util.Collection;
import Projeto.Util.Estado;

public interface IVoluntario extends IEntidade, Serializable {
    Estado getEstado();
    void insereVel(float v);
    boolean aceitoTransporteMedicamentos();
    void setEstado(Estado e);
    void setCapMax(int cap);
    float calculaVelMed();
    void setRaio(float raio);
    void aceitaMedicamentos(boolean medic);
    void insereClassificacao(int cl);
    boolean temCapacidade(boolean med);
    void addEncomendaPorEntregar(IEncomenda e);
    void switchEncomenda(IEncomenda e);
    Collection<IEncomenda> getEncomendasPorEntregar();
    float getRaio();
    IVoluntario clone();
}
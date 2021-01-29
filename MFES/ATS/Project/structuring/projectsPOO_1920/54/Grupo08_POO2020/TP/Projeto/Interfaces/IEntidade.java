package Projeto.Interfaces;
import Projeto.Util.GPS;
import java.io.Serializable;
import java.util.Collection;

public interface IEntidade extends Serializable, Comparable<IEntidade> {
    void adicionaEnc(IEncomenda e);
    String getId();
    String getPassword();
    Collection<IEncomenda> getEncomendas();
    GPS getLocalizacao();
    String getNome();
    void setNome(String nome);
    IEncomenda getEncomenda(String id);
    void setLocalizacao(GPS novaLoc);
    Collection<IAviso> getNotificacoes();
    void addNotificacao(IAviso a);
    void removeNotificacao(IAviso a);
    void removeEnc(IEncomenda e);
}
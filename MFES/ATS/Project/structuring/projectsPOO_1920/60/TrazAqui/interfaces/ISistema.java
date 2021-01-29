package interfaces;

import model.*;

import java.io.IOException;
import java.util.*;

public interface ISistema {

    void addVoluntario(IVoluntario v);
    void addEmpresa(IEmpresa e);
    void addLoja(ILoja l);
    void addUser(IUser u);
    void addEncomenda(IEncomenda e);
    void addEncomendaAceite(String e);
    Sistema clone();
    Map<String,ILoja> getLojas();
    Map<String,IUser> getUtilizadores();
    Map<String,IEncomenda> getEncomendas();
    Map<String,IVoluntario> getVoluntarios();
    List<String> getEncomendasAceites();
    void addProdutosALoja();
    void addEncomendaACliente();
    void aceitaEnc(String e);
    Map<String, IEmpresa> getTransportadoras();
    void removeEnc(String enc);
    List<AbstractMap.SimpleEntry<IUser, Integer>> top10Users();
    List<AbstractMap.SimpleEntry<IEmpresa, Double>> top10Empresas();
    void gravarDados() throws IOException;
    ISistema lerDados() throws IOException, ClassNotFoundException;
}

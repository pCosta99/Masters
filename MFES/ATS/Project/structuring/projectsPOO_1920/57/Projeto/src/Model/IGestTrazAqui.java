package Model;

import java.util.List;
import java.util.Set;

public interface IGestTrazAqui {
    Utilizador getUser(String userCode);
    void setUser(Utilizador user);
    List<String> getUserEncStatus(String userCode);
    boolean getUserEncStandBy(String enc);
    Set<String> getUserStandByTransp(String userCode);
    void addUser(Utilizador user);
    boolean containsUser(String code);
    Loja getLoja(String storeCode);
    void setLoja(Loja loja);
    void addLoja(Loja loja);
    Login getLogin(String code);
    void setLogin(Login login);
    void addLogin(Login login);
    boolean containsPassword(String code, String password);
    boolean containsNameAndType(String name, String type);
    Encomenda getEncomenda(String encCode);
    void setEncomenda(Encomenda enc);
    void addEncomenda(Encomenda encomenda);
    void aceitarEncomenda(String encCode);
    Set<String> encomendasAceites();
    boolean isEncomendaAceite(String encCode);
    double precoEncomenda(String encCode,String transpCode);
    List<String> encomendasPossiveis(String transpCode);
    boolean containsEncRota(String estCode,String encCode);
    String encomendaStandBy(String estCode);
    String getEncUser(String encCode);
    String getEncUserName(String encCode);
    String getEncTransp(String encCode);
    void sugerirTransp(String enc,String transpCode);
    void removeEstafetaEncRota(String transpCode,String enc);
    String escolheEstafeta(List<String>list,String enc);
    void addEstafetaRota(String transpCode,String rota);
    List<String> possiveisEstafetas(String enc);
    Estafeta getEstafeta(String code);
    void setEstafeta(Estafeta estafeta);
    void addEstafeta(Estafeta estafeta);
    int getEstafetaNumEnc(String transpCode);
    String getEstafetaType(String estCode);
    String getEstafetaName(String estCode);
    int getEstafetaRotaSize(String transpCode);
    Set<String> getEstafetaRota(String transpCode);
}

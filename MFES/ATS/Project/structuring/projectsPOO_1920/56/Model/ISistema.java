package Model;

import Model.Catalogos.ICatalogoLogins;
import Model.Catalogos.ICatalogoProds;
import Model.Catalogos.ICatalogoTipo;
import Model.Encomendas.IEncomenda;
import Model.Logins.ILogin;
import Model.Tipos.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public interface ISistema {

    ICatalogoLogins getLogins();
    ICatalogoProds getCatalogoProds();
    ILogin getQuem();
    HashSet<String> getAceites();
    IFila getFilaEspera();
    IFila getFilaEncomendas();
    IFilaEntregues getFilaEntregues();
    IGestaoEncomendas getGestao();
    ICatalogoTipo getLojas();
    ICatalogoTipo getUsers();
    ICatalogoTipo getEmpresas();
    ICatalogoTipo getVoluntarios();

    void setLojas(ICatalogoTipo lojas);
    void setUsers(ICatalogoTipo users);
    void setEmpresas(ICatalogoTipo empresas);
    void setVoluntarios(ICatalogoTipo voluntarios);
    void setGestao(IGestaoEncomendas gestao);
    void setFilaEspera(IFila filaEspera);
    void setFilaEncomendas(IFila filaEncomendas);
    void setFilaEntregues(IFilaEntregues filaEntregues);
    void setAceites(HashSet<String> encs);
    void setQuem(ILogin quem);

    void setCatalogo(ICatalogoProds cat);


    void StockLoja();


    void addAceite(String id);

    ITipo[] top10Users();
    Integer[] getNComprasUser(ITipo[] users);


}

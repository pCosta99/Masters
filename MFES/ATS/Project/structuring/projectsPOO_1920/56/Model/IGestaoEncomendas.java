package Model;

import Model.Catalogos.ICatalogoTipo;
import Model.Catalogos.IProduto;
import Model.Encomendas.IEncomenda;
import Model.Logins.ILogin;
import Model.Tipos.ITipo;
import Model.Tipos.Loja;
import Model.Tipos.User;

import java.util.HashSet;
import java.util.List;

public interface IGestaoEncomendas {
    IEncomenda constroiEncomendaParaLoja (String loja , List<IProduto> prods, List<String> quantidades, User user);
    HashSet<ITipo> verificarTransporte(ICatalogoTipo voluntarios, ICatalogoTipo empresas, IEncomenda encomenda, Loja loja);
    float distanciaPercorrida(User user , ITipo transp , Loja loja);
}

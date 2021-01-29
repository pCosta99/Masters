package Controller;

import Model.Catalogos.IProduto;
import Model.ISistema;
import View.IAppView;

import java.util.List;

public interface IUserController {
    void setSistema(ISistema sistema);
    void setAppView(IAppView view);
    int userMode();
    int catalogo(int opcao, List<IProduto> prods, List<String> quantidades);
    String escolheProdLoja(int opcao,List<IProduto> prods, List<String> quantidades);
}
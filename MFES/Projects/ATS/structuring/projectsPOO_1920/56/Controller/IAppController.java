package Controller;

import Model.ISistema;
import Model.Tipos.ILoja;
import View.IAppView;

import java.util.HashSet;
import java.util.List;

public interface IAppController {
    void setSistema(ISistema sistema);
    void setAppView(IAppView view);
    ISistema runController();
    char signUp ();
}

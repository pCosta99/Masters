package Controller;

import Model.ISistema;
import View.IAppView;

public interface IEmpresaController {
    void setSistema(ISistema sistema);
    void setView(IAppView view);
    void setEmpresa();
    void mode();
}

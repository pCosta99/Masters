package Controller;

import Model.ISistema;
import View.IAppView;

public interface ILojaController {
    void setSistema(ISistema sistema);
    void setView(IAppView view);

    void lojaMode();

}

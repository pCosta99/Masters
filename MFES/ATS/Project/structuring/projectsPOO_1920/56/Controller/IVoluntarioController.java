package Controller;

import Model.ISistema;
import View.IAppView;

public interface IVoluntarioController {

    void setSistema(ISistema sistema);
    void setAppView(IAppView view);
    void setVoluntario();
    void VoluntarioMode();
}

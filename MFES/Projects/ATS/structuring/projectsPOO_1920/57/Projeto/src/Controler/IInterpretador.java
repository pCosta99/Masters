package Controler;

import Model.GestTrazAqui;
import Model.Login;
import View.Apresentacao;

import java.io.IOException;

public interface IInterpretador {
    void interpretador(GestTrazAqui c, Apresentacao a, Login l) throws ClassNotFoundException, IOException;
}

package app;

import app.controllers.GestorRegistos;
import app.views.MenuInicial;

/**
 * App Start Class
 *
 */
public class App {
    public static void main(String[] args) {
        GestorRegistos gr = new GestorRegistos();
        MenuInicial mi = new MenuInicial(gr);
        mi.menuInicial();
    }
}

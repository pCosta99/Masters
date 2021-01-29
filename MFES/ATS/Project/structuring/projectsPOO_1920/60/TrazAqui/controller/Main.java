package controller;

import interfaces.ILogin;
import interfaces.ISistema;
import model.Login;

/**
 * Classe que implementa o programa
 */
public class Main {

    /**
     * Método main
     * @param args argumentos
     */
    public static void main(String[] args) {

        Parse p = new Parse();
        ILogin login = new Login();
        ISistema s = p.parse(login);

        Teste t = new Teste(s,login);

        t.executer();
    }

}

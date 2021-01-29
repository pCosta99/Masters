package Controlador;

import Model.*;
import View.ViewLogin;


public class ControladorLogInUser {
    private Modelo m;

    public ControladorLogInUser(Modelo m){
        this.m = m;
    }
    public void run() {
        InterfaceInput i = new Input();
        ViewLogin v = new ViewLogin();
        int o,o2;
        do {
            v.menuLogin();
            v.Op();
            o = i.lerInt();
            switch (o) {
                case 0:
                    break;
                case 1:
                    v.ID();
                    String u = i.lerString();
                    v.pass();
                    String p = i.lerString();
                    if (m.verificaLogin(u, p,3)){
                        v.loginAccep();
                        ControladorUser user = new ControladorUser(m,u);
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                        user.run();
                    }
                    else {
                        v.LoginDeny();
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                    }
                    break;
                case 2:
                    try {
                        v.ID();
                        String c = i.lerString();
                        if (m.existeUser(c)) {
                            v.siginD();
                            break;
                        }
                        v.pass();
                        String pa = i.lerString();
                        v.nome();
                        String n = i.lerString();
                        v.GPS();
                        double x = i.lerDouble();
                        double y = i.lerDouble();
                        GPS gps = new GPS(x, y);
                        Utilizador novo = new Utilizador(pa, c, n, gps);
                        m.addUtilizador(novo);
                        v.siginA();

                        v.continuar();
                        o2 = i.lerInt();
                        continuar(m, c, o2);
                    }catch(Exception e){
                        v.printInv();
                    }
                    break;
                default:
                    v.printError();
                    break;
            }
        } while (o != 0);
    }


    public boolean verificaLogin(String u, String p){
        return m.verificaLogin(u,p,3);
    }

    public void continuar(Modelo m, String u, int o) {
        InterfaceInput i = new Input();
        ViewLogin v = new ViewLogin();
        if (o == 1) {
            v.pressioneEnter();
            i.lerString();
            v.flush();
            ControladorUser user = new ControladorUser(m,u);
            user.run();
        }
    }


}
package Controlador;

import Model.*;
import View.ViewLogin;


public class ControladorLogInTransp {
    private Modelo m;

    public ControladorLogInTransp(Modelo m){
        this.m = m;
    }

    public void run() {
        InterfaceInput i = new Input();
        boolean medico = false;
        boolean cenas = true;
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
                    if (m.verificaLogin(u,p,1)){
                        v.loginAccep();
                        ControladorTransp transp = new ControladorTransp(u,m);
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                        transp.run();
                    }
                    else {
                        v.LoginDeny();
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                    }
                    medico = false;
                    cenas = true;
                    break;
                case 2:
                    try {
                        v.IDT();
                        String c = i.lerString();
                        if (m.existeTransp(c) || c.charAt(0) != 't') {
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
                        v.raio();
                        double r = i.lerDouble();
                        v.nif();
                        int nif = i.lerInt();
                        v.taxa();
                        double taxa = i.lerDouble();
                        v.taxapeso();
                        double taxapeso = i.lerDouble();
                        v.pesol();
                        int pesol = i.lerInt();
                        GPS gps = new GPS(x, y);
                        while (!medico && cenas) {
                            v.addMedico();
                            int med = i.lerInt();
                            if (med == 1) {
                                medico = true;
                            } else if (med == 2) cenas = false;
                            else v.printError();
                        }
                        Transportadora novo = new Transportadora(pa, c, n, gps, nif, r, taxa, taxapeso, pesol, medico);
                        m.addTransportadora(novo);
                        v.printDadosTrans(novo);
                        v.siginA();

                        v.continuar();
                        o2 = i.lerInt();
                        continuar(c, m, o2);
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

    public boolean verificaLogin(String u, String p){ return m.verificaLogin(u,p,1);}

    public void continuar(String c, Modelo m, int o) {
        InterfaceInput i = new Input();
        ViewLogin v = new ViewLogin();
        if (o == 1) {
            v.pressioneEnter();
            i.lerString();
            v.flush();
            ControladorTransp transp = new ControladorTransp(c,m);
            transp.run();
        }
    }
}

package Controlador;

import Model.*;
import View.ViewLogin;


public class ControladorLogInVoluntarios {
    private Modelo m;

    public ControladorLogInVoluntarios(Modelo m){
        this.m = m;
    }

    public void run() {
        InterfaceInput i = new Input();
        ViewLogin v = new ViewLogin();
        int o,o2;
        boolean medico = false;
        boolean cenas = true;
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
                    if (m.verificaLogin(u, p,2)){
                        v.loginAccep();
                        ControladorVoluntario vol = new ControladorVoluntario(u,m);
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                        vol.run();
                    }
                    else {
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                        v.LoginDeny();
                    }
                    break;
                case 2:
                    try {
                        v.IDV();
                        String c = i.lerString();
                        if (m.existeVol(c) || c.charAt(0) != 'v') {
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
                        GPS gps = new GPS(x, y);
                        while (!medico && cenas) {
                            v.addMedico();
                            int med = i.lerInt();
                            if (med == 1) {
                                medico = true;
                            } else if (med == 2) cenas = false;
                            else v.printError();
                        }

                        Voluntario novo = new Voluntario(pa, c, n, gps, r, medico);
                        v.printDadosVol(novo);
                        m.addVoluntario(novo);
                        v.siginA();

                        v.continuar();
                        o2 = i.lerInt();
                        continuar(c, m, o2);
                        medico = false;
                        cenas = true;
                    }catch (Exception e){
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
        return m.verificaLogin(u,p,2);
    }

    public void continuar(String u, Modelo m, int o) {
        InterfaceInput i = new Input();
        ViewLogin v = new ViewLogin();
        if (o == 1) {
            ControladorVoluntario vol = new ControladorVoluntario(u,m);
            v.pressioneEnter();
            i.lerString();
            v.flush();
            vol.run();
        }
    }
}

package Controlador;

import Model.*;
import View.ViewLogin;

import java.util.TreeSet;

public class ControladorLogInLoja {
    private Modelo m;

    public ControladorLogInLoja(Modelo m) {
        this.m = m;
    }

    public void run() {
//      Controlador.ControladorLoja loja = new Controlador.ControladorLoja();
        InterfaceInput i = new Input();
        ViewLogin v = new ViewLogin();
        int o, o2;
        do {
            v.menuLogin();
            v.Op();
            o = i.lerInt();
            switch (o) {
                case 0:
                    break;
                case 1:
                    v.pressioneEnter();
                    i.lerString();
                    v.flush();
                    v.ID();
                    String u = i.lerString();
                    v.pass();
                    String p = i.lerString();
                    if (m.verificaLogin(u, p, 0)) {
                        v.loginAccep();
                        ControladorLoja loja = new ControladorLoja(u, m);
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                        loja.run();
                    } else {
                        v.LoginDeny();
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                    }
                    break;
                case 2:
                    try {
                        v.pressioneEnter();
                        v.flush();
                        v.ID();
                        String c = i.lerString();
                        if (m.existeLoja(c)) {
                            v.siginD();
                            break;
                        }
                        v.pass();
                        String pi = i.lerString();
                        v.nome();
                        String n = i.lerString();
                        v.GPS();
                        double x = i.lerDouble();
                        double y = i.lerDouble();
                        GPS gps = new GPS(x, y);

                        boolean respo = false;
                        int ix = 0;
                        while (!respo) {
                            v.filaEspera();
                            int res = i.lerInt();
                            if (res == 1) {
                                Loja nova = new Loja(pi, c, n, gps, (TreeSet<Produto>) m.getProdutos(), 0);
                                m.addLoja(nova);
                                ix =1;
                                respo = true;

                            } else if (res == 0) {
                                Loja novo = new Loja(pi, c, n, gps, (TreeSet<Produto>) m.getProdutos(), -1);
                                m.addLoja(novo);
                                ix = 2;
                                respo = true;

                            } else v.printError();
                        }
                        v.siginA();
                        v.continuar();
                        o2 = i.lerInt();
                        continuar(c, m, o2,ix);
                    } catch (Exception e) {
                        System.out.println("Cona " + e.getMessage());
                    }
                    break;

                default:
                    v.printError();
                    break;
            }
        } while (o != 0);
    }

    public boolean verificaLogin(String u, String p) {
        return m.verificaLogin(u, p, 0);
    }


    public void continuar(String u, Modelo m, int o, int i) {
        if (i == 2) {
            ViewLogin v = new ViewLogin();
            if (o == 1) {
                ControladorLoja l = new ControladorLoja(u, m);
                v.pressioneEnter();
                v.flush();
                l.run();
            }
        } else if (i == 1) {
            ViewLogin v = new ViewLogin();
            if (o == 1) {
                ControladorLojaFila l1 = new ControladorLojaFila(u, m);
                v.pressioneEnter();
                v.flush();
                l1.runFila();
            }
        }
    }




}

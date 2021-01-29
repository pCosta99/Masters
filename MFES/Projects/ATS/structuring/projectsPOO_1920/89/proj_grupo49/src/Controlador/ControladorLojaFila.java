package Controlador;

import Model.*;
import View.ViewLoja;

public class ControladorLojaFila {
    private String c;   // cod loja
    private Modelo m;

    public ControladorLojaFila(String c, Modelo m) {
        this.c = c;
        this.m = m;
    }

    public void runFila(){
        InterfaceInput i = new Input();
        int o;
        boolean medico = false;
        boolean cenas = true;
        do{
            ViewLoja v = new ViewLoja();
            v.menuLojaFila();
            v.inst();
            o = i.lerInt();
                switch (o){
                    case 0:
                        break;
                    case 1:
                        v.pressioneEnter();
                        v.flush();
                        v.op1();
                        String e = i.lerString();
                        if(m.op1Loja(e,c) == 1) v.instExec();
                        else v.op1Error();
                        break;
                    case 2:
                        v.pressioneEnter();
                        v.flush();
                        v.addCP();
                        String p = i.lerString();
                        if (!m.getLojas().getLoja(c).existeProd(p)) {
                            v.addDesP();
                            String des = i.lerString();
                            while (!medico && cenas){
                                v.addMedico();
                                int med = i.lerInt();
                                if (med == 1){
                                    medico = true;
                                }
                                else if (med == 0) cenas = false;
                                else v.printError();
                            }
                            v.addPeso();
                            double pr = i.lerDouble();
                            v.addQP();
                            double q = i.lerDouble();
                            Produto produto = new Produto(p, des,medico, pr, q);
                            m.op2Loja(produto,c);
                            v.printProduto(produto);
                        }
                        else v.addPError();

                        medico = false;
                        cenas = true;
                        break;
                    case 3:
                        v.pressioneEnter();
                        v.flush();
                        v.opc3(m.getLojas().getLoja(c).getProntas());
                        break;
                    case 4:
                        v.flush();
                        v.opc4(m.getLojas().getLoja(c).getListaEnc(), m.getLoja(c).getProntas());
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                        break;
                    case 5:
                        v.pressioneEnter();
                        v.flush();
                        v.opc5(m.getLojas().getLoja(c).getListaEnc());
                        break;
                    case 6:
                        v.pressioneEnter();
                        v.flush();
                        v.printProd(m.getLojas().getLoja(c).getStock());
                        break;
                    case 7:
                        int op = -1;
                        while (op!=0) {
                            v.flush();
                            v.printDadosAtuais(m.getLojas().getLoja(c));
                            v.pressioneEnter();
                            v.printMenuDados();
                            v.inst();
                            op = i.lerInt();
                            op7(v, op);
                        }
                        break;
                    case 8:
                        v.flush();
                        v.filaEspera (m.getLoja(c));
                        v.setFilaEspera();
                        int f = i.lerInt();
                        m.getLoja(c).setFila(f);
                        v.pressioneEnter();
                        i.lerString();
                        break;
                    default:
                    v.printError();
                    break;
            }

        }while(o!=0);
    }

    public String getC() {
        return c;
    }

    public void setC(String c) {
        this.c = c;
    }

    public Modelo getM(){return this.m;}


    public void op7(ViewLoja v, int op){
        InterfaceInput i = new Input();
        switch (op){
            case 0:
                break;
            case 1:
//                v.flush();
//                v.pressioneEnter();
                v.altNome();
                String nome = i.lerString();
                m.op7LojaNome(nome, c);
                break;
            case 2:
//                v.flush();
//                v.pressioneEnter();
                v.passordAntiga();
                String passAnt = i.lerString();
                if (passAnt.equals(m.getLojas().getLoja(c).getPass())){
                    v.passordNova();
                    String nova = i.lerString();
                    m.op7LojaPass(nova,c);
                }
                else v.passError();
                break;
            case 3:
//                v.flush();
//                v.pressioneEnter();
                v.altloc();
                double lat = i.lerDouble();
                v.altloclon();
                double lon = i.lerDouble();
                m.op7LojaGPS(lat,lon,c);
                break;
            default:
                v.printError();
        }
    }
}





package Controlador;

import Model.Encomenda;
import Model.Modelo;
import View.ViewVoluntario;

import java.io.Serializable;

public class ControladorVoluntario  {
    private Modelo m;
    private String cod;


    public ControladorVoluntario(String c, Modelo m) {
        this.m = m;
        this.cod = c;
    }

    public void run(){
        InterfaceInput i = new Input();
        int o;
        do{
            ViewVoluntario v = new ViewVoluntario();
            v.menuVol();
            o = i.lerInt();
            switch (o){
                case 0:
                    break;
                case 1:
                    if (m.getVoluntarios().getVoluntario(cod).getPedidos().isEmpty() || !m.getVoluntario(cod).getLivre()){
                        v.printVazia();
                        break;
                    }
                    else{
                        for(Encomenda e : m.getVoluntario(cod).getPedidos()) {
                            v.op1(e);
                            int op;
                            v.printMenuPedidos();
                            v.inst();
                            op = i.lerInt();
                            if (op == 1) {
                                m.op1Voluntario_1(cod,e);
                                v.aceite();
                                break;
                            }
                            else if (op == 2) {
                                m.op1Voluntario_2(cod,e);
                                v.rejeite();
                            }
                        }
                    }
                    break;
                case 2:
                    v.flush();
                    v.opc2(m.getVoluntario(cod).getList());
                    break;
                case 3:
                    v.flush();
                    if (m.getVoluntario(cod).getList().size() <= 0){
                         v.empetyList();
                    }
                    else {
                        v.getEncomendas();
                        for (Encomenda es : m.getVoluntario(cod).getList()) {
                            if (!es.getEntregue()) v.op3_aux(es,m.getLoja(es.getCodloja()), m.getUtilizador(es.getCoduser()));
                        }
                    }
//                    v.opc3(m.getTransportadora(cod).getList());
                    break;
                case 4:
                    int op = -1;
                    while (op!=0) {
                        v.pressioneEnter();
                        v.flush();
                        v.printDadosAtuais(m.getVoluntario(cod));
                        v.printMenuDados();
                        v.inst();
                        op = i.lerInt();
                        op3(v, op);
                    }
                    break;
                case 5:
                    v.op4();
                    String e = i.lerString();
                    if(m.op5Vol(e,cod) == 1);
                    else v.printError();
                    break;
                default:
                    System.out.println("Opçao invalida!");
            }

        }while(o!=0);
    }

    public void op3(ViewVoluntario v, int op){
        InterfaceInput i = new Input();

        switch (op){
            case 0:
                break;
            case 1:
//                v.flush();
//                v.pressioneEnter();
                v.altNome();
                String nome = i.lerString();
                m.op3VolNome(nome,cod);
                break;
            case 2:
//                v.flush();
//                v.pressioneEnter();
                v.passordAntiga();
                String passAnt = i.lerString();
                if (passAnt.equals(m.getVoluntario(cod).getPass())){
                    v.passordNova();
                    String nova = i.lerString();
                    m.op3VolPass(nova,cod);
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
                m.op3VolGPS(lat,lon,cod);
                break;
            case 4:
                v.altRaio();
                double raio = i.lerDouble();
                m.op3VolRaio(raio,cod);
                v.raioSuc();
                break;
            case 5:
                v.altMedico();
                int op3 = -1;
                boolean op3b = false;
                while (!op3b){
                    op3 = i.lerInt();
                    switch (op3){
                        case 1:
                            m.getVoluntario(cod).aceitaMedicamentos(true);
                            op3b = true;
                            break;
                        case 2:
                            m.getVoluntario(cod).aceitaMedicamentos(false);
                            op3b = true;
                        default:
                            v.printError();
                            break;
                    }
                }
            default:
                v.printError();
        }
    }
}

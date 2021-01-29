package Controlador;

import Model.*;
import View.ViewTransp;

import java.io.Serializable;
import java.time.LocalDateTime;

public class ControladorTransp {
    private String cod;
    private Modelo m;

    public ControladorTransp(String cod, Modelo m){
        this.cod = cod;
        this.m = m;
    }

    public void run(){
        InterfaceInput i = new Input();
        int o;
        do{
            ViewTransp v = new ViewTransp();
            v.menuTransp();
            o = i.lerInt();
            switch (o){
                case 0:
                    break;
                case 1:
                    if (m.getTransportadora(cod).getPedidos().isEmpty()){
                        v.printVazia();
                        break;
                    }
                    else{
                        for(Encomenda e : m.getTransportadora(cod).getPedidos()) {
                            v.op1(e);
                            int op;
                            v.printMenuPedidos();
                            v.inst();
                            op = i.lerInt();
                            if (op == 1) {
                                m.op1Transp_1(cod,e);
                                v.aceite();
                            }
                            else if (op == 2) {
                                m.opTransp_2(cod,e);
                                v.rejeite();
                            }
                        }
                    }
                    break;

                case 2:
                    v.flush();
                    v.opc2(m.getTransportadora(cod).getList());
                    break;
                case 3:
                    v.flush();
                    if (m.getTransportadora(cod).getList().size() <= 0){
                         v.empetyList();
                    }
                    else {
                        v.getEncomendas();
                        for (Encomenda es : m.getTransportadora(cod).getList()) {
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
                        v.printDadosAtuais(m.getTransportadora(this.cod));
                        v.printMenuDados();
                        v.inst();
                        op = i.lerInt();
                        op4(v, op);
                    }
                    break;
                case 5:
                    v.printEntregue();
                    String e = i.lerString();
                    if(m.op5Transp(e,cod) ==1);
                    else v.printNonE();
                        break;
                case 6:
                    op6(v,this.cod);
                    break;
                default:
                    v.printError();
                    break;
            }

        }while(o!=0);
    }


    public void op4(ViewTransp t, int op){
        InterfaceInput i = new Input();
        switch (op){
            case 0:
                break;
            case 1:
//                v.flush();
//                v.pressioneEnter();
                t.altNome();
                String nome = i.lerString();
                m.op3TranspNome(nome,cod);
                break;
            case 2:
//                v.flush();
//                v.pressioneEnter();
                t.passordAntiga();
                String passAnt = i.lerString();
                if (passAnt.equals(m.getTransportadora(cod).getPass())){
                    t.passordNova();
                    String nova = i.lerString();
                    m.op3TranspPass(nova,cod);
                }
                else t.passError();
                break;
            case 3:
//                v.flush();
//                v.pressioneEnter();
                t.altloc();
                double lat = i.lerDouble();
                t.altloclon();
                double lon = i.lerDouble();
                m.op3TranspGPS(lat,lon,cod);
                break;
            case 4:
                t.altRaio();
                double raio = i.lerDouble();
                m.op3TranspRaio(raio,cod);
                t.raioSuc();
                break;
            case 5:
                t.altT();
                double taxa = i.lerDouble();
                m.op3TranspTaxa(taxa,cod);
                break;
            case 6:
                t.altTP();
                double taxap = i.lerDouble();
                m.op3TranspTaxap(taxap,cod);
                break;
            case 7:
                t.altMedico();
                int op7 = -1;
                boolean op7b = false;
                while (!op7b){
                    op7 = i.lerInt();
                    switch (op7){
                        case 1:
                            m.getTransportadora(cod).aceitaMedicamentos(true);
                            op7b = true;
                            break;
                        case 2:
                            m.getTransportadora(cod).aceitaMedicamentos(false);
                            op7b = true;
                        default:
                            t.printError();
                            break;
                    }
                }

            default:
                t.printError();
        }
    }

    public void op6(ViewTransp v, String cod) {
        InterfaceInput i = new Input();
        try {
            v.print1stDate();
            v.ano();
            int y = i.lerInt();
            v.mes();
            int m = i.lerInt();
            v.dia();
            int d = i.lerInt();
            v.hora();
            int h = i.lerInt();
            v.minuto();
            int min = i.lerInt();
            LocalDateTime date1 = LocalDateTime.of(y, m, d, h, min);

            v.print2ndDate();
            v.ano();
            int y2 = i.lerInt();
            v.mes();
            int m2 = i.lerInt();
            v.dia();
            int d2 = i.lerInt();
            v.hora();
            int h2 = i.lerInt();
            v.minuto();
            int min2 = i.lerInt();
            LocalDateTime date2 = LocalDateTime.of(y2, m2, d2, h2, min2);

            v.printFat(date1, date2, this.m.faturado(date1, date2, this.m.getTransportadora(cod)));
        } catch (Exception e) {
            v.invalidDate();
        }
    }

}

package Controlador;

import Model.*;
import View.ViewUser;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class ControladorUser{
    private Modelo m;
    private String user;

    public ControladorUser(Modelo m, String r){
        this.m = m;
        this.user = r;
    }

    public void run(){
        InterfaceInput i = new Input();
        int o;
        do{
            ViewUser v = new ViewUser();
            v.menuUser();
            v.inst();
            o = i.lerInt();
            switch (o){
                case 0:
                    break;
                case 1:
                    try {
                        v.codEnc();
                        String e = i.lerString();
                        if (!m.getEncomendas().containsKey(e)) {
                            v.printLojas(m.getLojas());
                            v.codloja();
                            String l = i.lerString();
                            List<LinhaEncomenda> linha = new ArrayList<>();
                            int op = -1;
                            while (op != 0) {
                                v.printProdutos(m.getLoja(l).getStock());
                                v.menuEncomenda();
                                op = i.lerInt();
                                op1(op, e, l, v, linha, m.getLoja(l).getStock());
                                if (op == 4) break;
                            }

                        } else v.existe();
                    }catch(Exception e){
                        v.printInv();
                    }
                    break;
                case 2:
                    if(m.getUtilizador(user).getPedidos().size()<=0) break;
                    else{
                        v.printEnc(m.getUtilizador(user).getPedidos());
                        v.codEncC();
                        String ec = i.lerString();
                        boolean ri = true;
                        while (ri) {
                            if (m.getUtilizador(user).getPedidos().size() <= 0) ri = false;
                            else if (m.getUtilizador(user).getPedidos().containsKey(ec)) {
                                v.pedTransp(ec);
                                for (String t : m.getUtilizador(user).getPedidos().get(ec)) {
                                    //falta funçao de tempo estimado
                                    v.printTransp(m.getTransportadora(t), m.getPrecoTransp(ec, t), m.getTempoEstimado(ec, t));
                                }
                                boolean r = true;
                                while (r) {
                                    v.acceptTransp();
                                    String ct = i.lerString();
                                    if (m.getUtilizador(user).getPedidos().get(ec).contains(ct)) {
                                        m.aceite(user, ct, ec);
                                        r = false;
                                    } else v.printNonT();
                                }
                                ri = false;
                            } else v.printNonE();
                        }
                    }
                    break;
                case 3:
                    v.histEnc(m.getUtilizador(user).getEntregues());
                    break;
                case 4:
                    List<Encomenda> cl = listaCl(m.getUtilizador(user).getEntregues());
                    if(cl.size()<=0){
                        v.vazia();
                        break;
                    }
                    else {
                        v.classifica(cl);
                        String en = i.lerString();
                        int op4 = -1;
                        while (op4 == -1) {
                            v.classificacao();
                            op4 = i.lerInt();
                            m.op4(en, op4);
                            v.obg();
                        }
                    }
                    break;
                case 5:
                    int op5 = -1;
                    while (op5!=0) {
                        v.flush();
                        v.printDadosAtuais(m.getUtilizador(user));
                        v.pressioneEnter();
                        i.lerString();
                        v.flush();
                        v.printMenuDados();
                        v.inst();
                        op5 = i.lerInt();
                        opU(v, op5);
                    }
                    break;

                default:
                    System.out.println("Opçao invalida!");
            }

        }while(o!=0);
    }


    public void op1(int op, String enc, String loja, ViewUser v, List<LinhaEncomenda> linha, Set<Produto> produtos){
        InterfaceInput i = new Input();
        switch(op){
            case 0:
                break;
            case 1:
                v.codProd();
                String p = i.lerString();
                if(m.existeProd(p,produtos)){
                    v.quantp();
                    int q = i.lerInt();
                    Produto pr = getProd(p,produtos);
                    LinhaEncomenda li = new LinhaEncomenda(p,pr.getNome(),q,pr.getPeso(),pr.getPreçouni() * q);
                    linha.add(li);
                    v.pressioneEnter();
                    i.lerString();
                    v.flush();
                }
                else{
                    v.prodInv();
                }
                break;
            case 2:
                v.remProd();
                String p1 = i.lerString();
                if(existeProd(p1,linha));
                else{
                    v.prodInv();
                }

                v.pressioneEnter();
                i.lerString();
                v.flush();
                break;
            case 3:
                v.getEstadoEnc(linha);
                v.pressioneEnter();
                i.lerString();
                v.flush();
                break;
            case 4:
                if(linha.size()<=0){
                    v.encVazia();
                }
                else {
                    Encomenda encomenda = new Encomenda(enc,this.user,loja, getPesoLinha(linha), linha);
                    encomenda.setMedica(m.isMedica(linha));
                    m.op1User_3(encomenda);
                    v.succes();
                }
                break;
            default:
                v.printError();
                break;
        }
    }

    public boolean existeProd(String cod, List<LinhaEncomenda> l){
        Iterator<LinhaEncomenda> it = l.iterator();
        boolean found = false;
        while(it.hasNext() && !found){
            LinhaEncomenda li = it.next();
            if(li.getCod().equals(cod)) {
                found = true;
                l.remove(li);
            }
        }
        return found;
    }

    public double getPesoLinha(List<LinhaEncomenda> linha){return linha.stream().mapToDouble(LinhaEncomenda::getPeso).sum();
    }

    public Produto getProd(String cod, Set<Produto> p){
        Iterator<Produto> it = p.iterator();
        boolean found = false;
        Produto pr = new Produto();
        while(it.hasNext() && !found){
            pr = it.next();
            if(pr.getCod().equals(cod)) {
                found = true;
            }
        }
        return pr;
    }
    public void opU(ViewUser v, int op){
        InterfaceInput i = new Input();
        switch (op){
            case 0:
                break;
            case 1:
//                v.flush();
//                v.pressioneEnter();
                v.altNome();
                String nome = i.lerString();
                m.opUNome(nome, user);
                break;
            case 2:
//                v.flush();
//                v.pressioneEnter();
                v.passordAntiga();
                String passAnt = i.lerString();
                if (passAnt.equals(m.getUtilizador(user).getPass())){
                    v.passordNova();
                    String nova = i.lerString();
                    m.opUPass(nova,user);
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
                m.opUGPS(lat,lon,user);
                break;
            default:
                v.printError();
        }
    }



    public List<Encomenda> listaCl(List<Encomenda> list){
        List<Encomenda> aux = new ArrayList<>();
        if(list.size() <=0) return aux;
        else{
            for(Encomenda e: list){
                if(e.getEntregue() && e.getClassificacao() ==-1)
                    aux.add(e);
            }
        }
        return aux;
    }
}



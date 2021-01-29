package Projeto;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TrazAqui {
    private Utilizadores utilizadores;
    private EncomendasAceites encomendasAceites;
    //private Voluntário voluntário; // Talvez tenha de mudar estas variaveis
    //private Empresa empresa;
    private ArrayList<Loja> lojas;
    private ArrayList<Empresa> empresas;
    private HashMap<String,Encomenda> encProntas;
    private ArrayList<Encomenda> encomendas;

    public TrazAqui() {
        this.utilizadores = new Utilizadores();
        this.encomendasAceites = new EncomendasAceites();
        //this.voluntário = new Voluntário();
        //this.empresa = new Empresa();
        this.lojas = new ArrayList<Loja>();
        this.empresas = new ArrayList<Empresa>();
        this.encProntas = new HashMap<String, Encomenda>();
        this.encomendas = new ArrayList<Encomenda>();
    }

    public Utilizador logIn(String username, String password) {
        Utilizador util = utilizadores.getUtilizador(username);
        if (!util.getPassword().equals(password))
            System.out.println("Password errada");
        return util;
    }

    public void adicionaUtilizador(Utilizador user) {
        this.utilizadores.addUtilizador(user);
    }

    public void adicionaEncAceite(Encomenda enc) {
        this.encomendasAceites.addEncomendaAceite(enc);
    }

    public void addLojas(Loja loja) {
        lojas.add(loja);
    }

    public void addEmpresa(Empresa empresa){
        empresas.add(empresa);
    }


    public void printLojas() {
        this.lojas.toString();
    }

    public Encomenda getEncomenda(ArrayList<Encomenda> encomendas,String code){
        for (int i = 0; i < encomendas.size() ; i++) {
            if (encomendas.get(i).getCode().equals(code)) return encomendas.get(i);
        }
        return null;
    }

    public Loja getLoja(String code) {
        for (Loja e : this.lojas)
            if ((e.getEmail().equals(code)))
                return e;

        return null;
    }
    public ArrayList<Loja> getLojas(){
        return this.lojas;
    }

    public int getSize(){
        return this.lojas.size();
    }

    public void addEncomenda(Encomenda enc){
        this.encomendas.add(enc);
    }

    public ArrayList<Encomenda> getEncomendas() {
        return encomendas;
    }

    public void respondEnc(Encomenda enc){

    }

    public Utilizadores getUtilizadores() {
        return utilizadores;
    }

    public EncomendasAceites getEncomendasAceites() {
        return encomendasAceites;
    }

    public HashMap<String, Encomenda> getEncProntas() {
        return encProntas;
    }

    public Empresa calculateClosestBuilding(Pedido pedido){
        Posicao posicaoEncomendaLoja = pedido.getLoja().getPos();
        double[] distancias = new double[empresas.size()];
        HashMap<Double,Empresa> fullList = new HashMap<Double,Empresa>();

        for (int i = 0; i < empresas.size(); i++) {
            distancias[i] = calculateDistanceBetweenPoints(posicaoEncomendaLoja.getX(),
                    posicaoEncomendaLoja.getY(),
                    empresas.get(i).getPos().getX(),
                    empresas.get(i).getPos().getY());
            fullList.put(distancias[i],empresas.get(i));
        }
        double distanciaMaisPerto = getMin(distancias);
        return fullList.get(distanciaMaisPerto);
    }

    // Method for getting the minimum value
    public double getMin(double[] inputArray){
        double minValue = inputArray[0];
        for(int i=1;i<inputArray.length;i++){
            if(inputArray[i] < minValue){
                minValue = inputArray[i];
            }
        }
        return minValue;
    }


    public double calculateDistanceBetweenPoints(
            double x1,
            double y1,
            double x2,
            double y2) {
        return Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1));
    }


    @Override
    public String toString() {
        return "TrazAqui{" +
                "utilizadores=" + utilizadores +
                ", encomendasAceites=" + encomendasAceites +
                ", lojas=" + lojas +
                ", pedidos=" + encProntas +
                '}';
    }
}

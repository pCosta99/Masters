package Model;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import Exceptions.*;
import static java.lang.Math.*;

/**
 * Classe que representa as lojas
 */
public class Lojas implements Serializable{
    private static final long serialVersionUID = 1915030980796881235L;
    public String password;
    public String codLoja;
    public String nome;
    private double lat;
    private double lon;
    private double tpAtend; 
    public boolean filaEspera;
    public List<EncomendaAceite> encA; 
    public List<Encomendas> encR;
    public List<LinhaEncomenda> produtos;


    public Lojas(){
        this.password=" ";
        this.codLoja=" ";
        this.nome=" ";
        this.lat=0.0;
        this.lon=0.0;
        this.tpAtend=0.0;
        this.encA= new ArrayList<>();
        this.encR = new ArrayList<>();
        this.filaEspera=false;
        this.produtos = new ArrayList<>();
    }
    public Lojas(String password, String cdL, String n,double la,double lo,double tA, boolean fE){
        this.password = password;
        this.codLoja=cdL;
        this.nome=n;
        this.lat=la;
        this.lon=lo;
        this.tpAtend=tA;
        this.encA= new ArrayList<EncomendaAceite>();
        this.encR = new ArrayList<Encomendas>();
        this.filaEspera=fE;
        this.produtos = new ArrayList<LinhaEncomenda>();
    }
    
    public Lojas(Lojas l){
        this.password=l.getPassword();
        this.codLoja=l.getCodLoja();
        this.nome=l.getNome();
        this.lat=l.getLat();
        this.lon=l.getLon();
        this.tpAtend=l.getTpAtend();
        setEncA(l.getEncA());
        setEncR(l.getEncR());
        this.filaEspera=l.isFilaEspera();
        this.produtos = l.getProdutos();
    }


    /**
     * Getter da password da loja
     */

    public String getPassword() { return password; }

    /**
     * Getter do código da loja
     */
    
    public String getCodLoja() { return codLoja; }

    /**
     * Getter do nome da loja
     */

    public String getNome() { return nome; }

    /**
     * Getter da latitude da loja
     */

    public double getLat() { return lat; }

    /**
     * Getter da longitude da loja
     */

    public double getLon() { return lon; }

    /**
     * Getter do tempo de atendimento
     */

    public double getTpAtend() { return tpAtend; }

    /**
     * Getter de se há fila de espera 
     */

    public boolean isFilaEspera() { return filaEspera; }

    /**
     * Getter da lista das encomendas registadas 
     */

    public List<Encomendas> getEncR() {
        ArrayList<Encomendas> e = new ArrayList<>();
        for(Encomendas en : this.encR)
            e.add(en);
        return e;
    }

    /**
     * Getter da lista das encomendas aceites
     */

    public List<EncomendaAceite> getEncA() {
        ArrayList<EncomendaAceite> e = new ArrayList<>();
        for(EncomendaAceite ea : this.encA)
            e.add(ea);
        return e;
    }

    /**
     * Getter da lista dos produtos da loja
     */

    public List<LinhaEncomenda> getProdutos() {
        ArrayList<LinhaEncomenda> e = new ArrayList<>();
        for(LinhaEncomenda l : this.produtos)
            e.add(l);
        return e;
    }


    /**
     * Setter do codigo da loja
     */

    public void setCodLoja(String codLoja) { this.codLoja = codLoja; }

    /**
     * Setter do nome da loja
     */

    public void setNome(String nome) { this.nome = nome; }
    
    /**
     * Setter da latitude da loja
     */

    public void setLat(double lat) { this.lat = lat; }

    /**
     * Setter da longitude da loja
     */

    public void setLon(double lon) { this.lon = lon; }

    /**
     * Setter do tempo de atendimento da loja
     */

    public void setTpAtend(double tpAtend) { this.tpAtend = tpAtend; }

    /**
     * Setter das encomendas registadas
     */

    public void setEncR(List<Encomendas> encR) {
        this.encR=new ArrayList<>();
        for(Encomendas e: encR)
            this.encR.add(e);
    }

    /**
     * Setter das encomendas aceites
     */

    public void setEncA(List<EncomendaAceite> encA) {
        this.encA=new ArrayList<>();
        for(EncomendaAceite e: encA)
            this.encA.add(e);
    }

    /**
     * Setter da fila de espera 
     */

    public void setFilaEspera(boolean filaEspera) { this.filaEspera = filaEspera; }

    
    /**
     * Método que nos permite atribuir aleatoriedade
     */

    void rent() {
        double meteorologia = new Metereologia().getEstacaoAtraso();
        double transito = new Transito().getAtrasoTransito(meteorologia);
        double atraso = (meteorologia % 0.5)+ (transito % 0.5);;
        this.tpAtend= this.tpAtend* (1 + atraso);
    }
    
    /**
     * Método que adiciona uma encomenda aceite
     */
    
    public void adicionaEncomendas(EncomendaAceite e){
                this.encA.add(e);
    }

    /**
     * Método que adicona uma encomenda registada
     */

    public void adicionaEncomendas(Encomendas e){
                this.encR.add(e);
    }
    /**
     * Método que adiciona um produto a uma linha de encomenda
     */

    public void adicionaProduto(LinhaEncomenda l) throws JaExisteProdutoException {
        List<LinhaEncomenda> lista = new ArrayList<>();
        lista = this.produtos.stream().filter(line-> line.getCodProdu().equals(l.getCodProdu())).collect(Collectors.toList());
        if(lista.size() == 0){
            this.produtos.add(l);
        }        
        else throw new JaExisteProdutoException();
    }

    /**
     * Método que produz uma encomenda na loja 
     */

    public void produzEncomenda(EncomendasDB bsdE) throws NaoExisteEncomendaException {
        while(this.encA.size()>0){
            String codE = this.encA.get(0).getCodEncomenda();
            Encomendas encRespetiva = bsdE.getEncomenda(codE);
            this.encA.get(0).swapstatePronta();
            this.encR.add(encRespetiva);
            this.encA.remove(0);
        }
    }

    /**
     * Método que retorna a lista de encomendas efetuadas nessa loja num determinado espaço de tempo
     */

    List<Encomendas> getListencLDH(LocalDateTime init, LocalDateTime end) {
        return this.encR.stream()
                .filter(e ->  e.getDataHora().isBefore(end) && e.getDataHora().isAfter(init))
                .collect(Collectors.toList());
    }
    /**
     * Método que determina a distancia entre o user e a loja
     */

    public double distanceLojaU( User u) {
        double lati1= Math.toRadians(this.lat);
        double long1=Math.toRadians(this.lon);
        double lati2= Math.toRadians(u.getLatitude());
        double long2=Math.toRadians(u.getLongitude());
        double dist = acos(sin(lati1)* sin(lati2) + cos(lati1) * cos(lati2) * cos(long1 - long2)) * 6371;
        return dist;
    }

    /**
     * Getter de uma loja dado o seu código
     */

    public Lojas getLoja(String codL){
        if(codL.equals(this.codLoja))
            return this;
        else return null;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Loja: ").append(this.codLoja).append(",")
                                     .append(this.nome).append(",")
                                     .append(this.lat).append(",")
                                     .append(this.lon).append(",")
                                     .append(this.tpAtend).append(",")
                                     .append(this.filaEspera).append(",")
                                     .append(this.encA).append(",")
                                     .append(this.encR).append(",");
        return sb.toString();
    }
    public Lojas clone(){return new Lojas(this);}

}

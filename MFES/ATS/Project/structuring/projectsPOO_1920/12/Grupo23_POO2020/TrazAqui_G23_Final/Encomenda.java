import java.io.*;
import java.lang.String;
import java.util.ArrayList;
import java.lang.Object;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Classe :: Encomenda
 * Info de uma encomenda efetuada a uma dada entidade.
 * Apresenta disponibilidade e confirmação de entrega por parte do comprador
 * e entregadores.
 */
public class Encomenda implements Serializable
{
    private static String naodisp = "NAO DISPONIVEL"; //uma loja ainda não disponibilizou esta encomenda para entrega
    private static String disp = "DISPONIVEL";        //uma loja já disponibilizou esta encomenda para entrega
    private static String nconfirm = "POR CONFIRMAR"; //um utilizador ainda não confirmou uma encomenda a ser transportada por uma empresa
    private static String confirm = "CONFIRMADO";     //um utilizador confirmou uma encomenda ser transportada por uma empresa
    private static String aceite = "ACEITE";          //uma empresa ou voluntário está a entregar esta encomenda
    private static String entregue = "ENTREGUE";      //uma empresa ou voluntario registou esta entrega
    
    private String codEnc; //código encomenda
    private String codUser; //email buyer
    private String codLoja; //email loja
    private String codEntregador; //email entregador
    private double peso; //em kgs
    private String estado;
    private List<LinhaEncomenda> le;
    
    
    public Encomenda(){
        this.codEnc = new String();
        this.codUser = new String();
        this.codLoja = new String();
        this.codEntregador = "n/a";
        this.peso = 0;
        this.estado = naodisp;
        this.le = new ArrayList<>();
    }
    
    public Encomenda(String e, String u, String l, String t, double peso, String state, List<LinhaEncomenda> le){
        this.setCEnc(e);
        this.setCUser(u);
        this.setCLoja(l);
        this.setEntregador(t);
        this.setPeso(peso);
        this.setState(state);
        this.setLEnc(le);
    }
    
    public Encomenda(Encomenda e){
        this.codEnc = e.getCEnc();
        this.codUser = e.getCUser();
        this.codLoja = e.getCLoja();
        this.codEntregador = e.getEntregador();
        this.peso = e.getPeso();
        this.estado = e.getState();
        this.le = e.getLEnc();
    }
    
    
    /**
     * Métodos get
     */
    public String getCEnc(){
        return this.codEnc;
    }
    
    public String getCUser(){
        return this.codUser;
    }
    
    public String getCLoja(){
        return this.codLoja;
    }
    
    public double getPeso(){
        return this.peso;
    }
    
    public List<LinhaEncomenda> getLEnc(){
        return this.le.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }
    
    public String getState(){
        return this.estado;
    }
    
    public String getEntregador(){
        return this.codEntregador;
    }
    
    /**
    * Métodos set
    */
    public void setCEnc(String e){
        this.codEnc = e;
    }
    
    public void setCUser(String u){
        this.codUser = u;
    }
    
    public void setCLoja(String l){
        this.codLoja = l;
    }
    
    public void setPeso(double p){
        this.peso = p;
    }
    
    public void setLEnc(List<LinhaEncomenda> le){
        this.le = le.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }
    
    public void setState(String state){
        this.estado = state;  
    }
    
    public void setEntregador(String cod){
        this.codEntregador = cod;
    }
    
        
    //Coloca a encomenda como "NAO DISPONIVEL"
    public void setNDisp(){
        this.estado = naodisp;
    }
    
    //Coloca a encomenda como "DISPONIVEL"
    public void setDisp(){
        this.estado = disp;
    }
    
    //Coloca a encomenda como "ENTREGUE"
    public void setEntregue(){
        this.estado = entregue;
    }
    
    //Coloca a encomenda como "ACEITE"
    public void setAceite(){
        this.estado = aceite;
    }
    
    //Adiciona encomenda a lista de previamente realizadas
    public void addLinhaEncomenda(LinhaEncomenda linha){
        this.le.add(linha.clone());
    }
    
    //Aumenta peso da encomenda após novo pedido
    public void acrescentaPeso(double p){
        this.peso = this.getPeso()+p;
    }
    
    //Custo total de uma encomenda
    public double custoEnc(){
        double r = 0;
        
        for(LinhaEncomenda les : this.le){
            r = r + les.calculaValorLinhaEnc();
        }
        return r;
    }
    
    
    /**
     * Clone
     */
    public Encomenda clone(){
        return new Encomenda(this);
    }
    
    /**
     * Equals
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        Encomenda e = (Encomenda) o;
        return (e.getCEnc().equals(this.codEnc) &&
                e.getCUser().equals(this.codUser) &&
                e.getCLoja().equals(this.codLoja) && 
                (e.getPeso() == this.peso) &&
                e.getLEnc().equals(this.le));
    }
    
    /**
     * toString
     */
    public String toString(){
        return "Encomenda numero: " + this.getCEnc()
                                    + " /Comprador: " + this.getCUser()
                                    + " /Loja: " + this.getCLoja()
                                    + " /Entregador: " + this.getEntregador()
                                    + " /Peso: " + this.getPeso()
                                    + " /Estado: " + this.getState()
                                    + " /Produtos: " + this.getLEnc();
    }
    
    /**
     * toFile
     */
    public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda:"+this.getCEnc());
        sb.append(","+this.getCUser());
        sb.append(","+this.getCLoja());
        sb.append(","+this.getEntregador());
        sb.append(","+this.getPeso());
        sb.append(","+this.getState());
        
        for(LinhaEncomenda les : this.le){
            sb.append(","+les.stringtoFile());
        }
        
        return(sb.toString());
    }
}

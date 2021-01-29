package Model;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Classe que representa cada Encomenda
 */

public class Encomendas implements Serializable{
    private static final long serialVersionUID = 5834639776302335022L;
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private ArrayList<LinhaEncomenda> linha;
    private LocalDateTime dataHora;
    public boolean isMed;
    private boolean pronta;

    public Encomendas(){
        this.codEncomenda="";
        this.codUtilizador="";
        this.codLoja=" ";
        this.peso=0.0;
        this.dataHora = LocalDateTime.now();
        this.isMed=false;
        this.pronta=false;
        this.linha = new ArrayList<>();
    }
    public Encomendas(String codE,String u,String l,double p,boolean iM, ArrayList<LinhaEncomenda> enc){
        this.codEncomenda=codE;
        this.codUtilizador=u;
        this.codLoja=l;
        this.peso=p;
        this.isMed=iM;
        this.dataHora = LocalDateTime.now();
        this.pronta=false;
        setLinha(enc);
    }
    public Encomendas(Encomendas e){
        this.codEncomenda=e.getCodEncomenda();
        this.codUtilizador=e.getCodUtilizador();
        this.codLoja=e.getNomeLoja();
        this.peso=e.getPeso();
        this.isMed=e.isMed();
        this.dataHora=e.getDataHora();
        this.pronta=e.getPronta();
        setLinha(e.getLinha());
    }

    /**
     * Getter do código da encomenda
     */

    public String getCodEncomenda() { return this.codEncomenda; }

    /**
     * Getter do código de utilizador da encomenda
     */

    public String getCodUtilizador() { return this.codUtilizador; }

    /**
     * Getter do nome da loja da encomenda
     */

    public String getNomeLoja() { return codLoja; }

    /**
     * Getter do peso da encomenda
     */

    public double getPeso() { return peso; }

    /**
     * Getter da data e hora da encomenda
     */

    public LocalDateTime getDataHora(){ return dataHora;}

    /**
     * Getter de se a encomenda é médica ou não
     */

    public boolean isMed() { return isMed; }

    /**
     * Getter de se a encomenda está pronta ou não
     */

    public boolean getPronta(){return pronta;}

    /**
     * Getter da linha da encomenda
     */

    public ArrayList<LinhaEncomenda> getLinha() {
        ArrayList<LinhaEncomenda> aux = new ArrayList<>();
        Iterator<LinhaEncomenda> it = this.linha.iterator();

        while(it.hasNext()) {
            LinhaEncomenda s = it.next();
            aux.add(s);
        }

        return aux;
    }

    /**
    * Setter do codigo da encomenda
    */

    public void setCodEncomenda(String codEncomenda) { this.codEncomenda = codEncomenda; }

    /**
    * Setter do codigo de utilizador da encomenda
    */

    public void setCodUtilizador(String codUtilizador) { this.codUtilizador = codUtilizador; }

    /**
    * Setter do codigo da loja da encomenda
    */

    public void setCodLoja(String codLoja) { this.codLoja = codLoja; }

    /**
    * Setter do peso da encomenda
    */

    public void setPeso(double peso) { this.peso = peso; }

    /**
    * Setter de se a encomenda é médica ou não
    */

    public void setIsMed(boolean med) { isMed = med; }

    /**
    * Setter de se a encomenda está pronta ou não
    */

    public void setPronta(boolean pronta) { this.pronta = pronta; }

    /**
    * Setter da linha da encomenda
    */

    public void setLinha(ArrayList<LinhaEncomenda> enc) {
        this.linha = new ArrayList<>();
        Iterator<LinhaEncomenda> it = enc.iterator();

        while(it.hasNext()) {
            LinhaEncomenda s = it.next();
            this.linha.add(s);
        }

    }

    
    /**
     * Método que nos dá o tamanho da encomenda
     */

    public int tamanhoEnc(){
        return this.linha.size();
    }

    /**
    * Método que adiciona uma linha á encomenda
    */

    public void adicionaLinha (LinhaEncomenda le){
        linha.add(le);
        this.peso += le.getPesoLinha();
    }

    /**
    * Método que retorna o custo total da encomenda
    */

    public double custoEncomenda(){
        double total=0;
        for(LinhaEncomenda l : this.linha)
            total+=l.ValorLinhaEnc();
        return total;
    }

    /**
    * Método que altera o estado da encomenda
    */

    public void swapstatePronta(){
        this.pronta=!this.pronta;
    }

    public Encomendas clone() { return new Encomendas (this); }

    public String toString()
    { StringBuilder sb = new StringBuilder();
        sb.append("[Encomenda:").append(" cod: ").append(this.codEncomenda).append(",")
                                .append(" utilizador ").append(this.codUtilizador).append(",")
                                .append(" loja ").append(this.codLoja).append(",")
                                .append(" peso ").append(this.peso).append(",")
                                .append(this.linha).append(",")
                                .append(" Data e Hora ").append(this.dataHora).append(",")
                                .append(" É medica? ").append(this.isMed).append("]\n");
        return sb.toString();
    }


}

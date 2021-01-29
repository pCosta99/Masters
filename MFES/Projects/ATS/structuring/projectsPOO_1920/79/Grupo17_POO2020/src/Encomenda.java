

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.HashMap;

public class Encomenda implements Serializable
{
    /** identificador de encomenda */
    private String codEncomenda;

    /** codigo de utilizador que fez a encomenda */
    private String destinatario;

    /** codigo de Loja que vendeu a encomenda */
    private String vendedor;

    /** peso total da encomenda */
    private double pesoEnc;

    /** conjunto de produtos da encomenda associados aos seus identificadores de produto  */
    private Map<String,LinhaEnc> produtos;

    /** data e hora do inicio da encomenda */
    private LocalDateTime inicio;

    /** data e hora do fim da encomenda */
    private LocalDateTime fim;

    /** tempo de entrega da encomenda */
    private String tempoEntrega;//Guarda o tempo de uma entrega

    /** kilometros feitos pela entidade de transporte na entrega */
    private double kmDeslocacao;//guarda os km feitos pelo transportador na entrega

    /** coordenada x do utilizador associado a encomenda */
    private double coorXU;

    /** coordenada y do utilizador associado a encomenda */
    private double coorYU;

    /** coordenada x da loja associado a encomenda */
    private double coorXL;

    /** coordenada y da loja associado a encomenda */
    private double coorYL;

    /** coordenada x da entidade de transporte associado a encomenda */
    private double coorXT;

    /** coordenada y da entidade de transporte associado a encomenda */
    private double coorYT;


    /**
     * Construtor por omissão de Encomenda
     */
    public Encomenda(){
        this.destinatario = new String();
        this.vendedor = new String();
        this.pesoEnc = 0.0;
        this.produtos = new HashMap<>();
        this.codEncomenda = new String();
        this.tempoEntrega = new String();
        this.inicio = LocalDateTime.of(2000,1,1,0,0);
        this.fim = LocalDateTime.of(2000,1,1,0,0);
        this.kmDeslocacao = 0;
        this.coorXU = 0;
        this.coorYU = 0;
        this.coorXL = 0;
        this.coorYL = 0;
        this.coorXT = 0;
        this.coorYT = 0;
    }


    /**
     * Construtor parametrizado de Encomenda
     * @param destinatario codigo de utilizador que fez a encomenda
     * @param vendedor codigo de Loja que vendeu a encomenda
     * @param pesoEnc peso total da encomenda
     * @param produtos onjunto de produtos da encomenda associados aos seus identificadores de produto
     * @param codEncomenda identificador de encomenda
     */
    public Encomenda(String destinatario,String vendedor,double pesoEnc,Map<String,LinhaEnc> produtos,String codEncomenda,double xu,double yu,double xl,double yl,double xt,double yt){
        setDestinatario(destinatario);
        setVendedor(vendedor);
        setPesoEnc(pesoEnc);
        setProdutos(produtos);
        setCodEncomenda(codEncomenda);
        this.tempoEntrega = new String();
        this.inicio = LocalDateTime.of(2000,1,1,0,0);
        this.fim = LocalDateTime.of(2000,1,1,0,0);
        this.kmDeslocacao = 0;
        this.coorXU = xu;
        this.coorYU = yu;
        this.coorXL = xl;
        this.coorYL = yl;
        this.coorXT = xt;
        this.coorYT = yt;
    }

    /**
     * Construtor por cópia de Encomenda
     * @param e Encomenda
     */
    public Encomenda(Encomenda e){
        setDestinatario(e.getDestinatario());
        setVendedor(e.getVendedor());
        setPesoEnc(e.getPesoEnc());
        setProdutos(e.getProdutos());
        setCodEncomenda(e.getCodEncomenda());
        setTempoEntrega(e.getTempoEntrega());
        setInicio(e.getInicio());
        setFim(e.getFim());
        setKmDeslocacao(e.getKmDeslocacao());
        alteraParametros(e.getCoorXU(),e.getCoorYU(),e.getCoorXL(),e.getCoorYL(),e.getCoorXT(),e.getCoorYT());
        
    }

    /**
     * Getter do codigo de utilizador que fez a encomenda
     * @return codigo de utilizador que fez a encomenda
     */
    public String getDestinatario(){return this.destinatario;}


    /**
     * Getter do codigo de Loja que vendeu a encomenda
     * @return codigo de Loja que vendeu a encomenda
     */
    public String getVendedor(){return this.vendedor;}

    /**
     * Getter do peso total da encomenda
     * @return peso total da encomenda
     */
    public double getPesoEnc(){return this.pesoEnc;}

    /**
     * Getter dos produtos da encomenda
     * @return produtos da encomenda
     */
    public Map<String,LinhaEnc> getProdutos(){
        Map<String,LinhaEnc> aux = new HashMap<>();
        for(Map.Entry<String,LinhaEnc> l : this.produtos.entrySet())
            aux.put(l.getKey(),l.getValue().clone());
        return aux;
    }

    /**
     * Getter do identificador da encomenda
     * @return identificador da encomenda
     */
    public String getCodEncomenda(){return this.codEncomenda;}

    /**
     * Getter da data e hora do inicio da encomenda
     * @return data e hora do inicio da encomenda
     */
    public LocalDateTime getInicio(){ return this.inicio;}

    /**
     * Getter da data e hora do fim da encomenda
     * @return data e hora do fim da encomenda
     */
    public LocalDateTime getFim(){ return this.fim;}

    /**
     * Getter do tempo de entrega da encomenda
     * @return tempo de entrega da encomenda
     */
    public String getTempoEntrega(){return this.tempoEntrega;}

    /**
     * Getter dos kilometros feitos pela entidade de transporte na entrega
     * @return kilometros feitos pela entidade de transporte na entrega
     */
    public double getKmDeslocacao(){return this.kmDeslocacao;}

    /**
     * Getter da coordenada x do utilizador associado a encomenda
     * @return  oordenada x do utilizador associado a encomenda
     */
    public double getCoorXU(){return this.coorXU;}

    /**
     * Getter da coordenada y do utilizador associado a encomenda
     * @return coordenada y do utilizador associado a encomenda
     */
    public double getCoorYU(){return this.coorYU;}

    /**
     * Getter da coordenada x da loja associado a encomenda
     * @return coordenada x da loja associado a encomenda
     */
    public double getCoorXL(){return this.coorXL;}

    /**
     * Getter da coordenada y da loja associado a encomenda
     * @return coordenada y da loja associado a encomenda
     */
    public double getCoorYL(){return this.coorYL;}

    /**
     * Getter da coordenada x da entidade de transporte associado a encomenda
     * @return coordenada x da entidade de transporte associado a encomenda
     */
    public double getCoorXT(){return this.coorXT;}

    /**
     * Getter da coordenada y da entidade de transporte associado a encomenda
     * @return coordenada y da entidade de transporte associado a encomenda
     */
    public double getCoorYT(){return this.coorYT;}

    /**
     * Setter do codigo de utilizador que fez a encomenda
     * @param novoDest novo codigo de utilizador que fez a encomenda
     */
    public void setDestinatario(String novoDest){this.destinatario = novoDest;}

    /**
     * Setter do codigo de Loja que vendeu a encomenda
     * @param novoVend novo  odigo de Loja que vendeu a encomenda
     */
    public void setVendedor(String novoVend){this.vendedor = novoVend;}

    /**
     * Setter do peso total da encomenda
     * @param novoPeso novo peso total da encomenda
     */
    public void setPesoEnc(double novoPeso){this.pesoEnc = novoPeso;}

    /**
     * Setter dos produtos da encomenda
     * @param novosProdutos novos produtos da encomenda
     */
    public void setProdutos(Map<String,LinhaEnc> novosProdutos){
        this.produtos = new HashMap<>();

        for(Map.Entry<String,LinhaEnc> l : novosProdutos.entrySet())
            this.produtos.put(l.getKey(),l.getValue().clone());
    }

    /**
     * Setter da data e hora do inicio da encomenda
     * @param novo nova data e hora do inicio da encomenda
     */
    public void setInicio(LocalDateTime novo){this.inicio = novo; }

    /**
     * Setter da data e hora do fim da encomenda
     * @param novo nova data e hora do fim da encomenda
     */
    public void setFim(LocalDateTime novo){ this.fim = novo;}

    /**
     * Setter do tempo de entrega de uma encomenda
     * @param novo novo tempo de entrega
     */
    public void setTempoEntrega(String novo){ this.tempoEntrega = novo;}

    /**
     * Setter do identificador de encomenda
     * @param novoCodEnc novo identificador de encomenda
     */
    public void setCodEncomenda(String novoCodEnc){this.codEncomenda = novoCodEnc;}

    /**
     * Setter dos kilometros realizados numa entrega
     * @param novo novos kilometros realizados numa entrega
     */
    public void setKmDeslocacao(double novo){this.kmDeslocacao = novo;}

    /**
     * Metodo que altera as coordenadas do utilizador, loja e entidade de entrega associados a uma encomenda
     * @param xu coordenada x do utilizador
     * @param yu coordenada y do utilizador
     * @param xl coordenada x da loja
     * @param yl coordenada y da loja
     * @param xt coordenada x da entidade de entrega
     * @param yt coordenada y da entidade de entrega
     */
    public void alteraParametros(double xu,double yu,double xl,double yl,double xt,double yt){
        this.coorXU = xu;
        this.coorYU = yu;
        this.coorXL = xl;
        this.coorYL = yl;
        this.coorXT = xt;
        this.coorYT = yt;
    }


    /**
     * Metodo clone de uma encomenda
      * @return clone da encomenda
     */
    public Encomenda clone(){return new Encomenda(this);}

    /**
     * Metodo equals de encomenda
     * @param o Objeto
     * @return verificação se as encomendas sáo iguais
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass()!=this.getClass()) return false;
        Encomenda e = (Encomenda) o;
        return this.codEncomenda.equals(e.getCodEncomenda()) &&
                this.destinatario.equals(e.getDestinatario()) &&
                this.vendedor.equals(e.getVendedor()) &&
                this.pesoEnc == e.getPesoEnc() &&
                this.produtos.equals(e.getProdutos()) &&
                this.inicio.equals(e.getInicio()) &&
                this.fim.equals(e.getFim()) &&
                this.tempoEntrega.equals(e.getTempoEntrega()) &&
                this.kmDeslocacao == e.getKmDeslocacao() &&
                this.getCoorXU() == e.getCoorXU() &&
                this.getCoorYU() == e.getCoorYU() &&
                this.getCoorXL() == e.getCoorXL() &&
                this.getCoorYL() == e.getCoorYL() &&
                this.getCoorXT() == e.getCoorXT() &&
                this.getCoorYT() == e.getCoorYT();
    }

    /**
     * Metodo toString de encomenda
      * @return String
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda: ").append(this.destinatario).append(",")
                .append(this.vendedor).append(",").append(this.pesoEnc).append(",")
                .append(this.produtos);
        return sb.toString();
    }


    /**
     * Metodo que devolve uma String com o identificador de encomenda
     * @return String
     */
    public String toStringEcra(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.codEncomenda);
        return sb.toString();
    }


    /**
     * Metodo que insere um novo produto numa encomenda
     * @param l novo produto
     */
    public void insereLinhaEnc(LinhaEnc l){
        this.pesoEnc += (l.getPesoUni() * l.getQuantidade());
        this.produtos.put(l.getCodProduto(),l.clone());
    }


    /**
     * Metodo que calcula o valor de uma Encomenda (sem ter em conta o peso dela)
     * @return valor de uma Encomenda
     */
    public double calculaValorEnc(){ return this.produtos.values().stream().mapToDouble(LinhaEnc::calculaValorLinha).sum(); }


    /**
     * Metodo devolve uma String com o inicio, fim e tempo de entrega de uma encomenda
     * @return String
     */
    public String imprimeTempos(){
        StringBuilder s = new StringBuilder();
        LocalDateTime a = LocalDateTime.of(2000,1,1,0,0);
        s.append("Encomenda iniciada as: ");
        if(this.inicio.equals(a))s.append("nao atribuido");
        else s.append(this.inicio);
        s.append(" , ").append("Encomenda finalizada as: ");
        if(this.fim.equals(a)) s.append("nao atribuido");
        else s.append(this.fim);
        s.append(" , ").append("Tempo de entrega: ");
        if(this.tempoEntrega.equals("")) s.append("nao atribuido");
        else s.append(this.tempoEntrega).append("\n");
        return s.toString();
    }


    /**
     * Metodo que verifica se a encomenda ẽ de teor medico
     * @return boolean
     */
    public boolean eEncomendaMedica(){ return this.produtos.values().stream().anyMatch(LinhaEnc::getProdMed); }


    /**
     * Metodo que calcula a distancia total desde da entidade de transporte á loja e desta ao utilizador
     * @return distancia total desde da entidade de transporte á loja e desta ao utilizador
     */
    public double calculaDistancia(){
        return Math.sqrt(Math.pow(getCoorXT()-getCoorXL(),2) + Math.pow(getCoorYT()-getCoorYL(),2))
                + Math.sqrt(Math.pow(getCoorXL()-getCoorXU(),2) + Math.pow(getCoorYL()-getCoorYU(),2));
    }
}
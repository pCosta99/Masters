


import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;

public class Loja extends Entidade implements Serializable
{
    /** variavel que verifica se existem encomendas para serem entregues
     * true-ha encomendas,false- não ha encomendas para entregar */
    private boolean haEnc;

    /** conjunto de encomendas para entregar */
    private Map<String,Encomenda> encomendas;

    /** conjunto de produtos disponiveis na loja */
    private Map<String,LinhaEnc> catalogo;

    /**
     * Construtor por omissão de loja
     */
    Loja(){
        super();
        this.haEnc = false;
        this.encomendas = new TreeMap<>();
        this.catalogo = new TreeMap<>();
    }

    /**
     * Construtor parametrizado de loja
     * @param codLoja identificador da entidade
     * @param nomeLoja nome da entidade
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param registos conjunto das encomendas feitas por uma entidade
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     */
    Loja(String codLoja,String nomeLoja,double xGPS,double yGPS,Map<String,Encomenda> registos, String email, String pass){
        super(nomeLoja,codLoja,xGPS,yGPS,registos,email,pass);
        this.haEnc = false;
        this.encomendas = new TreeMap<>();
        this.catalogo = new TreeMap<>();
    }

    /**
     * Construtor por copia de loja
     * @param l loja
     */
    Loja(Loja l){
        super(l.getNome(),l.getCodigo(),l.getXGPS(),l.getYGPS(),l.getRegistos(),l.getEmail(),l.getPass());
        setHaEnc(l.getHaEnc());
        setEncomendas(l.getEncomendas());
        setCatalogo(l.getCatalogo());
    }

    /**
     * Getter da verificação se há encomendas para entregar
     * @return verificação se há encomendas para entregar
     */
    public boolean getHaEnc(){return this.haEnc;}

    /**
     * Getter do conjunto de encomendas para entregar
     * @return conjunto de encomendas para entregar
     */
    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> aux = new TreeMap<>();
        for(Map.Entry<String,Encomenda> e : this.encomendas.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    /**
     * Getter do conjunto de produtos disponiveis na loja
     * @return conjunto de produtos disponiveis na loja
     */
    public Map<String,LinhaEnc> getCatalogo(){
        Map<String,LinhaEnc> aux = new TreeMap<>();
        for(Map.Entry<String,LinhaEnc> e : this.catalogo.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }


    /**
     * Setter da verificação se há encomendas para entregar
     * @param novo nova verificação se há encomendas para entregar
     */
    public void setHaEnc(boolean novo){this.haEnc = novo;}

    /**
     * Setter do conjunto de encomendas para entregar
     * @param e novo conjunto de encomendas para entregar
     */
    public void setEncomendas(Map<String,Encomenda> e){
        this.encomendas = new TreeMap<>();
        e.entrySet().forEach(l->this.encomendas.put(l.getKey(),l.getValue().clone()));
    }

    /**
     * Setter do conjunto de produtos disponiveis na loja
     * @param e novo conjunto de produtos disponiveis na loja
     */
    public void setCatalogo(Map<String,LinhaEnc> e){
        this.catalogo = new TreeMap<>();
        e.entrySet().forEach(l->this.catalogo.put(l.getKey(),l.getValue().clone()));
    }

    /**
     * Metodo clone de loja
     * @return clone de loja
     */
    public Loja clone(){return new Loja(this);}

    /**
     * Metodo equals de loja
     * @param o Objeto
     * @return booleano
     */
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass()!=this.getClass()) return false;
        Loja l = (Loja) o;
        return super.getCodigo().equals(l.getCodigo()) &&
                super.getNome().equals(l.getNome()) &&
                super.getXGPS() == l.getXGPS() &&
                super.getYGPS() == l.getYGPS() &&
                super.getRegistos().equals(l.getRegistos()) &&
                this.haEnc == l.getHaEnc() &&
                this.encomendas.equals(l.getEncomendas()) &&
                this.catalogo.equals(l.getCatalogo());
    }

    /**
     * Metodo toString de loja
      * @return String
     */
    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append("\nLoja:\n").append(super.toString()).append(" | Ha encomendas para entregar: ");
        if(this.haEnc) s.append("Sim ");
        else s.append("Nao ");
        s.append(" | Encomendas para serem entregues: ")
                .append(this.encomendas).append(" | Artigos Disponiveis na Loja: ").append(this.catalogo);
        return s.toString();
    }

    /**
     * Metodo que retorna uma String com o codigo, nome e coordenadas da loja
     * @return String
     */
    public String toStringCSV(){
        StringBuilder s = new StringBuilder();
        s.append("Loja:").append(super.getCodigo()).append(",").append(super.getNome()).append(",").append(super.getXGPS()).append(",").append(super.getYGPS()).append("\n");
        return s.toString();
    }



    /**
     * Adiciona uma Encomenda á lista de Encomendas a serem entregues
     * @param e encomenda a inserir
     */
    public void insereEncEntregar(Encomenda e){this.encomendas.put(e.getCodEncomenda(),e.clone());}



    /**
     * Adicionar um novo produto ao catalogo da loja
     * @param codProduto identificador de produto
     * @param descricao descrição do produto
     * @param valorUni valor unitário do produto
     * @param peso peso do produto
     * @param prodMed verificação se é um produto médico
     * @throws ValoresNaoValidosException
     */
    public void insereNovoProduto(String codProduto, String descricao, double valorUni, double peso, boolean prodMed) throws ValoresNaoValidosException {
        if(peso<0 || valorUni<0) throw new ValoresNaoValidosException();
        else {
            LinhaEnc l = new LinhaEnc(codProduto,descricao,0,valorUni,peso,prodMed);
            this.catalogo.put(codProduto,l.clone());
        }
    }


    /**
     * Metodo que adiciona um conjunto de produtos ao catalogo
     * @param produtos conjunto de produtos a ser adicionado
     */
    public void insereProdutosCat(Map<String,LinhaEnc> produtos){ this.catalogo.putAll(produtos); }



    /**
     * Metodo que dado o codigo de produto devolve a linha de encomenda do catalogo com esse codigo
     * @param cod identificador de produto
     * @return produto do catalogo
     * @throws CodigoInvalidoException
     */
    public LinhaEnc getLinhaEncomenda(String cod) throws CodigoInvalidoException {
        if(!this.catalogo.containsKey(cod)) throw new CodigoInvalidoException(cod);
        else
            return this.catalogo.get(cod).clone();
    }


    /**
     * Metodo que sinaliza que há encomendas a entregar
     */
    public void haEncEntregar(){
        int tam = this.encomendas.values().size();
        setHaEnc(tam > 0);
    }


    /**
     * Metodo que devolve o numero de produtos no catalogo da loja
     * @return numero de produtos no catalogo da loja
     */
    public int nmrProdCat(){return this.catalogo.size();}


    /**
     * Metodo que remove um produto do catalogo
     * @param codProd identificador do produto a remover
     */
    public void removeProduto(String codProd){ this.catalogo.remove(codProd); }


    /**
     * Metodo que remove uma encomenda das encomendas a fazer
     * @param codEnc identificador da encomenda a remover
     */
    public void removeEnc(String codEnc) { this.encomendas.remove(codEnc); }


}
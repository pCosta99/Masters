

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

public class Transportadora extends EntidadeEntrega implements Serializable
{
    /** Numero de identificação fiscal */
    private String nif;

    /** preço por kilometro */
    private double taxa;

    /** capacidade/número maximo encomendas que pode transportar numa viagem */
    private int capacidade;

    /** conjunto de encomendas que chegam feitas de um utilizador */
    private Map<String,Encomenda> encomendasAfazer;

    /**
     * Construtor por omissão de Transportadora
     */
    Transportadora(){
        super();
        this.nif = new String();
        this.taxa = 0;
        this.capacidade = 0;
        this.encomendasAfazer = new TreeMap<>();
    }

    /**
     * Construtor parametrizado de Transportadora
     * @param codEmpresa identificador da entidade
     * @param nomeEmpresa nome da entidade
     * @param nif Numero de identificação fiscal
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param taxa preço por kilometro
     * @param raio raio de circulação
     * @param capacidade número maximo encomendas que pode transportar numa viagem
     * @param aceitaMed verificação se aceita transporte de encomendas médicas
     * @param registos conjunto das encomendas feitas por uma entidade
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     * @param cla classificação geral
     * @param ser quantidade de serviços
     * @param kms kilometros totais efetuados
     */
    Transportadora(String codEmpresa,String nomeEmpresa,String nif,double xGPS,double yGPS,double taxa,double raio,int capacidade,boolean aceitaMed,Map<String,Encomenda> registos, String email, String pass,double cla,int ser, double kms){
        super(nomeEmpresa,codEmpresa,xGPS,yGPS,raio,aceitaMed,registos,email,pass,cla,ser,kms);
        this.nif = nif;
        this.taxa = taxa;
        this.capacidade = capacidade;
        this.encomendasAfazer = new TreeMap<>();
    }

    /**
     * Construtor por cópia de Transportadora
     * @param t transportadora
     */
    Transportadora(Transportadora t){
        super(t.getNome(),t.getCodigo(),t.getXGPS(),t.getYGPS(),t.getRaio(),t.aceitaMed(),t.getRegistos(),t.getEmail(),t.getPass(),t.getClassificacao(),t.getQuantServicos(),t.getKmTotais());
        setNif(t.getNif());
        setTaxa(t.getTaxa());
        setCapacidade(t.getCapacidade());
        setRegistos(t.getRegistos());
        setEmail(t.getEmail());
        setPass(t.getPass());;
        setEncomendasAfazer(t.getEncomendasAfazer());
    }

    /**
     * Getter do numero de identificação fiscal
      * @return numero de identificação fiscal
     */
    public String getNif(){return this.nif;}

    /**
     * Getter do preço por kilometro
     * @return preço por kilometro
     */
    public double getTaxa(){ return this.taxa;}

    /**
     * Getter da capacidade
     * @return capacidade
     */
    public int getCapacidade(){return this.capacidade;}

    /**
     * Getter do conjunto de encomendas a fazer
     * @return conjunto de encomendas a fazer
     */
    public Map<String,Encomenda> getEncomendasAfazer(){
        Map<String,Encomenda> aux = new TreeMap<>();
        for(Map.Entry<String,Encomenda> e : this.encomendasAfazer.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    /**
     * Setter do numero de identificação fiscal
      * @param novo novo numero de identificação fiscal
     */
    public void setNif(String novo){this.nif = novo;}

    /**
     * Setter do preço por kilometro
     * @param novo novo preço por kilometro
     */
    public void setTaxa(double novo){this.taxa = novo;}

    /**
     * Setter da capacidade
     * @param novo nova capacidade
     */
    public void setCapacidade(int novo){this.capacidade = novo;}

    /**
     * Setter das encomendas a fazer
     * @param e encomenda
     */
    public void setEncomendasAfazer(Map<String,Encomenda> e){
        this.encomendasAfazer = new TreeMap<>();
        e.entrySet().forEach(l->this.encomendasAfazer.put(l.getKey(),l.getValue().clone()));
    }

    /**
     * Metodo clone da transportadora
      * @return transportadora clonada
     */
    public Transportadora clone(){return new Transportadora(this);}


    /**
     * Metodo equals da transportadora
     * @param o Objeto
     * @return booleano
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass()!=this.getClass()) return false;
        Transportadora t = (Transportadora) o;
        return super.getCodigo().equals(t.getCodigo()) &&
                super.getNome().equals(t.getNome()) &&
                super.getRegistos().equals(t.getRegistos()) &&
                super.getXGPS() == t.getXGPS() &&
                super.getYGPS() == t.getYGPS() &&
                super.getEstado() == t.getEstado() &&
                super.getRaio() == t.getRaio() &&
                super.aceitaMed() == t.aceitaMed() &&
                super.getQuantServicos() == t.getQuantServicos() &&
                super.getClassificacao() == t.getClassificacao() &&
                super.getKmTotais() == t.getKmTotais() &&
                this.encomendasAfazer.equals(t.getEncomendasAfazer()) &&
                this.nif.equals(t.getNif()) &&
                this.taxa == t.getTaxa() &&
                this.capacidade == t.getCapacidade();
    }

    /**
     * Metodo toString
      * @return String
     */
    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append("\nTransportadora\n").append(super.toString()).append(" | Nif da empresa: ")
                .append(this.nif).append(" | Capacidade: ").append(this.capacidade)
                .append(" | Preço por km: ").append(this.taxa).append(" | Encomendas para entregar : ")
                .append(this.encomendasAfazer).append("\n");
        return s.toString();
    }



    /**
     * Metodo que determina o preço de transporte de uma encomenda.
     * Recebe como argumentos: a encomenda que esta a entregar e o (x,y) quer da loja quer da casa do utilizador.
     * Caso o peso da encomenda seja menor ou igual  a 1, nao tem influencia no valor.
     * @param e encomenda a transportar
     * @param xU coordenada x do utilizador
     * @param yU coordenada y do utilizador
     * @param xL coordenada x da loja
     * @param yL coordenada y da loja
     * @return preço total
     */
    public double calculaEncTrans(Encomenda e,double xU,double yU,double xL,double yL){
        if(e.getPesoEnc()<=1) return e.calculaValorEnc() + super.calculaDistancia(xU,yU,xL,yL) * this.taxa;
        return e.calculaValorEnc() + super.calculaDistancia(xU,yU,xL,yL) * this.taxa * e.getPesoEnc();
    }

    /**
     * Metodo que devolve uma String com o nome, codigo, classificação geral, custo total do transporte e tempo estimado da entrega
     * @param e encomenda a transportar
     * @param xU coordenada x do utilizador
     * @param yU coordenada y do utilizador
     * @param xL coordenada x da loja
     * @param yL coordenada y da loja
     * @return String
     */
    public String imprime(Encomenda e,double xU,double yU,double xL,double yL){
        StringBuilder s = new StringBuilder();
        s.append("Transportadora\n").append("Nome: ").append(super.getNome()).append(" , Codigo: ").append(super.getCodigo())
                .append(" , Classificaçao geral: ").append(super.getClassificacao()).append(" , Custo total do transporte: ").append(calculaEncTrans(e,xU,yU,xL,yL))
                .append(" , Tempo estimado da sua entrega : ").append(super.calculaTempoEstimado(xU,yU,xL,yL)).append(" minutos");
        return s.toString();
    }


    /**
     * Metodo que verifica se a empresa transportadora ainda pode carregar mais encomendas
     * @return booleano
     */
    public boolean temEspaco(){return this.encomendasAfazer.size() < this.capacidade;}


    /**
     * Metodo que insere uma encomenda na lista de encomendas a fazer
     * @param e encomenda a inserir
     */
    public void insereEncAChegar(Encomenda e){this.encomendasAfazer.put(e.getCodEncomenda(),e.clone());}


    /**
     * Metodo que remove uma encomenda da lista de encomendas a fazer por uma transportadora
     * @param codEnc codigo da encomenda a remover
     */
    public void removeEnc(String codEnc){this.encomendasAfazer.remove(codEnc);}


    /**
     * Metodo que devolve um map das encomendas ordenadas por distancia(da que está mais perto da transportadora até á que está mais longe)
     * @return conjunto de encomendas ordenadas por distancia
     */
    public TreeSet<Encomenda> ordenaPorDistancia(){
        TreeSet<Encomenda> ts = new TreeSet<>(new ComparatorDistancia());
        for(Encomenda e : this.encomendasAfazer.values()) ts.add(e.clone());
        return ts;
    }


    /**
     * Metodo que devolve a encomenda que a transportadora tem que fazer em primeiro lugar(a que está mais perto de ser entregue pela transportadora)
     * @param ts conjunto ordenado por distancia
     * @return encomenda a fazer(segundo a rota)
     * @throws ConjuntoVazioException
     */
    public Encomenda encAfazer(TreeSet<Encomenda> ts) throws ConjuntoVazioException {
        if(ts.size()<=0) throw new ConjuntoVazioException();
        else
            return ts.first().clone();
    }


    /**
     * Metodo que devolve o codigo de encomenda que a transportadora tem que fazer em primeiro lugar e remove-a da lista de encomendas a fazer
     * @param ts conjunto ordenado por distancia
     * @return  encomenda a fazer(segundo a rota)
     * @throws ConjuntoVazioException
     */
    public String codEncAfazer(TreeSet<Encomenda> ts) throws ConjuntoVazioException {
        if(ts.size()<=0) throw new ConjuntoVazioException();
        else
            return ts.first().getCodEncomenda();
    }


    /**
     * Metodo que remove a encomenda que a transportadora tem que fazer em primeiro lugar da lista de encomendas a fazer
     * @param ts conjunto ordenado por distancia
     * @throws ConjuntoVazioException
     */
    public void remEncAfazer(TreeSet<Encomenda> ts) throws ConjuntoVazioException {
        this.encomendasAfazer.remove(codEncAfazer(ts));
    }


    /**
     * Metodo que devolve o numero de encomendas a fazer nume entrega
     * @return numero de encomendas a fazer nume entrega
     */
    public int nmrEnc(){return this.encomendasAfazer.values().size();}



    /**
     * Metodo que faz as alterações necessarias quando uma transportadora inicia uma entrega
     * @param e encomenda a entregar
     */
    public void aceitaEncT(Encomenda e){
        e.setInicio(LocalDateTime.now());
        e.setFim(LocalDateTime.of(2000,1,1,0,0));
        e.setTempoEntrega("");
    }

    /**
     * Metodo que faz as alterações necessarias quando um voluntario acaba de transportar uma encomenda
     * @param e encomenda a entregar
     */
    public void acabaEncT(Encomenda e){
        e.setFim(LocalDateTime.now());
        if(e.getFim().getHour() - e.getInicio().getHour()==0) e.setTempoEntrega(String.valueOf(e.getFim().getMinute() - e.getInicio().getMinute()) + " minutos");
        else
            e.setTempoEntrega(String.valueOf(e.getFim().getHour() - e.getInicio().getHour()) + " horas e " + String.valueOf(e.getFim().getMinute() - e.getInicio().getMinute()) + " minutos");
    }


    /**
     * Metodo que devolve uma String com o codigo, nome, coordenadas, raio de circulação, nif e preço por kilometro de uma transportadora
     * @return
     */
    public String toStringCSV(){
        StringBuilder s = new StringBuilder();
        s.append("Transportadora:").append(super.getCodigo()).append(",").append(super.getNome()).append(",").append(super.getXGPS()).append(",").append(super.getYGPS())
                .append(",").append(this.nif).append(",").append(super.getRaio()).append(",").append(this.taxa).append("\n");
        return s.toString();
    }

}
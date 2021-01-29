


import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

public class Voluntario extends EntidadeEntrega implements Serializable
{

    /** verifica se o voluntario está com disposição para fazer entregas */
    private boolean disposicao;

    /** codigo de encomenda que está a ser transportada */
    private String codEncATransportar;

    /** encomenda que está a ser transportada */
    private Encomenda encomendaAtransportar;

    /** data e hora do inicio da encomenda */
    private LocalDateTime inicio;

    /** data e hora do fim da encomenda */
    private LocalDateTime fim;

    /** tempo de entrega */
    private String tempoEntrega;


    /**
     * Construtor por omissão de Voluntario
     */
    public Voluntario(){
        super();
        this.disposicao = false;
        this.codEncATransportar = new String();
        this.encomendaAtransportar = new Encomenda();
        this.tempoEntrega = new String();
        this.inicio = LocalDateTime.of(2000,1,1,0,0);
        this.fim = LocalDateTime.of(2000,1,1,0,0);
    }

    /**
     * Construtor parametrizado de Voluntario
     * @param nome nome da entidade
     * @param codVoluntario identificador da entidade
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param raio raio de circulação da entidade
     * @param aceitaMed verificação se aceita o transporte de encomendas médicas
     * @param registos conjunto das encomendas feitas por uma entidade
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     * @param cla classificação geral da entidade de entrega
     * @param ser quantidade de serviços(entrega) feitos até ao momento
     * @param kms kilometros totais efetuados pela entidade de entrega
     */
    public Voluntario(String nome,String codVoluntario,double xGPS,double yGPS,double raio,boolean aceitaMed,Map<String,Encomenda> registos, String email, String pass,double cla,int ser,double kms,boolean d,String c,Encomenda e,String t,LocalDateTime in, LocalDateTime fi){
        super(nome,codVoluntario,xGPS,yGPS,raio,aceitaMed,registos,email,pass,cla,ser,kms);
        this.disposicao = d;
        this.codEncATransportar = c;
        this.encomendaAtransportar = e;
        this.tempoEntrega = t;
        this.inicio = in; //LocalDateTime.of(2000,1,1,0,0);
        this.fim = fi; //LocalDateTime.of(2000,1,1,0,0);

    }

    /**
     * Construtor por cópia de Voluntario
     * @param v voluntario
     */
    public Voluntario(Voluntario v){
        super(v.getNome(),v.getCodigo(),v.getXGPS(),v.getYGPS(),v.getRaio(),v.aceitaMed(),v.getRegistos(),v.getEmail(),v.getPass(),v.getClassificacao(),v.getQuantServicos(),v.getKmTotais());
        this.codEncATransportar = v.getCodEncATransportar();
        this.encomendaAtransportar = v.getEncomendaAtransportar();
        this.tempoEntrega = v.getTempoEntrega();
        this.disposicao = v.getDisposicao();
        this.tempoEntrega = v.getTempoEntrega();
        this.inicio = v.getInicio();
        this.fim = v.getFim();
    }


    /**
     * Getter da disposição
      * @return disposição
     */
    public boolean getDisposicao(){return this.disposicao;}

    /**
     * Getter do codigo da encomenda a transportar
     * @return codigo da encomenda a transportar
     */
    public String getCodEncATransportar(){return this.codEncATransportar;}

    /**
     * Getter da encomenda a transportar
     * @return encomenda a transportar
     */
    public Encomenda getEncomendaAtransportar(){return this.encomendaAtransportar;}

    /**
     * Getter do inicio da entrega
     * @return inicio da entrega
     */
    public LocalDateTime getInicio(){ return this.inicio;}

    /**
     * Getter do fim da entrega
     * @return fim da entrega
     */
    public LocalDateTime getFim(){ return this.fim;}

    /**
     * Getter do tempo de entrega
     * @return tempo de entrega
     */
    public String getTempoEntrega(){return this.tempoEntrega;}

    /**
     * Setter da disposição
      * @param novo nova disposição
     */
    public void setDisposicao(boolean novo){this.disposicao = novo;}

    /**
     * Setter do codigo de encomenda a ser transportada
     * @param novo novo codigo de encomenda a ser transportada
     */
    public void setCodEncATransportar(String novo){this.codEncATransportar = novo;}

    /**
     * Setter da encomedda a transportar
     * @param e nova encomedda a transportar
     */
    public void setEncomendaAtransportar(Encomenda e){this.encomendaAtransportar = e;}

    /**
     * Setter do inicio da entrega
     * @param novo novo inicio de entrega
     */
    public void setInicio(LocalDateTime novo){this.inicio = novo; }

    /**
     * Setter do fim da entrega
     * @param novo novo fim de entrega
     */
    public void setFim(LocalDateTime novo){ this.fim = novo;}

    /**
     * Setter do tempo de entrega
     * @param novo novo tempo de entrega
     */
    public void setTempoEntrega(String novo){ this.tempoEntrega = novo;}



    /**
     * Metodo clone de um voluntario
     * @return voluntario clonado
     */
    public Voluntario clone(){return new Voluntario(this);}

    /**
     * Metodo que faz as alterações necessarias quando um voluntario aceita transportar uma encomenda
     */
    public void aceitaEncV() {
        setEstado(false);
        setInicio(LocalDateTime.now());
        setFim(LocalDateTime.of(2000,1,1,0,0));
        setTempoEntrega("");
    }

    /**
     * Metodo que faz as alterações necessarias quando um voluntario acaba de transportar uma encomenda
     */
    public void acabaEncV(){
        setFim(LocalDateTime.now());
        if(this.fim.getHour() - this.inicio.getHour()==0) setTempoEntrega(String.valueOf(this.fim.getMinute() - this.inicio.getMinute()) + " minutos");
        else
            setTempoEntrega(String.valueOf(this.fim.getHour() - this.inicio.getHour()) + " horas e " + String.valueOf(this.fim.getMinute() - this.inicio.getMinute()) + " minutos");
    }


    /**
     * Metodo equals de voluntario
      * @param o Objeto
     * @return booleano
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass()!=this.getClass()) return false;
        Voluntario v = (Voluntario) o;
        return super.getCodigo().equals(v.getCodigo()) &&
                super.getNome().equals(v.getNome()) &&
                super.getEstado() == v.getEstado() &&
                super.getRegistos().equals(v.getRegistos()) &&
                super.getXGPS() == v.getXGPS() &&
                super.getYGPS() == v.getYGPS() &&
                super.getRaio() == v.getRaio() &&
                super.aceitaMed() == v.aceitaMed() &&
                this.inicio.equals(v.getInicio()) &&
                this.fim.equals(v.getFim()) &&
                this.tempoEntrega.equals(v.getTempoEntrega()) &&
                super.getClassificacao() == v.getClassificacao() &&
                super.getKmTotais() == v.getKmTotais() &&
                super.getQuantServicos() == v.getQuantServicos() &&
                this.codEncATransportar.equals(v.getCodEncATransportar()) &&
                this.disposicao == v.getDisposicao() &&
                this.encomendaAtransportar.equals(v.getEncomendaAtransportar());

    }


    /**
     * Metodo toString de volunatario
     * @return String
     */
    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append("\nVoluntario:\n").append(super.toString()).append(" | Disposicao do voluntario: ");
        if(this.getDisposicao()) s.append("Disposto ");
        else s.append("Nao disposto ");
        s.append(" | Encomenda a entregar de momento: ");
        return s.toString();
    }


    /**
     * Metodo que devolde uma String com o nome, codigo, classificação geral e tempo estimado da entrega
     * @param e encomenda a transportar
     * @param xU coordenada x do utilizador
     * @param yU coordenada y do utilizador
     * @param xL coordenada x do loja
     * @param yL coordenada y do loja
     * @return String
     */
    public String imprime(Encomenda e,double xU,double yU,double xL,double yL){
        StringBuilder s = new StringBuilder();
        s.append("Voluntario\n").append("Nome: ").append(super.getNome()).append(" , Codigo: ").append(super.getCodigo())
                .append(" , Classificacao geral: ").append(super.getClassificacao()) .append(" , Tempo estimado da sua entrega : ")
                .append(super.calculaTempoEstimado(xU,yU,xL,yL)).append(" minutos");

        return s.toString();
    }


    /**
     * Metodo que altera o inicio e fim para uma data referencia
     */
    public void resetTempo(){
        setInicio(LocalDateTime.of(2000,1,1,0,0));
        setFim(LocalDateTime.of(2000,1,1,0,0));
        setTempoEntrega("");
    }


    /**
     * Metodo que devolve uma String com o codigo, nome, coordenadas e raio de circulação de um voluntario
     * @return String
     */
    public String toStringCSV(){
        StringBuilder s = new StringBuilder();
        s.append("Voluntario:").append(super.getCodigo()).append(",").append(super.getNome()).append(",").append(super.getXGPS()).append(",").append(super.getYGPS())
                .append(",").append(super.getRaio()).append("\n");
        return s.toString();
    }

}
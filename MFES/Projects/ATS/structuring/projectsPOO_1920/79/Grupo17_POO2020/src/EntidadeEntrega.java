
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Map;


public abstract class EntidadeEntrega extends Entidade implements Serializable
{

    /** raio de circulação da entidade de entrega */
    private double raio;

    /** verifica se a entidade de entrega está disponivel
     *  para fazer a entregas(true-não ocupado,false-ocupado) */
    private boolean estado;

    /** verifica se a entidade de entrega aceita o transporte
     * de produtos médicos(true-aceita,false-não aceita) */
    private boolean aceitaMed;

    /** classificação geral da entidade de entrega */
    private double classificacao;

    /** quantidade de serviços(entrega) feitos até ao momento */
    private int quantServicos;

    /** kilometros totais efetuados pela entidade de entrega */
    private double kmTotais;

    /** tempo estimado de transporte */
    private LocalDateTime tempoEstimadoDeTransporte;


    /**
     * Construtor por omissão de EntidadeEntrega
     */
    EntidadeEntrega(){
        super();
        this.estado = true;
        this.raio = 0;
        this.aceitaMed = false;
        this.classificacao = 0;
        this.quantServicos = 0;
        this.kmTotais = 0;
        this.tempoEstimadoDeTransporte = LocalDateTime.of(2000,1,1,0,0);
    }


    /**
     * Construtor parametrizado de EntidadeEntrega
     * @param nome nome da entidade
     * @param codigo identificador da entidade
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param raio raio de circulação da entidade de entrega
     * @param aceitaMed booleano que verifica se é produto médico
     * @param registos conjunto das encomendas feitas por uma entidade
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     * @param cla classificação geral da entidade de entrega
     * @param ser quantidade de serviços(entrega) feitos até ao momento
     * @param kms kilometros totais efetuados pela entidade de entrega
     */
    EntidadeEntrega(String nome, String codigo, double xGPS, double yGPS, double raio, boolean aceitaMed,Map<String,Encomenda> registos, String email, String pass,double cla,int ser, double kms){
        super(nome,codigo,xGPS,yGPS,registos,email,pass);
        this.estado = true;
        this.raio = raio;
        this.aceitaMed = aceitaMed;
        this.classificacao = cla;
        this.quantServicos = ser;
        this.kmTotais = kms;
        this.tempoEstimadoDeTransporte = LocalDateTime.of(2000,1,1,0,0);
    }


    /**
     * Getter do estado da entidade
     * @return estado da entidade
     */
    public boolean getEstado(){return this.estado;}

    /**
     * Getter do raio de circulação da entidade de entrega
     * @return raio de circulação da entidade de entrega
     */
    public double getRaio(){return this.raio;}

    /**
     * Getter da verificação se transporta encomendas médicas
     * @return verificação se transporta encomendas médicas
     */
    public boolean aceitaMed(){return this.aceitaMed;}

    /**
     * Getter da classificação geral
     * @return classificação geral
     */
    public double getClassificacao(){return this.classificacao;}

    /**
     * Getter da quantidade de serviços
     * @return quantidade de serviços
     */
    public int getQuantServicos(){return this.quantServicos;}

    /**
     * Getter do numero de kilometros totais realizados
     * @return kilometros totais realizados
     */
    public double getKmTotais(){return this.kmTotais;}

    /**
     * Getter do tempo estimado de transporte
     * @return tempo estimado de transporte
     */
    public LocalDateTime getTempoDeTransporte(){ return this.tempoEstimadoDeTransporte;}


    /**
     * Setter do estado da entidade
     * @param novo novo estado
     */
    public void setEstado(boolean novo){this.estado = novo;}

    /**
     * Setter do raio de circulação
     * @param novo novo raio de circulação
     */
    public void setRaio(double novo){this.raio = novo;}

    /**
     * Setter da verificação se transporta produtos médicos
     * @param state nova verificação se transporta produtos médicos
     */
    public void aceitaMedicamentos(boolean state){this.aceitaMed = state;}

    /**
     * Setter da classificação geral
     * @param novo nova classificação geral
     */
    public void setClassificacao(double novo){this.classificacao = novo;}

    /**
     * Setter da quantidade de serviços realizados
     * @param novo nova quantidade de serviços realizados
     */
    public void setQuantServicos(int novo){this.quantServicos = novo;}

    /**
     * Setter dos kilometros realizados
     * @param novo novos kilometros realizados
     */
    public void setKmTotais(double novo){this.kmTotais = novo;}

    /**
     * Setter do tempo estimado de transporte
     * @param novo novo tempo estimado de transporte
     */
    public void setTempoDeTransporte(LocalDateTime novo){ this.tempoEstimadoDeTransporte = novo;}


    /**
     * Metodo toString
     * @return String
     */
    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append(super.toString()).append(" | Raio de circulacao: ").append(this.raio).append(" | Ocupado: ");
        if(this.estado) s.append("Nao ");
        else s.append("Sim ");
        s.append(" | Aceita encomendas medicas: ");
        if(this.aceitaMed) s.append("Sim ");
        else s.append("Nao ");
        s.append(" | Classificacao geral: ").append(this.classificacao)
                .append(" | Numero de servicos feitos ate ao momento: ").append(this.quantServicos)
                .append(" | Kilometros totais efetuados: ").append(this.kmTotais);
        return s.toString();
    }


    /**
     * Metodo abstrato equals
     * @param o Objeto
     * @return booleano
     */
    public abstract boolean equals (Object o);


    /**
     * Metodo abstrato clone
     * @return clone da entidade
     */
    public abstract Entidade clone();



    /**
     * Metodo que verifica se uma entidade de entrega está dentro do alcance tanto de um destinatario como de um vendedor
     * @param xU coordenada x do utilizador
     * @param yU coordenada y do utilizador
     * @param xL coordenada x da loja
     * @param yL coordenada y da loja
     * @return booleano
     */
    public boolean dentroAlcance(double xU, double yU,double xL, double yL){
        return (Math.sqrt(Math.pow(super.getXGPS()-xU,2) + Math.pow(super.getYGPS()-yU,2))<this.raio && Math.sqrt(Math.pow(super.getXGPS()-xL,2) + Math.pow(super.getYGPS()-yL,2))<this.raio);
    }


    /**
     * Metodo que adiciona uma nova classificacao a entidade de entrega
     * @param n nova classificação
     * @throws NaoHaServicosException
     * @throws ValoresNaoValidosException
     */
    public void adicionaClassificacao(double n) throws NaoHaServicosException,ValoresNaoValidosException{
        if(n<0) throw new ValoresNaoValidosException();
        else if(this.quantServicos<0){
            throw new NaoHaServicosException();
        } else
            setClassificacao((((this.quantServicos-1) * this.classificacao) + n) / (this.quantServicos));
    }


    /**
     * Metodo que faz as alterações necessárias quando um transportador termina de entregar uma encomenda de um utilizador.
     * @param e entidade a fazer a entrega
     * @param enc encomenda a entregar
     * @param xU coordenada x do utilizador
     * @param yU coordenada y do utilizador
     * @param xL coordenada x da loja
     * @param yL coordenada y da loja
     */
    public void acabaEnc(Entidade e,Encomenda enc,double xU,double yU,double xL,double yL){
        if(e instanceof Voluntario) {
            Voluntario v  = (Voluntario) e;
            v.acabaEncV();
            setEstado(true);
        } else {
            Transportadora t = (Transportadora) e;
            t.acabaEncT(enc);
            if(t.nmrEnc()==1) setEstado(true);
        }
        super.insereEnc(enc);
        setQuantServicos(getQuantServicos()+1);
        setKmTotais(this.kmTotais+calculaDistancia(xU,yU,xL,yL));
    }


    /**
     * Metodo que devolve uma String com a classificação geral, kilometros totais realizados e numero de serviços de uma entidade de entrega
     * @return String
     */
    public String imprimeClassKmServicos(){
        StringBuilder s = new StringBuilder();
        s.append("Classificaçao geral: ").append(this.classificacao).append(" , ")
                .append("Km totais realizados: ").append(this.kmTotais).append(" , ")
                .append("Numero de serviços realizados: ").append(this.quantServicos).append("\n");
        return s.toString();
    }


    /**
     * Metodo que determina a distancia total percorrida da localização do transportador á loja e desta a casa do utilizador
     * @param xU coordenada x do utilizador
     * @param yU coordenada y do utilizador
     * @param xL coordenada x da loja
     * @param yL coordenada y da loja
     * @return distancia total do transporte
     */
    public double calculaDistancia(double xU,double yU,double xL,double yL){
        return Math.sqrt(Math.pow(super.getXGPS()-xL,2) + Math.pow(super.getYGPS()-yL,2)) + Math.sqrt(Math.pow(xL-xU,2) + Math.pow(yL-yU,2));
    }


    /**
     * Metodo abstrato imprime
     * @param en encomenda a realizar
     * @param xu coordenada x do utilizador
     * @param yu coordenada y do utilizador
     * @param xl coordenada x da loja
     * @param yl coordenada y da loja
     * @return String
     */
    public abstract String imprime(Encomenda en, double xu, double yu, double xl, double yl);

    /**
     * Metodo que calcula o tempo estimado de transporte considerando que as entidades de entrega andam a uma velocidade media de 60km/hora
     * @param xU coordenada x do utilizador
     * @param yU coordenada y do utilizador
     * @param xL coordenada x da loja
     * @param yL coordenada y da loja
     * @return tenpo estimado
     */
    public double calculaTempoEstimado(double xU,double yU,double xL,double yL){
        int velMedia = 60;
        return (calculaDistancia(xU,yU,xL,yL)*60)/velMedia;
    }
}
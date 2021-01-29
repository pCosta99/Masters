package Modelo;


import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Classe abstrata que contém a implementação da estrutura da Transportadora que aceita mais do que uma encomenda
 */
public class TransportadoraMulti extends Transportadora{

    private int numero;
    private List<Encomenda> pendentes;

    /**
     * Construtor por omissão
     */
    public TransportadoraMulti(){
        super();
        this.numero = 0;
        this.pendentes = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param codEmpr               Codigo
     * @param nomeEmpr              Nome
     * @param nif                   Nif
     * @param multi                 Multi ou não
     * @param meds                  Transporta ou não medicamentos
     * @param gps                   Coordenadas
     * @param raio                  Raio
     * @param preco_km              Preço por km
     * @param preco_peso            Preço por peso
     * @param velocidade            Velocidade
     * @param nkms                  Nkms
     * @param clas                  Classificação
     * @param registoT              Registos
     * @param numero                Numero de encomendas que  entrega
     * @param pendentes             Encomendas por entregar
     */
    public TransportadoraMulti(String codEmpr, String nomeEmpr, int nif, boolean multi, boolean meds,
                               Coordenadas gps, double raio, double preco_km, double preco_peso,
                               double velocidade,double nkms,List<Integer> clas,Map<Encomenda, LocalDateTime> registoT, int numero,List<Encomenda> pendentes) {
        super(codEmpr, nomeEmpr, nif,multi,meds, gps, raio, preco_km, preco_peso, velocidade,nkms,clas, registoT);
        this.numero = numero;
        setPendentes(pendentes);
    }

    /**
     * Construtor por cópia
     * @param t         TransportadoraMulti
     */
    public TransportadoraMulti(TransportadoraMulti t){
        super(t);
        this.numero = t.getNumero();
        setPendentes(t.getPendentes());
    }




    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve o número máximo de encomendas que uma transportadora aceita
     * @return int
     */
    public int getNumero() {
        return numero;
    }

    /**
     * Define o número máximo de encomendas que uma transportadora aceita
     * @param numero                Numero a definir
     */
    public void setNumero(int numero) {
        this.numero = numero;
    }

    /**
     * Devolve a lista de encomendas pendentes.
     * @return List<Encomenda>
     */
    public List<Encomenda> getPendentes() {
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda e: this.pendentes)
            aux.add(e);
        return aux;
    }

    /**
     * Define a lista de encomendas pendentes
     * @param pendentes             Encomendas pendentes
     */
    public void setPendentes(List<Encomenda> pendentes) {
        this.pendentes = new ArrayList<>();
        for(Encomenda e: pendentes)
            this.pendentes.add(e);
    }


    /**
     * Devolve uma cópia da instância
     * @return TransportadoraMulti          this
     */
    public TransportadoraMulti clone(){
        return new TransportadoraMulti(this);
    }


    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("TransportadoraMulti{");
        sb.append("numero=").append(numero);
        sb.append(", pendentes=").append(pendentes);
        sb.append('}');
        return sb.toString();
    }


    /**
     * Função que informa a transportadora se o utilizador está dentro do raio ou não
     * @param utilizador                Coordenadas do utilizador
     * @return  boolean                 Dentro ou não do raio
     */
    public boolean dentroRaio(Coordenadas utilizador){
        boolean flag;
        flag = utilizador.distance(this.getGps()) <= getRaio();
        return flag;
    }


    /**
     * Função que nos dá o custo de cada encomenda, dada uma encomenda, um utilizador e as coordenadas de uma loja
     * @param e                      Encomenda
     * @param u                      Utilizador
     * @param loja                   Loja
     * @return double                Custo da encomenda
     */
    public double custoEncomenda(Encomenda e, Utilizador u, Coordenadas loja){
        double peso = e.getPeso();
        Coordenadas gpsU = u.getGps();
        double distTranLoja = getGps().distance(loja);
        double distTranUtil = getGps().distance(gpsU);
        return ((distTranLoja + distTranUtil) * getPreco_km()) + (peso * getPreco_peso());
    }


    /**
     * Função que nos dá o tempo de cada encomenda, dada uma encomenda, um utilizador e uma loja
     * @param e                      Encomenda
     * @param u                      Utilizador
     * @param l                      Loja
     * @return double                Tempo de encomenda
     */
    public double tempodeEncomenda(Encomenda e, Utilizador u, Loja l){
        double distancia,tempo;
        if(l.getTempoAtend() >= 0){
            distancia = (getGps().distance(l.getGps()) + l.getGps().distance(u.getGps())) /1000; //km
            tempo = (distancia/getVelocidade())*60 + l.getTempoFila() + l.getTempoAtend(); //minutos
        }
        else {
            distancia = (getGps().distance(l.getGps()) + l.getGps().distance(u.getGps())) /1000; //km
            tempo = (distancia/getVelocidade())*60 + l.getTempoAtend(); //minutos
        }
        return tempo;
    }


    /**
     * Função que nos diz se a transportandora atingiu o seu numero maximo de encomendas possivel
     * @return booleano que dá true se, de facto, estiver cheia e false caso contrario
     */
    public boolean isCheio(){
        return this.pendentes.size() == numero;
    }


    /**
     * Função que enche a Lista de pendentes
     * @param p                 TrazAquiModel
     */
    public void cheia(TrazAquiModel p){
        for(Encomenda e: this.pendentes){
            Utilizador u = p.getUtilizador().get(e.getCodUtilizador());
            u.adicionaEncomendas(e.clone());
        }
    }


    /**
     *Função que "limpa" a lista de pendentes
     */
    public void remove(){
        setPendentes(new ArrayList<>());
    }


    /**
     * Função que adiciona uma encomenda à lista de encomendas pendentes
     * @param e                          Adiciona uma pendente
     */
    public void adicionaPendente(Encomenda e){
        this.pendentes.add(e);
    }


}

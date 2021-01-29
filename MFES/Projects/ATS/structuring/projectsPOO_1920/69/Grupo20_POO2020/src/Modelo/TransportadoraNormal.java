package Modelo;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * Classe abstrata que contém a implementação da estrutura da Transportadora Normal
 */
public class TransportadoraNormal extends Transportadora{

    /**
     * Construtor por omissão
     */
    public TransportadoraNormal(){
        super();
    }

    /**
     * Construtor parametrizado
     * @param codEmpr           Codigo da Empresa
     * @param nomeEmpr          Nome da Empresa
     * @param nif               Nif
     * @param multi             Se multi ou não
     * @param meds              Se transporta ou não medicamentos
     * @param gps               Coordenadas
     * @param raio              Raio
     * @param preco_km          Preço por km
     * @param preco_peso        Preço por peso
     * @param velocidade        Velocidade media
     * @param nKms              Nkms feitos
     * @param clas              Classificacao
     * @param registoT          Registos
     */
    public TransportadoraNormal(String codEmpr, String nomeEmpr, int nif,boolean multi, boolean meds,
                                Coordenadas gps, double raio, double preco_km, double preco_peso,
                                double velocidade,double nKms,List<Integer> clas, Map<Encomenda, LocalDateTime> registoT) {
        super(codEmpr, nomeEmpr, nif,multi,meds, gps, raio, preco_km, preco_peso, velocidade,nKms,clas, registoT);
    }


    /**
     * Construtor por cópia.
     * @param t          TransportadoraNormal
     */
    public TransportadoraNormal(TransportadoraNormal t) {
        super(t);
    }


    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(super.toString());
        s.append("\n");
        return s.toString();
    }

    /**
     * Devolve uma cópia da instância
     * @return TransportadoraNormal
     */
    public TransportadoraNormal clone(){
        return new TransportadoraNormal(this);
    }

    /**
     * Função que informa se a transportadora está dentro do raio ou não
     * @param gps                Coordenadas
     * @return boolean
     */
    public boolean dentroRaio(Coordenadas gps){
        boolean flag;
        flag = gps.distance(getGps()) <= getRaio();
        return flag;
    }


    /**
     * Função que nos dá o custo de cada encomenda, dada uma encomenda, um utilizador e as coordenadas de uma loja
     * @param e                      Encomenda
     * @param u                      Utilizador
     * @param loja                   Loja
     * @return                       Custo da  encomenda
     */
    public double custoEncomenda(Encomenda e, Utilizador u, Coordenadas loja){
        double peso = e.getPeso();
        Coordenadas gpsU = u.getGps();
        double distTranLoja = getGps().distance(loja);
        double distLojaUtil = loja.distance(gpsU);
        return ((distTranLoja + distLojaUtil) * getPreco_km()) + (peso * getPreco_peso());
    }


    /**
     * Função que nos dá o tempo de cada encomenda, dada uma encomenda, um utilizador e uma loja
     * @param e                     Encomenda
     * @param u                     Utilizador
     * @param l                     Loja
     * @return double               Tempo de encomenda
     */
    public double tempodeEncomenda(Encomenda e, Utilizador u, Loja l){
        double distancia,tempo;
        if(l.getTempoFila() >= 0){
            distancia = (getGps().distance(l.getGps()) + l.getGps().distance(u.getGps())); //km
            tempo = (distancia/getVelocidade())*60 +  l.getTempoFila() + l.getTempoAtend(); //minutos
        }
        else {
            distancia = (getGps().distance(l.getGps()) + l.getGps().distance(u.getGps())); //km
            tempo = (distancia/getVelocidade())*60; //minutos
        }
        return tempo;
    }


}

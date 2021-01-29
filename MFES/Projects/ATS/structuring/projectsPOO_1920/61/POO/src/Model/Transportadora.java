package Model;

import Exceptions.EncomendaInexistenteException;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

public class Transportadora extends Transporte implements Serializable {

    private String nif;
    private Double prec_km;
    private Double total_km;
    private List<Encomenda> paraEntregar;
    //Lista de encomendas que ficam no caminho
    private List<Encomenda> noCaminho;

    /**
     * Construtor da classe
     */
    public Transportadora(){
        super();
        this.total_km = 0.0;
        nif = null;
        prec_km = 0.0;
        paraEntregar = new ArrayList<>();
        noCaminho = new ArrayList<>();
    }

    /**
     * Construtor da classe
     * @param code O código da transportadora
     * @param nome O nome da transportadora
     * @param encEsp A indicação do transporte ou não de encomendas médicas
     * @param gps As coordenadas GPS da transportadora
     * @param raio O raio de ação da transportadora
     * @param disp A disponibilidade da transportadora
     * @param encEntregues As encomendas já entregues pela transportadora
     * @param futurasEnc As encomendas que a transportadora tem para ir levantar às lojas
     * @param classificacoes As classificações atribuídas à transportadora
     * @param nif O NIF da transportadora
     * @param prec_km O preço por quilometro
     * @param total_km O total de kilometros percorridos pela transportadora
     * @param paraEntregar As encomendas que a transportadora tem na sua posse para entregar
     * @param noCaminho As encomendas que se encontram no percurso que a transportadora está a percorrer
     */
    public Transportadora(String code, String nome,boolean encEsp, GPS gps, Double raio, boolean disp, Map<Utilizador, Set<Encomenda>> encEntregues, Map<String, Encomenda> futurasEnc, List<Double> classificacoes, String nif, Double prec_km, Double total_km, List<Encomenda> paraEntregar, List<Encomenda> noCaminho) {
        super(code, nome, gps,raio, disp, encEntregues, futurasEnc, classificacoes,encEsp);
        this.total_km = total_km;
        setNif(nif);
        setPrec_km(prec_km);
        setParaEntregar(paraEntregar);
        setNoCaminho(noCaminho);
    }

    /**
     * Construtor da classe
     * @param t A transportadora da qual se pretende obter as informações
     */
    public Transportadora(Transportadora t) {
        super(t);
        this.total_km = t.getTotal_km();
        setNif(t.nif);
        setPrec_km(t.prec_km);
        setParaEntregar(t.paraEntregar);
        setNoCaminho(t.noCaminho);
    }

    /**
     * Indica as encomendas que uma transportadora tem na sua posse para entregar
     * @return A lista das encomendas para entregar
     */
    public List<Encomenda> getParaEntregar() {
        List<Encomenda> res = new ArrayList<>();
        for(Encomenda e : this.paraEntregar){
            res.add(e.clone());
        }
        return res;
    }

    /**
     * Define as encomendas que uma transportadora tem na sua posse para entregar
     * @param encs A lista das encomendas para entregar a definir
     */
    public void setParaEntregar(List<Encomenda> encs){
        List<Encomenda> res = new ArrayList<>();
        for(Encomenda e : encs){
            res.add(e.clone());
        }
        this.paraEntregar = res;
    }

    /**
     * Indica as encomendas que se encontram no percurso que a transportadora está a percorrer
     * @return As encomendas que se encontram no percurso que a transportadora está a percorrer
     */
    public List<Encomenda> getNoCaminho() {
        List<Encomenda> res = new ArrayList<>();
        for(Encomenda e : this.noCaminho){
            res.add(e.clone());
        }
        return res;
    }

    /**
     * Define as encomendas que se encontram no percurso que a transportadora está a percorrer
     * @param encs As encomendas que se encontram no percurso que a transportadora está a percorrer a definir
     */
    public void setNoCaminho(List<Encomenda> encs){
        List<Encomenda> res = new ArrayList<>();
        for(Encomenda e : encs){
            res.add(e.clone());
        }
        this.noCaminho = res;
    }

    /**
     * Incia o número total de kms percorridos
     * @return O número total de kms percorridos
     */
    public Double getTotal_km() {
        return total_km;
    }

    /**
     * Define o número total de kms percorridos
     * @param total_km O número total de kms percorridos a definir
     */
    public void setTotal_km(Double total_km) {
        this.total_km = total_km;
    }

    /**
     * Define o NIF
     * @param nif O NIF a definir
     */
    public void setNif(String nif) {
        this.nif = nif;
    }

    /**
     * Define o preço por km
     * @param prec_km O preço por km a definir
     */
    public void setPrec_km(Double prec_km) {
        this.prec_km = prec_km;
    }

    /**
     * Indica o preço por km
     * @return O preço por km
     */
    public Double getPrec_km() {
        return prec_km;
    }

    /**
     * Adiciona uma lista de encomendas às encomendas para entregar
     * @param e A lista das encomendas a adicionar
     */
    public void addEncomendas(List<Encomenda> e) {
        if(paraEntregar == null)
            paraEntregar = new ArrayList<>();
        for (Encomenda enc: e) {
            paraEntregar.add(enc.clone());
        }
    }

    /**
     * Limpa a lista de encomendas para entregar
     */
    public void clearEncomendas() {
        paraEntregar.clear();
    }

    /**
     * Remove uma encomenda das encomendas para entregar
     * @param e A encomenda a remover
     */
    public void removeEncomenda(Encomenda e) {
        paraEntregar.remove(e);
    }

    /**
     * Remove uma encomenda das encomendas que se encontram no percurso que a transportadora está a percorrer
     * @param e A encomenda a remover
     */
    public void removeEncomendaCaminho(Encomenda e) {
        noCaminho.remove(e);
    }

    /**
     * Verifica se um objeto é igual a uma transportadora
     * @param o O objeto com o qual se pretende comparar
     * @return True caso afirmativo e false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Transportadora)) return false;
        return super.equals(o);
    }

    /**
     * Cria o clone de uma transportadora
     * @return O clone da transportadora
     */
    public Transportadora clone() {
        return new Transportadora(this);
    }

    /**
     * Indica o NIF da transportadora
     * @return O NIF da transportadora
     */
    public String getNif() {
        return nif;
    }

    /**
     * Verifica se é possível entregar uma encomenda num dado percurso
     * @param whereCliente As coordenadas GPS do cliente
     * @param noCaminho As coordenadas GPS da encomenda
     * @return True caso afirmativo e false caso contrário
     */
    public boolean encomendasNoCaminho(GPS whereCliente,GPS noCaminho) {
        GPS whereTransp = getGps();

        if(whereCliente.equals(noCaminho)) {
            return true;
        }


        double x0 = noCaminho.getLon();
        double y0 = noCaminho.getLat();

        double y2 = whereTransp.getLat();
        double y1 = whereCliente.getLat();

        double x2 = whereTransp.getLon();
        double x1 = whereCliente.getLon();

        double distNum = Math.abs((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1);
        double distDen = Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1) );
        double dist;

        if(distDen != 0){
            dist = distNum / distDen;
        } else
            return false;

        if(dist > 0.05)
            return false;

        double minX,minY,maxX,maxY;

        if(y2 < y1) {
            minY = y2 - 0.05;
            maxY = y1 + 0.05;
        }else{
            maxY = y2 + 0.05;
            minY = y1 - 0.05;
        }

        if(x2 < x1) {
            minX = x2 - 0.05;
            maxX = x1 + 0.05;
        }else{
            maxX = x2 + 0.05;
            minX = x1 - 0.05;
        }


        return x0 > minX && x0 < maxX && y0 > minY && y0 < maxY;

    }

    /**
     * Indica a encomenda correspondente ao código dado caso ela exista na lista de encomendas para entregar
     * @param s O código da encomenda que se pretende obter
     * @return A encomenda correspondete ao código
     * @throws EncomendaInexistenteException Caso não exista a encomenda na lista das encomendas para entregar
     */
    public Encomenda getEncParaEntregar(String s) throws EncomendaInexistenteException {
        Iterator<Encomenda> it = paraEntregar.iterator();
        boolean found = false;
        Encomenda ret = null;
        while(it.hasNext() && !found){
            ret = it.next();
            if(ret.getNumEnc().equals(s)){
                found = true;
            }
        }

        if(found){
            return  ret;
        }
        throw new EncomendaInexistenteException("Encomenda não existe.");
    }

    /**
     * Adiciona uma encomenda à lista das encomendas para entregar
     * @param e A encomenda que se pretende adicionar
     */
    public void addEncomenda(Encomenda e){
        paraEntregar.add(e);
    }

    /**
     * Indica a faturação total num período de tempo
     * @param inicio O início do período
     * @param fim O fim do período
     * @return A faturação total num dado período de tempo
     * @throws EncomendaInexistenteException Caso não hajam encomendas
     */
    public double faturacaoTotal(LocalDateTime inicio, LocalDateTime fim) throws EncomendaInexistenteException {

        double res = 0;

        for(Map.Entry<Utilizador, Set<Encomenda>> u : this.getEncEntregues().entrySet()){
            for(Encomenda e : u.getValue()){
                if(e.getDataEntrega().isAfter(inicio) && e.getDataEntrega().isBefore(fim)){
                    res += e.getPreco() - e.ValorTotal();
                }
            }
        }

        return res;
    }

    /**
     * Aumenta o número total de kilometros
     * @param dist O número de kilometros a incrementar
     */
    public void aumentaDistancia(Double dist){
        this.total_km += dist;
    }

    /**
     * Encomendas entregues realizadas num dado período
     * @param inicio O início do período
     * @param fim O fim do período
     * @return As encomendas entregues realizadas num dado período
     * @throws EncomendaInexistenteException Caso não hajam encomendas
     */
    public List<Encomenda> encomendasEntre(LocalDateTime inicio, LocalDateTime fim) throws EncomendaInexistenteException {

        List<Encomenda> res = new ArrayList<>();

        for(Map.Entry<Utilizador, Set<Encomenda>> u : this.getEncEntregues().entrySet()){
            for(Encomenda e : u.getValue()){
                if(e.getDataEntrega().isAfter(inicio) && e.getDataEntrega().isBefore(fim)){
                    res.add(e.clone());
                }
            }
        }

        return res;

    }

}

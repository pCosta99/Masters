import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

public class CatTransportadoras implements Serializable  {
    /**Map que representa o catálogo de transportadoras sendo as keys os seus codigos e os values as respectivas Transportadoras*/
    private Map<String,Transportadoras> trans;

    public CatTransportadoras () {
        this.trans = new HashMap<>();
    }

    /**Método que adiciona uma nova transportadora ao catálogo de transportadoras*/
    public void adicionaTrans (Transportadoras a){
        this.trans.put(a.getCod(),a.clone());
    }

    /**Método que dado as coordenas da loja e do utilizador e um boolean de prioridade retorna uma lista de StringDistAux respectiva às Transportadoras com melhores condições para a encomenda
     *
     * @param cl Coordenadas Loja
     * @param cu Coordenadas Utilizador
     * @param pri Boolean prioridade
     */
    public List<StringDistAux> melhorTransporteT(Coordenadas cl, Coordenadas cu, boolean pri){
        return this.trans.values().stream().filter(x->x.isRangeT(cl,x.getRange())).filter(x->!(!x.isMed() && pri))
                .filter(x->x.isRangeT(cu,x.getRange())).filter(x->x.getCap()>0)
                .map(x->new StringDistAux(x.distanceT(cl),x.getCod())).collect(Collectors.toList());
    }

    /**Método que retorna o codigo de uma transportadora caso o username e password derem match com uma das transportadoras registadas
     *
     * @param e Email
     * @param p Password
     */
    public String loginT (String e, String p){
        for (Transportadoras a : this.trans.values()){
            if (a.getEmail().equals(e) && a.getPass().equals(p)) return a.getCod();
        }
        return null;
    }

    /**Método que gera um código único de transportadora*/
    public String codUnicoT (){
        String a = null;
        while (this.trans.containsKey(a) || a == null) {
            StringBuilder sb = new StringBuilder();
            sb.append("t").append(ThreadLocalRandom.current().nextInt(10, 99));
            a = sb.toString();
        }
        return a;
    }

    /**Método que regista uma nova transportadora no sistema (sign up)
     *
     * @param cod Código
     * @param nome Nome
     * @param cord Coordenadas
     * @param range Range
     * @param prkm Preço kilómetro
     * @param nif NIF
     * @param clas Classificação
     * @param med Medicamentos
     * @param vkm Velocidade
     * @param email Email
     * @param pass Password
     * @param kmp Kilómetros percorridos
     * @param cap Capacidade
     */
    public void signupT (String cod, String nome, Coordenadas cord, double range, double prkm, double nif, double clas, boolean med,double vkm , String email, String pass,double kmp, int cap){
        this.trans.put(cod,new Transportadoras(cod, nome, cord, range, prkm, nif, clas, med,vkm, email, pass,kmp,cap));
    }

    /**Método que retorna o tempo de transporte de uma determinada transportadora
     *
     * @param l Coordenadas loja
     * @param u Coordenadas utilizador
     * @param cl Distância
     * @param driver Código condutor
     */
    public double calcTmp(Coordenadas l,Coordenadas u,Double cl,String driver){
        return this.trans.get(driver).calcTemp(l,u,cl);
    }

    /**Método que retorna o custo de viajem de uma determinada Transportadora
     *
     * @param l Coordenadas loja
     * @param u Coordenadas utilizador
     * @param peso Peso
     * @param driver Código condutor
     */
    public double calcCost(Coordenadas l,Coordenadas u,double peso,String driver){
        return this.trans.get(driver).calcCost(l,peso,u);
    }

    /**Método que retorna true se o email da Transportadora estiver registado no catálogo
     * @param email Email
     */
    public boolean verMail (String email){
        return this.trans.values().stream().anyMatch(x->x.getEmail().equals(email));
    }

    /**Método que adiciona uma classificação a uma determinada Transportadora
     *
     * @param driver Código condutor
     * @param rating Rating
     */
    public void adicionaCla (String driver, double rating){
        this.trans.get(driver).changeR(rating);
    }

    /**Método que retorna uma classificação de uma determinada Transportadora
     * @param driver Código condutor */
    public double getClas (String driver){
           return this.trans.get(driver).getClas();
    }

    /**Método que retorna as Coordenadas de uma determinada Transportadora
     *
     * @param user Código
     */
    public Coordenadas getCord (String user){
        return this.trans.get(user).getCord().clone();
    }

    /**Método que altera as Coordenadas de uma determinada Transportadora
     *
     * @param driver Código condutor
     * @param a Coordenadas
     */
    public void setCoords (String driver, Coordenadas a){
        this.trans.get(driver).setCord(a);
    }

    /**Método que retorna os kilometros percorridos por uma determinada Transportadora
     *
     * @param cod Código condutor
     */
    public double getKmp (String cod){
        return this.trans.get(cod).getKmp();
    }

    /**Método que faz set dos kilometros percorridos de uma determinada Transportadora dependendo da sua distância à loja e utilizador
     *
     * @param cod Código condutor
     * @param l Coordenadas
     * @param u Coordenadas
     */
    public void addKmps (String cod , Coordenadas l , Coordenadas u){
        this.trans.get(cod).setKmp(this.trans.get(cod).getCord().distancia(l)+l.distancia(u));
    }

    /**Método que diminui a capacidade de uma determinada Transportadora
     *
     * @param cod Código condutor
     */
    public void dimCap (String cod){
        this.trans.get(cod).dimCap();
    }

    /**Método que aumenta a capacidade de uma determinada Transportadora
     *
     * @param cod Código condutor
     */
    public  void aumCap (String cod){
        this.trans.get(cod).aumCap();
    }

    /**Método que retorna a capacidade de uma determinada Transportadora
     *
     * @param cod Código condutor
     */
    public int getCap (String cod){
        return this.trans.get(cod).getCap();
    }
}


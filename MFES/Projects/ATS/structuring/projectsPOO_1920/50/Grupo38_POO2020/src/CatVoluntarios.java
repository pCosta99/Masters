import java.io.Serializable;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

public class CatVoluntarios implements Serializable  {
    /**Map que representa o catálogo de voluntários sendo as keys os seus codigos e os values os respectivos voluntários*/
    private Map<String,Voluntario> volunt;

    public Map<String,Voluntario> getVoluntario () {
        Map<String, Voluntario> a = new HashMap<>();
        for (Map.Entry<String,Voluntario> b : this.volunt.entrySet()) {
            a.put(b.getKey(),b.getValue());
        }
        return a;
    }

    public CatVoluntarios () {
        this.volunt = new HashMap<>();
    }

    /**Método que adiciona um novo voluntário ao catálogo de voluntários
     *
     * @param a Voluntário
     */
    public void adicionaVolunt (Voluntario a){
        this.volunt.put(a.getCod(),a.clone());
    }

    /**Método que dado as coordenas da loja e do utilizador e um boolean de prioridade retorna uma lista de StringDistAux respectiva aos voluntários com melhores condições para a encomenda
     *
     * @param cl Coordenadas
     * @param cu Coordenadas
     * @param pri prioridade
     */
    public List<StringDistAux> melhorTransporteV (Coordenadas cl, Coordenadas cu, boolean pri){
        return this.volunt.values().stream().filter(x->x.isRangeV(cl,x.getRange())).filter(x->!(!x.isMed() && pri))
                .filter(x->x.isRangeV(cu,x.getRange())).filter(x-> x.getCap()>0)
                .map(x->new StringDistAux(x.distanceV(cl),x.getCod())).collect(Collectors.toList());
    }

    /**Método que retorna o tempo de transporte de um determinado voluntário
     *
     * @param l Coordenadas loja
     * @param u Coordenadas utilizador
     * @param cl Distância
     * @param driver Código condutor
     */
    public double calcTmp(Coordenadas l,Coordenadas u,Double cl,String driver){
        return this.volunt.get(driver).calcTemp(l,u,cl);
    }

    /**Método que retorna o codigo de um voluntário caso o username e password derem match com um dos voluntários registados
     *
     * @param e Email
     * @param p Password
     */
    public String loginV (String e, String p){
        for (Voluntario a : this.volunt.values()){
            if (a.getEmail().equals(e) && a.getPass().equals(p)) return a.getCod();
        }
        return null;
    }

    /**Método que gera um código único de voluntário*/
    public String codUnicoV (){
        String a = null;
        while (this.volunt.containsKey(a) || a == null) {
            StringBuilder sb = new StringBuilder();
            sb.append("v").append(ThreadLocalRandom.current().nextInt(10, 99));
            a = sb.toString();
        }
        return a;
    }

    /**Método que retorna true se o email do Voluntário estiver registado no catálogo
     *
     * @param email Email
     */
    public boolean verMail (String email){
        return this.volunt.values().stream().anyMatch(x->x.getEmail().equals(email));
    }

    /**Método que regista um novo Voluntário no sistema (sign up)
     *
     * @param cod Código voluntário
     * @param nome Nome voluntário
     * @param range Range
     * @param cord Coordenadas
     * @param clas Classificação
     * @param med Boolean Medicamentos
     * @param vkm Velocidade
     * @param email Email
     * @param pass Password
     * @param cap Capacidade
     */
    public void signupV (String cod, String nome, double range, Coordenadas cord, double clas, boolean med, double vkm, String email, String pass, int cap){
        this.volunt.put(cod,new Voluntario(cod, nome, range,cord, clas, med,vkm, email,pass,cap));
    }

    /**Método que adiciona uma classificação a um determinado Voluntário
     *
     * @param driver Código condutor
     * @param rating Rating
     */
    public void adicionaCla (String driver, double rating){
        this.volunt.get(driver).changeR(rating);
    }

    /**Método que retorna uma classificação de uma determinada Voluntário
     *
     * @param driver Código condutor
     */
    public double getClas (String driver){
        return this.volunt.get(driver).getClas();
    }

    /**Método que retorna as Coordenadas de um determinado Voluntário
     *
     * @param user Código condutor
     */
    public Coordenadas getCord (String user){
        return this.volunt.get(user).getCord().clone();
    }

    /**Método que altera as Coordenadas de um determinado Voluntário
     *
     * @param driver Código condutor
     * @param a Coordenadas
     */
    public void setCoords (String driver, Coordenadas a){
        this.volunt.get(driver).setCord(a);
    }

    /**Método que diminui a capacidade de um determinado Voluntário
     *
     * @param cod Código condutor
     */
    public void dimCap (String cod){
        this.volunt.get(cod).dimCap();
    }

    /**Método que aumenta a capacidade de um determinado Voluntário
     *
     * @param cod Código Condutor
     */
    public  void aumCap (String cod){
        this.volunt.get(cod).aumCap();
    }

    /**Método que retorna a capacidade de uma determinada Voluntário
     *
     * @param cod Código condutor
     */
    public int getCap (String cod){
        return this.volunt.get(cod).getCap();
    }
}

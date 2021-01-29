import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

public class CatLojas implements Serializable {
    /**Map que representa o catálogo de lojas sendo as keys os seus codigos e os values as respectivas Lojas*/
    private Map<String,Loja> lojas;

    public Map<String,Loja> getLojas () {
        Map<String, Loja> a = new HashMap<>();
        for (Map.Entry<String,Loja> b : this.lojas.entrySet()) {
            a.put(b.getKey(),b.getValue());
        }
        return a;
    }

    public CatLojas () {
        this.lojas = new HashMap<>();
    }

    /**Método que adiciona uma nova loja ao catálogo de lojas
     *@param a Loja */
    public void adicionaLoja (Loja a){
        this.lojas.put(a.getCod(),a.clone());
    }

    /**Método que retorna uma lista das lojas que estão num determinado range
     *@param a  Coordenadas
     *@param b Distância */
    public List<Loja> LojasinRange(Coordenadas a, double b){
        return this.lojas.values().stream().filter(x->x.rangeLoja(a,b)).map(x->x.clone()).collect(Collectors.toList());
    }

    /**Método que retorna o tempo de fila de uma determinada loja no catálogo
     *@param loja Código loja */
    public double getTmpF (String loja){
        return this.lojas.get(loja).calcTmp();
    }

    /**Método que retorna as coordenas de uma determinada loja no catálogo
     *@param loja Código loja */
    public Coordenadas getCord (String loja){
        return this.lojas.get(loja).getCord().clone();
    }

    /**Método que retorna o codigo de uma loja caso o username e password derem match com uma das lojas registadas
     *@param e email
     *@param p password */
    public String loginL (String e, String p){
        for (Loja a : this.lojas.values()){
            if (a.getEmail().equals(e) && a.getPass().equals(p)) return a.getCod();
        }
        return null;
    }

    /**Método que regista uma nova loja no sistema (sign up)
     *
     * @param cod Código loja
     * @param nome Nome Loja
     * @param cord  Coordenadas Loja
     * @param fila  Fila Loja
     * @param email Email Loja
     * @param pass  Password Loja
     */
    public void signupL (String cod, String nome, Coordenadas cord,int fila, String email, String pass){
        this.lojas.put(cod,new Loja(cod, nome, cord,fila , email, pass));
    }

    /**Método que retorna true se o email da loja estiver registado no catálogo
     *@param email Email loja */
    public boolean verMail (String email){
        return this.lojas.values().stream().anyMatch(x->x.getEmail().equals(email));
    }

    /**Método que gera um código único de loja*/
    public String codUnicoL (){
        String a = null;
        while (this.lojas.containsKey(a) || a == null) {
            StringBuilder sb = new StringBuilder();
            sb.append("l").append(ThreadLocalRandom.current().nextInt(1, 999));
            a = sb.toString();
        }
        return a;
    }
}

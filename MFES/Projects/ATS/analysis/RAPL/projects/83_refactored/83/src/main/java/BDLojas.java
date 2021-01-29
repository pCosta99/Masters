import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class BDLojas implements Serializable {
    private Map<String, Loja> lojas;
    private Set<String> codigos;

    /**
     * Construtores
     */

    public BDLojas(){
        this.lojas = new HashMap<>();
        this.codigos = new TreeSet<>();
    }

    public BDLojas(BDLojas r){
        setLojas(r.getLojas());
        setCodigos(r.getCodigos());
    }

    //Getters

    public Map<String, Loja> getLojas() {
        return this.lojas.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, r -> r.getValue().clone()));
    }

    public Set<String> getCodigos() {
        return new HashSet<>(this.codigos);
    }

    //Setters
    public void setCodigos(Set<String> codigos) {
        this.codigos = new TreeSet<>();
        this.codigos.addAll(codigos);
    }

    public void setLojas(Map<String, Loja> lojas) {
        this.lojas = new HashMap<>();
        lojas.forEach((key, value) -> this.lojas.put(key, value.clone()));
    }


    public BDLojas clone(){
        return new BDLojas(this);
    }

    public String toString(){
        return "Total de Lojas: " + "\n" +
                this.lojas;
    }

    public boolean equals(Object obj){
        if(obj == this) return true;
        if(obj == null || obj.getClass() != this.getClass()) return false;
        BDLojas r = (BDLojas) obj;
        return this.lojas.equals(r.getLojas());
    }

    /**
     * Método que verifica se uma loja existe
     * @param v é a loja que se pretende saber se existe
     * @return se a loja existe
     */

    public boolean existe(Loja v){
        return this.lojas.containsKey(v.getEmail());
    }

    /**
     * Método que adiciona uma encomenda a uma loja
     * @param e é a encomenda
     * @param j é a loja
     */
    public void updateLoja(Encomenda e, Loja j){
        j.addEncomenda(e);
        this.lojas.put(j.getEmail(), j);
    }

    /**
     * Método que remove uma encomenda de uma loja
     * @param e é a encomenda
     * @param j é a loja
     */

    public void updateLoja2(Encomenda e ,Loja j){
        j.removeEncomenda(e);
        this.lojas.put(j.getEmail(), j);
    }

    /**
     * Método que atualiza uma loja
     * @param j loja
     */
    public void updateLoja3(Loja j){
        this.lojas.put(j.getEmail(), j);
    }

    /**
     * Método que verifica se um email já está registado
     * @param email email
     * @return se existe o email
     */
    public boolean existeEmail(String email){
        return this.lojas.containsKey(email);
    }

    /**
     * Método que verifica se um código de uma loja existe
     * @param s é o código a verificar
     * @return se existe o codigo
     */

   public boolean existeCodigo(String s){
        return this.codigos.contains(s);
    }

    /**
     * Método que adiciona uma loja
     * @param l loja
     */

    public void add(Loja l){
        this.lojas.put(l.getEmail(), l.clone());
        this.codigos.add(l.getCodigo());
    }

    /**
     * Método que tenta efetuar o login de uma loja no sistema
     * @param email email
     * @param password password
     * @return a loja em que foi feito o login
     */
    public Loja tryLogin(String email, String password){
        Loja aux = this.lojas.get(email);
        if(aux == null) System.out.println("Não existe essa loja");
        else{
            if(aux.getPassword().equals(password)){
                System.out.println("Login feito com sucesso");
                return aux;
            }
            else{
                System.out.println("Password incorreta");
                return null;
            }
        }
        return null;
    }

    /**
     * Método que imprime as lojas, indicando a distância a uma determinado utilizador doméstico
     * @param u é o utilizador que serve de referência
     * @return lojas do utilizador
     */
    public String listLojasUser(Utilizador u){
        StringBuilder sb = new StringBuilder();
        sb.append("LISTA DE LOJAS\n");
        for(Loja l : this.lojas.values()){
            double dist = DistanceCalculator.distance(u.getLatitude(), l.getLatitude(), u.getLongitude(), l.getLongitude());
            sb.append(l.getCodigo()).append(" --> ").append(l.getNome()).append(" ----> DIST:  ").append(dist).append(" KMS").append("\n");
        }
        return sb.toString();
    }

    /**
     * Método que devolve o email de uma loja, quando lhe é fornecido o seu código
     * @param cod codigo
     * @return email
     * @throws LojaNotFoundException caso nao exista a loja
     */

    public String getEmail(String cod) throws LojaNotFoundException{
        for(Loja l : this.lojas.values()){
            if(l.getCodigo().equals(cod)) return l.getEmail();
        }
        throw new LojaNotFoundException();
    }
}

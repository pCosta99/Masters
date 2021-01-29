import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class BDVoluntarios implements Serializable {
    private Map<String, Voluntario> voluntarios;
    private Set<String> codigos;

    public BDVoluntarios(){
        this.voluntarios = new HashMap<>();
        this.codigos = new TreeSet<>();
    }

    public BDVoluntarios(BDVoluntarios r){
        setVoluntarios(r.getVoluntarios());
        setCodigos(r.getCodigos());
    }

    public Map<String, Voluntario> getVoluntarios() {
        return this.voluntarios.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, r -> r.getValue().clone()));
    }

    public void setVoluntarios(Map<String, Voluntario> voluntarios) {
        this.voluntarios = new HashMap<>();
        voluntarios.forEach((key, value) -> this.voluntarios.put(key, value.clone()));
    }

    public Set<String> getCodigos() {
        return new HashSet<>(this.codigos);
    }

    public void setCodigos(Set<String> codigos) {
        this.codigos = new TreeSet<>();
        this.codigos.addAll(codigos);
    }


    public BDVoluntarios clone(){
        return new BDVoluntarios(this);
    }

    public String toString(){
        return "Total de Voluntarios: " + "\n" +
                this.voluntarios;
    }

    public boolean equals(Object obj){
        if(obj == this) return true;
        if(obj == null || obj.getClass() != this.getClass()) return false;
        BDVoluntarios r = (BDVoluntarios) obj;
        return this.voluntarios.equals(r.getVoluntarios());
    }
    /**
     * Método que verifica se um email já está registado
     */
    public boolean existeEmail(String email){
        return this.voluntarios.containsKey(email);
    }


    public boolean existe(Voluntario v){
        return this.voluntarios.containsKey(v.getEmail());
    }
    public boolean existeCodigo(String s){
        return this.codigos.contains(s);
    }

    /**
     * Método que adiciona um voluntário
     */

    public void add(Voluntario v){
        this.voluntarios.put(v.getEmail(), v.clone());
        this.codigos.add(v.getCodigo());
    }

    /**
     * Método que efetua o login de um voluntário
     */
    public Voluntario tryLogin(String email, String password){
        Voluntario aux = this.voluntarios.get(email);
        if(aux == null) System.out.println("Não existe esse voluntário");
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
     * Método que devolve os voluntário a imprimir
     */
    public String printVoluntario(){
        StringBuilder sb = new StringBuilder();
        for(Voluntario v : this.voluntarios.values()){
            sb.append(v.getCodigo()).append(" ---> ").append(v.getNome()).append(" || RATE --> ").append(v.getClassificacao()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Método que devolve a lista de voluntário disponíveis para se deslocarem a uma determinada loja
     */
    public List<Voluntario> voluntariosDisponiveis(Loja j, Utilizador u) {
        List<Voluntario> ret = new ArrayList<>();
        for(Voluntario v : this.voluntarios.values()){
            double dist1 = DistanceCalculator.distance(j.getLatitude(), v.getLatitude(), j.getLongitude(), v.getLongitude());
            double dist2 = DistanceCalculator.distance(j.getLatitude(), u.getLatitude(), j.getLongitude(), u.getLongitude());
            if (dist1 <= v.getRaioAcao() && dist2 <= v.getRaioAcao() && v.getDisponibilidade()) {
                ret.add(v.clone());
            }
        }
        return ret;
    }

    public List<Voluntario> voluntariosDisponiveisMed(Loja j , Utilizador u) {
        List<Voluntario> ret = new ArrayList<>();
        for(Voluntario v : this.voluntarios.values()){
            double dist1 = DistanceCalculator.distance(j.getLatitude(), v.getLatitude(), j.getLongitude(), v.getLongitude());
            double dist2 = DistanceCalculator.distance(j.getLatitude(), u.getLatitude(), j.getLongitude(), u.getLongitude());
            if (dist1 <= v.getRaioAcao() && dist2 <= v.getRaioAcao() && v.getDisponibilidade() && v.aceitoTransporteMedicamentos()) {
                ret.add(v.clone());
            }
        }
        return ret;
    }

    /**
     * Método que devolve o email de um voluntário
     */
    public String getEmail(String cod) throws VoluntarioNotFoundException{
        for(Voluntario v : this.voluntarios.values()){
            if(v.getCodigo().equals(cod)) return v.getEmail();
        }
        throw new VoluntarioNotFoundException();
    }

    /**
     * Método que atualiza a classificação de um voluntário
     */

    public void updateVoluntario(Voluntario v, double classificao){
        v.updateRate(classificao);
        this.voluntarios.put(v.getEmail(), v);
    }

    /**
     * Método que atualiza o voluntário v
     */
    public void updateVoluntario2(Voluntario v){
        this.voluntarios.put(v.getEmail(), v);
    }

    /**
     * Método que devolve o voluntário que tem a encomenda com o código enc
     */
    public Voluntario encontraEnc(String enc) throws EncomendaNotFoundException{
        for(Voluntario aux : this.voluntarios.values()){
            if(aux.existe(enc)) return aux;
        }
        throw new EncomendaNotFoundException();
    }

    /**
     * Método que diz se a encomenda existe na BDVoluntários
     */
    public boolean existeEnc(String enc){
        for(Voluntario aux : this.voluntarios.values()){
            if(aux.existe(enc)) return true;
        }
        return false;
    }

}

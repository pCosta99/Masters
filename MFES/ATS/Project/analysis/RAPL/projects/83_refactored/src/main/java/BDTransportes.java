import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class BDTransportes implements Serializable {

    private Map<String, EmpresaTransportes> transportes;
    private Set<String> codigos;

    public BDTransportes(){
        this.transportes = new HashMap<>();
        this.codigos = new TreeSet<>();
    }

    public BDTransportes(BDTransportes r){
        setTransportes(r.getTransportes());
        setCodigos(r.getCodigos());
    }

    public Map<String, EmpresaTransportes> getTransportes() {
        return this.transportes.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, r -> r.getValue().clone()));
    }

    public Set<String> getCodigos() {
        return new HashSet<>(this.codigos);
    }

    public void setCodigos(Set<String> codigos) {
        this.codigos = new TreeSet<>();
        this.codigos.addAll(codigos);
    }

    public void setTransportes(Map<String, EmpresaTransportes> transportes) {
        this.transportes = new HashMap<>();
        transportes.forEach((key, value) -> this.transportes.put(key, value.clone()));
    }


    public BDTransportes clone(){
        return new BDTransportes(this);
    }

    public String toString(){
        return "Total de Empresas de transporte: " + "\n" +
                this.transportes;
    }

    public boolean equals(Object obj){
        if(obj == this) return true;
        if(obj == null || obj.getClass() != this.getClass()) return false;
        BDTransportes r = (BDTransportes) obj;
        return this.transportes.equals(r.getTransportes());
    }

    /**
     * Método que verifica se um dado email já está registado
     * @param email email
     */

    public boolean existeEmail(String email){
        return this.transportes.containsKey(email);
    }

    /**
     * Método que verifica se uma empresa de transportes existe
     * @param v empresa transportes
     */
    public boolean existe(EmpresaTransportes v){
        return this.transportes.containsKey(v.getEmail());
    }

    /**
     * Método que verifica se o código de uma empresa existe
     * @param s codigo
     */
    public boolean existeCodigo(String s){
        return this.codigos.contains(s);
    }

    /**
     * Método que adiciona uma empresa de transportes
     * @param t empresa de transportes
     */

    public void add(EmpresaTransportes t){
        this.transportes.put(t.getEmail(), t.clone());
        this.codigos.add(t.getCodigo());
    }

    /**
     * Método que tenta efetuar o login de uma empresa de transportes
     */
    public EmpresaTransportes tryLogin(String email, String password){
        EmpresaTransportes aux = this.transportes.get(email);
        if(aux == null) System.out.println("Não existe essa empresa de transportes");
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
     * Método que devolve os transportes a serem impressos, bem como a sua classificação
     */
    public String printTransportes(){
        StringBuilder sb = new StringBuilder();
        for(String s: this.transportes.keySet()){
            sb.append(this.transportes.get(s).clone().getCodigo()).append(" ---> ").append(this.transportes.get(s).clone().getNome()).append(" || RATE --> ").append(this.transportes.get(s).clone().getClassificao()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Método que devolve o email de uma empresa, dando lhe o seu código
     * @throws TransporteNotFoundException caso nao exista transporte
     */
    public String getEmail(String cod) throws TransporteNotFoundException{
        for(String s: this.transportes.keySet()){
            if(this.transportes.get(s).clone().getCodigo().equals(cod)) return this.transportes.get(s).getEmail();
        }
        throw new TransporteNotFoundException();
    }

    /**
     * Método que atualiza a calssificação de uma empresa de transportes
     */
    public void updateTransporte(EmpresaTransportes e, double classificao){
        e.updateRate(classificao);
        this.transportes.put(e.getEmail(), e);
    }

    /**
     * Método que devolve as empresas de transportes disponíveis e o custo de efetuar a entrega da encomenda
     */
    public String printEmpresas(Utilizador u, Loja j, double peso) {
        StringBuilder sb = new StringBuilder();
        int count = 0;
        for (String s : this.transportes.keySet()) {
            EmpresaTransportes et = this.transportes.get(s).clone();
            double dist1 = DistanceCalculator.distance(j.getLatitude(), et.getLatitude(), j.getLongitude(), et.getLongitude());
            double dist2 = DistanceCalculator.distance(j.getLatitude(), u.getLatitude(), j.getLongitude(), u.getLongitude());
            if(dist1 <= et.getRaioDeAcao() && dist2 <= et.getRaioDeAcao() && et.isDisponivel()){
                double custo = dist1 * et.getCusto_km() + dist2 *et.getCusto_km() + (peso * 0.2);
                sb.append(et.getCodigo()).append(" ---> ").append(et.getNome()).append(" || RATE --> ").append(et.getClassificao()).append(" || CUSTO: ").append(custo).append("\n");
                count++;
            }
        }
        if(count == 0) sb.append("0");
        return sb.toString();
    }

    public String printEmpresasMed(Utilizador u, Loja j, double peso) {
        StringBuilder sb = new StringBuilder();
        int count = 0;
        for (String s : this.transportes.keySet()) {
            EmpresaTransportes et = this.transportes.get(s).clone();
            double dist1 = DistanceCalculator.distance(j.getLatitude(), et.getLatitude(), j.getLongitude(), et.getLongitude());
            double dist2 = DistanceCalculator.distance(j.getLatitude(), u.getLatitude(), j.getLongitude(), u.getLongitude());
            if(dist1 <= et.getRaioDeAcao() && dist2 <= et.getRaioDeAcao() && et.aceitoTransporteMedicamentos() && et.isDisponivel()){
                double custo = dist1 * et.getCusto_km() + dist2 *et.getCusto_km() + (peso * 0.2);
                sb.append(et.getCodigo()).append(" ---> ").append(et.getNome()).append(" || RATE --> ").append(et.getClassificao()).append(" || CUSTO: ").append(custo).append("\n");
                count++;
            }
        }
        if(count == 0) sb.append("0");
        return sb.toString();
    }

    public void updateTransportes2(EmpresaTransportes et){
        this.transportes.put(et.getEmail(), et);
    }

    public List<EmpresaTransportes> transDisponiveis(Loja j, Utilizador u) {
        List<EmpresaTransportes> ret = new ArrayList<>();
        for (String s : this.transportes.keySet()) {
            EmpresaTransportes et = this.transportes.get(s).clone();
            double dist1 = DistanceCalculator.distance(j.getLatitude(), et.getLatitude(), j.getLongitude(), et.getLongitude());
            double dist2 = DistanceCalculator.distance(j.getLatitude(), u.getLatitude(), j.getLongitude(), u.getLongitude());
            if (dist1 <= et.getRaioDeAcao() && dist2 <= et.getRaioDeAcao() && et.isDisponivel()) {
                ret.add(et.clone());
            }
        }
        return ret;
    }

    public List<EmpresaTransportes> transDisponiveisMedParse(Loja j , Utilizador u) {
        List<EmpresaTransportes> ret = new ArrayList<>();
        for (String s : this.transportes.keySet()) {
            EmpresaTransportes et = this.transportes.get(s).clone();
            double dist1 = DistanceCalculator.distance(j.getLatitude(), et.getLatitude(), j.getLongitude(), et.getLongitude());
            double dist2 = DistanceCalculator.distance(j.getLatitude(), u.getLatitude(), j.getLongitude(), u.getLongitude());
            if (dist1 <= et.getRaioDeAcao() && dist2 <= et.getRaioDeAcao() && et.aceitoTransporteMedicamentos() && et.isDisponivel()){
                ret.add(et.clone());
            }
        }
        return ret;
    }

    public EmpresaTransportes encontraEnc(String enc) throws EncomendaNotFoundException{
        EmpresaTransportes aux;
        for(String s: this.transportes.keySet()){
            aux = this.transportes.get(s).clone();
            if(aux.existe(enc)) return aux;
        }
        throw new EncomendaNotFoundException();
    }

}

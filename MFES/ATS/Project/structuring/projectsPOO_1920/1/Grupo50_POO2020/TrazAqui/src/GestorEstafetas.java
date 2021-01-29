import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class GestorEstafetas implements Serializable{
    private Map<String, Estafeta> estafetas;

    public GestorEstafetas(){
        this.estafetas = new HashMap<String, Estafeta>();
    }

    public GestorEstafetas(Map<String, Estafeta> estafetas){
        this.estafetas = estafetas;
    }

    public GestorEstafetas(GestorEstafetas umaBaseDados){
        this.estafetas = umaBaseDados.getEstafetas();
    }


    /**
     * Getters & Setters
     */

    /**
     * Obter uma copia do mapeamento das Estafetas
     * @return Map<String: código da Estafeta, Estafeta>
     */
    public Map<String, Estafeta> getEstafetas(){
        return this.estafetas;
    }

    /**
     * @param novasEstafetas
     */
    public void setEstafetas(Map<String, Estafeta> novasEstafetas){
        this.estafetas = novasEstafetas;
    }

    /**
     * Verificar a existência de uma encomenda dado o seu codigo
     * @param codigo da encomenda
     * @return True se a encomenda existir
     */
    public boolean existeEstafeta(String codigo){
        return this.estafetas.containsKey(codigo);
    }

    /**
     *
     * @return Número de estafetas
     */
    public int numEstafetas(){
        return this.estafetas.size();
    }

    /**
     * Adiciona a informação de uma nova encomenda
     * @param e nova encomenda a inserir
     */
    public void addEstafeta(Estafeta e) throws EntidadeRepetidaException{
        if(this.estafetas.putIfAbsent(e.getCodigo(), e.clone()) != null)
            throw new EntidadeRepetidaException("Já existe uma estafeta com o essa referência " + e.getCodigo() + " na App");
    }

    /**
     *
     * @param cod código da encomenda a procurar
     * @return cópia da encomenda encontrada
     * @throws EntidadeInexistenteException caso o código da encomenda não exista
     */
    public Estafeta getEstafeta(String cod) throws EntidadeInexistenteException{
        Estafeta l;

        try{
            l = this.estafetas.get(cod).clone();
        }
        catch(NullPointerException e){
            throw new EntidadeInexistenteException("Não existe objeto com nome " + cod);
        }
        return l;
    }

    public int tamanho(){
        return this.estafetas.size();
    }

    /**
     *
     * @return Lista das estafetas
     */
    public List<Estafeta> getEstafetaAsList(){
        return estafetas.values()
                .stream()
                .map(Estafeta::clone)
                .collect(Collectors.toList());
    }

    /**
     *
     * @param localizacao Localização de estafetas
     * @param raio Raio máximo de estafetas
     * @return Lista de estafetas que se apresentam numa localização dentro de um certo raio
     */
    public List<String> estafetasPeriferia(GPS localizacao, double raio){
        return this.estafetas.values().stream()
                .filter(estafeta -> estafeta.getLocalizacao().dentroRaio(localizacao, raio))
                .map(Estafeta::getCodigo)
                .collect(Collectors.toList());
    }

}


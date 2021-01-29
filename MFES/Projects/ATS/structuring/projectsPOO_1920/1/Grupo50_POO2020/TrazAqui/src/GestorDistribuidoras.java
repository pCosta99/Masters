import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class GestorDistribuidoras implements Serializable{
    private Map<String, Distribuidora> distribuidoras;

    public GestorDistribuidoras(){
        this.distribuidoras = new HashMap<String, Distribuidora>();
    }

    public GestorDistribuidoras(Map<String, Distribuidora> distribuidoras){
        this.distribuidoras = distribuidoras;
    }

    public GestorDistribuidoras(GestorDistribuidoras umaBaseDados){
        this.distribuidoras = umaBaseDados.getDistribuidoras();
    }


    /**
     * Getters & Setters
     */

    /**
     * Obter uma copia do mapeamento das Distribuidoras
     * @return Map<String: código da Distribuidora, Distribuidora>
     */
    public Map<String, Distribuidora> getDistribuidoras(){
        return this.distribuidoras;
    }

    /**
     * @param novasDistribuidoras
     */
    public void setDistribuidoras(Map<String, Distribuidora> novasDistribuidoras){
        this.distribuidoras = novasDistribuidoras;
    }

    /**
     * Verificar a existência de uma encomenda dado o seu codigo
     * @param codigo da encomenda
     * @return True se a encomenda existir
     */
    public boolean existeDistribuidora(String codigo){
        return this.distribuidoras.containsKey(codigo);
    }

    /**
     *
     * @return Número de distribuidoras
     */
    public int numDistribuidoras(){
        return this.distribuidoras.size();
    }

    /**
     * Adiciona a informação de uma nova encomenda
     * @param e nova encomenda a inserir
     */
    public void addDistribuidora(Distribuidora e) throws EntidadeRepetidaException{
        if(this.distribuidoras.putIfAbsent(e.getCodigo(), e.clone()) != null)
            throw new EntidadeRepetidaException("Já existe uma distribuidora com o essa referência " + e.getCodigo() + " na App");
    }

    /**
     *
     * @param cod código da encomenda a procurar
     * @return cópia da encomenda encontrada
     * @throws EntidadeInexistenteException caso o código da encomenda não exista
     */
    public Distribuidora getDistribuidora(String cod) throws EntidadeInexistenteException{
        Distribuidora l;

        try{
            l = this.distribuidoras.get(cod);
        }
        catch(NullPointerException e){
            throw new EntidadeInexistenteException("Não existe objeto com nome " + cod);
        }
        return l;
    }

    public int tamanho(){
        return this.distribuidoras.size();
    }

    /**
     *
     * @return Lista das distribuidoras
     */
    public List<Distribuidora> getDistribuidoraAsList(){
        return distribuidoras.values()
                .stream()
                .map(Distribuidora::clone)
                .collect(Collectors.toList());
    }

    /**
     *
     * @param localizacao Localização de distribuidoras
     * @param raio Raio máximo de distribuidoras
     * @return Lista de distribuidoras que se apresentam numa localização dentro de um certo raio
     */
    public List<String> distribuidorasPeriferia(GPS localizacao, double raio){
        return this.distribuidoras.values().stream()
                .filter(distribuidora -> distribuidora.getLocalizacao().dentroRaio(localizacao, raio))
                .map(Distribuidora::getCodigo)
                .collect(Collectors.toList());
    }

}


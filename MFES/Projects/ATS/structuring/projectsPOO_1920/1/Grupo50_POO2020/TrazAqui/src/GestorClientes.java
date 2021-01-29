import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class GestorClientes implements Serializable{
    private Map<String, Cliente> clientes;

    public GestorClientes(){
        this.clientes = new HashMap<String, Cliente>();
    }

    public GestorClientes(Map<String, Cliente> clientes){
        this.clientes = clientes;
    }

    public GestorClientes(GestorClientes umaBaseDados){
        this.clientes = umaBaseDados.getClientes();
    }


    /**
     * Getters & Setters
     */

    /**
     * Obter uma copia do mapeamento das Clientes
     * @return Map<String: código da Cliente, Cliente>
     */
    public Map<String, Cliente> getClientes(){
        return this.clientes;
    }

    /**
     * @param novasClientes
     */
    public void setClientes(Map<String, Cliente> novasClientes){
        this.clientes = novasClientes;
    }

    /**
     * Verificar a existência de uma encomenda dado o seu codigo
     * @param codigo da encomenda
     * @return True se a encomenda existir
     */
    public boolean existeCliente(String codigo){
        return this.clientes.containsKey(codigo);
    }

    /**
     *
     * @return Número de clientes
     */
    public int numClientes(){
        return this.clientes.size();
    }

    /**
     * Adiciona a informação de uma nova encomenda
     * @param e nova encomenda a inserir
     */
    public void addCliente(Cliente e) throws EntidadeRepetidaException{
        if(this.clientes.putIfAbsent(e.getCodigo(), e.clone()) != null)
            throw new EntidadeRepetidaException("Já existe uma cliente com o essa referência " + e.getCodigo() + " na App");
    }

    /**
     *
     * @param cod código da encomenda a procurar
     * @return cópia da encomenda encontrada
     * @throws EntidadeInexistenteException caso o código da encomenda não exista
     */
    public Cliente getCliente(String cod) throws EntidadeInexistenteException{
        Cliente l;

        try{
            l = this.clientes.get(cod);
        }
        catch(NullPointerException e){
            throw new EntidadeInexistenteException("Não existe objeto com nome " + cod);
        }
        return l;
    }

    public int tamanho(){
        return this.clientes.size();
    }

    /**
     *
     * @return Lista das clientes
     */
    public List<Cliente> getClienteAsList(){
        return clientes.values()
                .stream()
                .map(Cliente::clone)
                .collect(Collectors.toList());
    }

    /**
     *
     * @param localizacao Localização de clientes
     * @param raio Raio máximo de clientes
     * @return Lista de clientes que se apresentam numa localização dentro de um certo raio
     */
    public List<Cliente> clientesPeriferia(GPS localizacao, double raio){
        return this.clientes.values().stream()
                .filter(cliente -> cliente.getLocalizacao().dentroRaio(localizacao, raio))
                .collect(Collectors.toList());
    }

}



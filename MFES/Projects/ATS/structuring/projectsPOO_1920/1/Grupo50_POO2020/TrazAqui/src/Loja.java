/**
 * Classe que implementa um Usuário para efeitos de log in
 *
 * @author monteiro06
 * @version 20200406
 */

import java.io.Serializable;
import java.net.PortUnreachableException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Loja extends Entidade implements Serializable {
    private List<Produto> inventario;
    private Map<String, Encomenda> pedidos;

    /**
     * Construtores da classe User
     * Declaração dos construtores por omissão (vazio), parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de User.
     */
    public Loja() {
        super();
        this.inventario = new ArrayList<>();
        this.pedidos = new HashMap<>();
    }

    /**
     * Construtor parametrizado de User
     */

    public Loja(String umCodigo, String umNome, GPS umaLocalizacao, List<Produto> umInventario, Map<String, Encomenda> unsPedidos){
        super(umCodigo, umNome, umaLocalizacao);
        this.inventario = umInventario;
        this.pedidos = unsPedidos;
    }

    public Loja(String umCodigo, String umNome, GPS umaLocalizacao){
        super(umCodigo, umNome, umaLocalizacao);
        this.inventario = new ArrayList<>();
        this.pedidos = new HashMap<>();
    }

    /**
     * Construtor de cópia de Loja.
     * Aceita como parâmetro outra Loja e utiliza os métodos de acesso aos valores das variáveis de instância.
     * @param umaLoja
     */
    public Loja(Loja umaLoja){
        super(umaLoja);
        this.inventario = umaLoja.getInventario();
        this.pedidos = umaLoja.getPedidos();
    }

    /**
     * Método de clone de loja
     * @return nova Loja
     */
    public Loja clone(){
        return new Loja(this);
    }

    /**
     * Métodos de instância
     */

    /**
     * Devolve o valor do nome do user
     * @return name
     */

    public List<Produto> getInventario() {
        return this.inventario;
    }

    public Map<String, Encomenda> getPedidos() {
        return this.pedidos;
    }

    public void setInventario(ArrayList<Produto> novoInventario) {
        this.inventario = novoInventario;
    }

    public void setPedidos(HashMap<String, Encomenda> novosPedidos) {
        this.pedidos = novosPedidos;
    }

    public boolean adicionaProduto(Produto umProduto){
        for(Produto p : inventario){
            if(p.getCodigo().equals(umProduto.getCodigo())){
                p.setQuantidade(p.getQuantidade() + umProduto.getQuantidade());
                return true;
            }
        }
        inventario.add(umProduto.clone());
        return false;
    }


    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || this.getClass() != o.getClass())
            return false;
        Loja l = (Loja) o;
        return super.equals(l) &&
                this.inventario.equals(l.getInventario()) &&
                this.pedidos.equals(l.getPedidos());
    }

    public String toString(){
        return  super.toString() +
                "\n\t Inventario: " + this.inventario +
                "\n\t Pedidos: " + this.pedidos +
                '\n' +'}';
    }

    public String toCSV(){
        return super.toCSV() + ",[" + this.inventario + "],[" + this.pedidos + '}';
    }

    public String toLog(){
        return super.toLog();
    }

    //ver quantas encomendasd tenho
    //ver quantas encomendas tenho por entregar
    //sinalizar uma encomenda para ser entregue

    public void adicionaPedido(Encomenda e){
        if(!this.pedidos.containsKey(e.getCodigo()))
            this.pedidos.put(e.getCodigo(), e.clone());
    }

    //sinaliza que existe uma encomenda de um utilizador para ser entregue
    //[EXCEPTION]
    public void terminaEncomenda(Encomenda e){
        if(e.getEstado() == 1)
            e.alteraEstado();
    }

    //numero de utilizadores
    public int quantosQueue(){
        return (int) this.pedidos.values().stream()
                .filter(e -> 1 == e.getEstado())
                .count();
    }

    public List<Encomenda> getEncomendasProntas(){
        return this.pedidos.values().stream()
                .filter(e -> 2 == e.getEstado())
                .collect(Collectors.toList());
    }



}

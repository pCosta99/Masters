import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Utilizador extends User implements Serializable {
    private List<Encomenda> encomendasJaPreparadas;     /*quando a loja prepara uma encomenda,
                                                        vai ser adicionada a esta lista para depois 
                                                        sabermos as que podemos solicitar a sua entrega*/
    
    private List<String> encomendasPorAceitarEntrega;   /* As encomendas que ja foram solicitadas e possivelmente
                                                        ja tem transportadoras interessadas em as transportar, mas o 
                                                        utilizador ainda nao aceitou a sua entrega(escolheu qual transportadora)
                                                        vai transportar a mesma */
    
    public Utilizador()
    {
        super();
        this.encomendasJaPreparadas = new ArrayList<>();
        this.encomendasPorAceitarEntrega = new ArrayList<>();
        
    }


    public Utilizador(String codUtilizador, String password, String nome, GPS coordenadas) 
    {
        super(codUtilizador,nome,password,coordenadas);
        this.encomendasJaPreparadas = new ArrayList<>();
        this.encomendasPorAceitarEntrega = new ArrayList<>();
    }


    public Utilizador (Utilizador u) 
    {
        super(u);
        
        this.encomendasJaPreparadas = u.getencomendasJaPreparadas();
        this.encomendasPorAceitarEntrega = new ArrayList<>();
    }

    public Utilizador (String codUtilizador, String nome, GPS coordenadas){
        super(codUtilizador, nome, "123",coordenadas); //password default
        this.encomendasJaPreparadas = new ArrayList<>();
        this.encomendasPorAceitarEntrega = new ArrayList<>();
    }
    
    public List<Encomenda> getencomendasJaPreparadas () {
        return this.encomendasJaPreparadas.stream().map(x -> x.clone()).collect(Collectors.toList());
    }


    public Utilizador clone (){

        return new Utilizador ( this );

    }

    /**
     * Apenas vamos ver se as credenciais do User sao as mesmas
    */

    public boolean equals(Object obj) {

        if (obj == this)  return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;

        Utilizador u = (Utilizador) obj;

        return  super.equals(u);
    }


    public String toString (){

        StringBuilder sb = new StringBuilder();

        sb.append("\nCodigo de Utilizador: ").append(super.getId());
        sb.append("\nNome: ").append(super.getNome());
        sb.append("\nCoordenadas: ").append(super.getCoordenadas().toString());
        sb.append("\nEncomendas Para Ser Entregue: ").append(this.encomendasJaPreparadas.toString());
        sb.append("\nEncomendas Por Aceitar: ").append(this.encomendasPorAceitarEntrega.toString());
        sb.append(super.toString());

        return sb.toString();
    }

    /**
     * Adicionar uma encomenda as encomendasJaPreparadas, metodo chamado apos
     * a loja a qual foi pedida a encomenda a preparou
     */

    public void adicionaEncomenda (Encomenda e) {
        encomendasJaPreparadas.add(e.clone());
    }

    /**
     * Remover uma encomenda das ja preparadas e dar return a esse objeto, 
     * procuramos a mesma a partir do seu codigo, metodo chamado quando
     * um utilizador solicita a sua entrega
     */

    public Encomenda pop (String idEnc){
        Encomenda ret = null;
        for (Encomenda e: this.encomendasJaPreparadas){
            if (e.getCodEncomenda().equals(idEnc)){
                ret = e.clone();
                this.encomendasJaPreparadas.remove(e);
                break;
            }
        }
        
        return ret;
    }

    /**
     * add para as encomendasPorAceitarEntrega
     */
    public void addEncomendaPorAceitar (String codEnc){
        this.encomendasPorAceitarEntrega.add(codEnc);
    }

    /**
     * remove para as encomendasPorAceitarEntrega
     */

    public void removeEncomendaPorAceitar (String codEnc){
        this.encomendasPorAceitarEntrega.remove(codEnc);
    }

    /**
     * imprimir a lista das encomendas por aceitar para o utilizador depois aceitar uma
     * pode dar uma exception de lista vazia
     */

    public String apresentarListaEncomendasPorAceitar () throws ListaVaziaException {
        if (this.encomendasPorAceitarEntrega.size() == 0) throw new ListaVaziaException("Lista Vazia");
        return this.encomendasPorAceitarEntrega.toString();
    }

    /**
     * Imprimir a lista das encomendas ja preparadas por uma loja
     */
    public String apresentarListaencomendasJaPreparadas (){
        return this.encomendasJaPreparadas.toString();
    }

}

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Lojas extends User implements Serializable{

    private List<Encomenda> emPreparacao;  //encomendas pedidas por um Utilizador, 
                                           //ainda nao preparadas por esta loja


    //Construtores
    public Lojas (Lojas l) {
        super(l);
        this.emPreparacao = new ArrayList<>();
    }

    public Lojas() {
        super();
        this.emPreparacao = new ArrayList<>();
    }

    public Lojas( String id, String nome, String password, GPS coordenadas) {
        super(id, nome , password,coordenadas);
        this.emPreparacao = new ArrayList<>();
    }

    public Lojas (String id, String nome, GPS coordenadas){
        super(id, nome, "123",coordenadas); //password default
        this.emPreparacao = new ArrayList<>();
    }


    //Getters e setters
    public List<Encomenda> getEmPreparacao (){
        return this.emPreparacao.stream().map(e -> e.clone()).collect(Collectors.toList());
    }

    public void setEmPreparacao (List<Encomenda> le) {
        this.emPreparacao = le.stream().map(e -> e.clone()).collect(Collectors.toList());
    }

    /**
     * Apenas comparamos as credenciais do super
     */
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Lojas)) {
            return false;
        }
        Lojas l = (Lojas) o;
        return super.equals(l);
    }

    public String toString (){

        StringBuilder sb = new StringBuilder();

        sb.append("\nCodigo da Loja: ").append(super.getId());
        sb.append("\nNome da Loja: ").append(super.getNome());
        sb.append("\nCoordenadas: ").append(super.getCoordenadas().toString());
        sb.append(super.toString());

        return sb.toString();
    }


    public Lojas clone () {
        return new Lojas(this);
    }




    /**
     * Adicionar nas encomendas para preparacao
     */
    public void addParaPreparacao (Encomenda e){
        emPreparacao.add(e.clone());
    }

    /**
     * @return total de encomendas que falta preparar
     */
    public int sizeParaPreparacao (){
        return emPreparacao.size();
    }

    
    /**
     * apos preparar uma encomenda removemos-la da lista pelo seu codigo
     */
    public Encomenda removeDasPreparadas (String codEnc){
        for (Encomenda e : emPreparacao){
            if(e.getCodEncomenda().equals(codEnc)){
                emPreparacao.remove(e);
                return e;
            }     
        }
    return null;
    }

    /**
     * calcular o total faturado a partir dos registos que estao no user
     */
    public double totalFaturado (){
        
        List<RegistoEncomenda> r = this.verRegistosGeral();
        double sum = 0;
        for (RegistoEncomenda atual : r){
            sum += atual.getEncomenda().valorTotalEncomenda();
        }

        return sum;
    }
    
    /**
     * ver as encomendas em preparacao mas vai dar return a uma lista de encomendas
     * pode dar uma exception caso a lista seja vazia
     */
    public List<Encomenda> verEncomendasEmPreparacao () throws ListaVaziaException {
        if (this.emPreparacao.size() == 0) {
            throw new ListaVaziaException("Lista Vazia");
        }
        
        return this.getEmPreparacao();}

}
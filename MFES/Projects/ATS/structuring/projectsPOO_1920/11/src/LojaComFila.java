import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;

/**
 * Classe que trata das Lojas com Fila de Espera
 * 
 * @author Rui Cunha
 * @version 13/04/2020
 */
public class LojaComFila extends Loja
{
    private List<Encomenda> fila_espera;//fila de espera
    private int tamanho;
    
    //Construtor vazio
    public LojaComFila()
    {
        super();
        this.fila_espera = new ArrayList<>();
        this.tamanho = 0;
    }
    
    //Construtor por parametros
    public LojaComFila(String username, String nome, String password,Localizacao local,
    HashMap<String,Encomenda> encomendas, HashMap<String,Integer> classificacao,List<Encomenda> fila_espera, int tamanho)
    {
        super(username,nome,password,local,encomendas,classificacao);
        this.setFila(fila_espera);
        this.setTamanho(tamanho);
    }
    
    //Construtor por copia
    public LojaComFila(LojaComFila loj)
    {
        super(loj);
        this.setFila(loj.getFila());
        this.setTamanho(loj.getTamanho());
    }
    
    //Getters
    public List<Encomenda> getFila()
    {
        List<Encomenda> res = new ArrayList<>();
        this.fila_espera.stream()
            .forEach(e -> res.add(e.clone()));
        return res;
    }

    public int getTamanho() {
        return this.tamanho;
    }

    //Setters
    public void setFila(List<Encomenda> fil)
    {
        this.fila_espera = new ArrayList<>();
        for(Encomenda e : fil)
        {
            this.fila_espera.add(e.clone());
        }
    }

    public void setTamanho(int tamanho) {
        this.tamanho = tamanho;
    }

    public void addEncFila(Encomenda e){
        this.fila_espera.add(e.clone());
    }

    public void retiraEncFila(){
        this.fila_espera.remove(0);
    }

    public String retiraEncFilaCod(){
        String e = this.fila_espera.get(0).getCodigo();
        this.fila_espera.remove(0);
        return e;
    }

    //Metodo toString
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
        .append("Fila de Espera: ").append(this.fila_espera.toString() + "\n")
        .append("Tamanho da fila: ").append(this.tamanho + "\n");
        return sb.toString();
    }
    
    //Metodo Equals
    public boolean equals(Object o)
    {
        if(this == o)
            return true;
        if((o==null) || (this.getClass() != o.getClass()))
            return false;
            
        LojaComFila p = (LojaComFila) o;
        return(super.equals(p) &&
            this.fila_espera.equals(p.getFila())
            && this.tamanho == p.getTamanho());
    }
    
    //Clone
    public LojaComFila clone()
    {
        return new LojaComFila(this);
    }
}

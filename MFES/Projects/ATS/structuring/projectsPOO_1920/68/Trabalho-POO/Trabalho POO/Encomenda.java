import java.io.Serializable;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;


public class Encomenda implements Serializable, Comparable<Encomenda> {
    private static Set<Integer> codigos = new HashSet<>(); // <--- ISTO É ASSIM?

    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private float peso;
    
    private Set<LinhaEncomenda> encomenda; // falta fazer hashCode()
    private boolean med;

    private boolean aceite;


    /**
     * Método de classe. 
     * Cria um novo código único, identificador de uma encomenda.
    */
    public static String novoCodigo() {
        Random random = new Random();
        int valor = random.ints(0, 9999).findFirst().getAsInt();
        while(Encomenda.codigos.contains(valor)) {
            valor = random.ints(0, 9999).findFirst().getAsInt();
        }
        Encomenda.codigos.add(valor);

        return "e"+String.valueOf(valor); // <--- VERIFICAR SE FUNCIONA
    }

    /**
     * Método de classe. 
     * Elimina o registo de um código de encomenda.
     * Usa-se quando uma encomenda acaba de ser tratada.
    */
    public static void removeCodigo(String cod) {
        String substring = cod.substring(1);
        int valor = Integer.parseInt(substring);

        Encomenda.codigos.remove(valor);
    }


    /**
     * Construtor por omissao de objetos da classe LinhaEncomenda
     */
    public Encomenda()
    {
       this.codEncomenda = "";
       this.codUtilizador = "";
       this.codLoja = "";
       this.peso = 0;
       this.encomenda = new HashSet<>();
       this.aceite = false;
    }

    /**
     * Construtor parametrizado de objetos da classe LinhaEncomenda
     */
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, float peso, Set<LinhaEncomenda> encomenda)
    {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        setEncomenda(encomenda);
        this.aceite = false;
    }

    /**
     * construtor de copia de objetos da classe LinhaEncomenda
     */
    public Encomenda(Encomenda enc)
    {
        this.codEncomenda = enc.getCodEnc();
        this.codUtilizador = enc.getCodUtilizador();
        this.codLoja = enc.getCodLoja();
        this.peso = enc.getPeso();
        this.encomenda = enc.getEncomenda();
        this.med = enc.isMed();
        this.aceite = enc.estaAceite();
    }

    /**
     * Construtor da classe Encomenda usado quando o cliente faz um pedido.
     * @param encomendas
     * @param user
     * @param loja
     */
    public Encomenda(Set<LinhaEncomenda> encomendas, String user, String loja) {
        this.codEncomenda = Encomenda.novoCodigo();
        this.codUtilizador = user;
        this.codLoja = loja;
        setEncomenda(encomendas);
        this.peso = (float) this.encomenda.stream().mapToDouble(e -> e.getQuantidade()).sum();
        this.aceite = false;
    }

    ////////////////////////////// GETTERS E SETTERS ////////////////////////////////////////

    public String getCodEnc()
    {
        return this.codEncomenda;
    }

    public String getCodUtilizador()
    {
        return this.codUtilizador;
    }

    public String getCodLoja()
    {
        return this.codLoja;
    }

    public float getPeso()
    {
        return this.peso;
    }

    public Set<LinhaEncomenda> getEncomenda()
    {
        return this.encomenda.stream().map(LinhaEncomenda::clone).collect(Collectors.toSet());
    }

    public boolean isMed() {
        return med;
    }

    public void setMed(boolean med) {
        this.med = med;
    }

    public void setCodEncomenda(String novo_codEnc)
    {
        this.codEncomenda = novo_codEnc;
    }

    public void setCodUtilizador(String novo_codUtilizador)
    {
        this.codUtilizador = novo_codUtilizador;
    }

    public void setPeso(float peso){
        this.peso=peso;
    }

    public void setCodLoja(String novo_codLoja)
    {
        this.codLoja = novo_codLoja;
    }

    public void setEncomenda(Set<LinhaEncomenda> nova_encomenda)
    {
        this.encomenda = nova_encomenda.stream().map(LinhaEncomenda::clone).collect(Collectors.toSet());
    }

    ///////////////////////////////////// Outros métodos //////////////////////////////////////////

    public double pesoProdutos(){
        return  this.encomenda.stream().map(LinhaEncomenda::getQuantidade).reduce(0.0,(ac, x)->ac+x);
    }

    public void addLinhaEncomenda(LinhaEncomenda linha){
        this.encomenda.add(linha);
    }


    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("CodEncomenda: ").append(codEncomenda).append("\n");
        sb.append("CodUtilizador: ").append(codUtilizador).append("\n");
        sb.append("CodLoja: ").append(codLoja).append("\n");
        sb.append("Peso: ").append(peso).append("\n");
        sb.append("Encomenda:\n").append(this.encomenda.toString()).append("\n");

        return sb.toString();
    }

    public Encomenda clone()
    {
        Encomenda clone = new Encomenda(this);
        clone.foiAceite();
        return clone;
    }

    public boolean equals(Object o)
    {
        if (o==this) return true;
        if (o==null || (o.getClass().equals(this.getClass())) == false) return false;
        Encomenda enc = (Encomenda)o;

        return enc.getCodEnc().equals(this.codEncomenda) && 
               enc.getCodUtilizador().equals(this.codUtilizador) &&
               enc.getCodLoja().equals(this.codLoja) && 
               enc.getPeso() == this.peso &&
               enc.getEncomenda().equals(this.encomenda);
    }

    public int compareTo(Encomenda enc) {
        return this.codEncomenda.compareTo(enc.getCodEnc());
    }

    public boolean estaAceite() {
        return this.aceite;
    }

    public void foiAceite() {
        this.aceite = true;
    }
}

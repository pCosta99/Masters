import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Escreva a descrição da classe Encomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Encomenda
{
    private String id;
    private String idComprador;
    private String idLoja;
    private float peso;
    private String produto;
    private boolean encMedica; //é ou não uma encomenda médica
    private boolean aceite; //aceite pelo transportador
    private List<LinhaEncomenda> linhas; //uma encomenda tem várias linhas de encomenda, uma para cada produto da encomenda
    private String idTransportador;
    private boolean encPronta;
    private boolean aceComprador;
    private LocalDate data; // data em que a encomenda é aceite


    //construtor vazio

    public Encomenda(){
    this.id= "";
    this.idComprador = "";
    this.idLoja = "";
    this.peso = 0;
    this.produto = "";
    this.encMedica = false;
    this.aceite = false;
    this.linhas = new ArrayList<>();
    this.idTransportador = "";
    this.encPronta = false;
    this.aceComprador = false;
    this.data = LocalDate.now();
    }

    //construtor parametrizado

    public Encomenda (String id, String idComprador, String idLoja, float peso){
        this.id = id;
        this.idComprador = idComprador;
        this.idLoja = idLoja;
        this.peso = peso;
        this.produto = "";
        this.encMedica = false;
        this.aceite = false;
        this.linhas = new ArrayList<LinhaEncomenda>();
        this.idTransportador = "";
        this.encPronta = false;
        this.aceComprador = false;
        this.data = LocalDate.now();
    }

    //construtor parametrizado
    
    public Encomenda (String id, String idComprador, String idLoja, String idL, float peso, String produto, boolean encMedica, boolean ace, ArrayList<LinhaEncomenda> l, String i, boolean pronta, boolean acee, LocalDate data){
    this.id = id;
    this.idComprador = idComprador;
    this.idLoja = idLoja;
    this.peso = peso;
    this.produto = produto;
    this.encMedica = encMedica;
    this.aceite = ace;
    this.linhas = l;
    this.idTransportador = i;
    this.encPronta = pronta;
    this.aceComprador = acee;
    this.data = data;
    }

    //construtor de cópia
    
    public Encomenda (Encomenda e){
    this.id = e.getId();
    this.idComprador = e.getIdComprador();
    this.idLoja = e.getIdLoja();
    this.peso = e.getPeso();
    this.produto = e.getProduto();
    this.encMedica = e.getEncMedica();
    this.aceite= e.getAce();
    this.linhas = e.getLinhas();
    this.idTransportador = e.getIdTransportador();
    this.encPronta = e.getEncPronta();
    this.aceComprador = e.getaceC();
    this.data = getData();
    }

    //metodo que devolve o ID da encomenda em causa
    
    public String getId(){
       return this.id;
    }
    
    //metodo que devolve o ID do comprador associado à encomenda em causa

    public String getIdComprador(){
       return this.idComprador;
    }
    
    //metodo que devolve o ID da loja associada à encomenda em causa

    public String getIdLoja(){
       return this.idLoja;
    }

    //metodo que devolve o peso da encomenda em causa
    
    public float getPeso(){
        return this.peso;
    }
    
    //metodo que devolve o produto em causa, neste caso, uma breve descrição do produto

    public String getProduto(){
        return this.produto;
    }
    
    //metodo que verifica se a encomenda em causa é médica ou normal

    public boolean getEncMedica(){
        return this.encMedica;
    }

    //metodo que devolve 0 se a encomenda ainda não foi aceite para transporte e 1 se esta foi aceite
    
    public boolean getAce(){
        return this.aceite;
    }

    public ArrayList<LinhaEncomenda> getLinhas(){
        ArrayList<LinhaEncomenda> l = new ArrayList<LinhaEncomenda>();
        l.addAll(this.linhas);
        return l;
    }

    public String getIdTransportador() {return  this.idTransportador;}

    public boolean getaceC() {return this.aceComprador;}

    public boolean getEncPronta(){return this.encPronta;}

    public LocalDate getData(){ return this.data;}
    
    //metodo que dá set ao ID da encomenda em causa

    public void setId(String id){
    this.id = id;
    }
    
    //metodo que dá set ao ID do comprador associado à encomenda em causa

    public void setIdComprador(String idC){
    this.idComprador = idC;
    }
    
    //metodo que dá set ao ID da loja associada à encomenda em causa

    public void setIdLoja(String idL){
    this.idLoja = idL;
    }
    
    //metodo que dá set ao peso da encomenda em causa

    public void setPeso(float peso){
    this.peso = peso;
    }
    
    //metodo que dá set à descrição da encomenda em causa

    public void setProduto(String produto){
    this.produto = produto;
    }
    
    //metodo que dá set da encomenda em causa, em termos de ser médica ou nao

    public void setEncMedica(boolean enc){
    this.encMedica = enc;
    }

    //metodo que dá set da aceitação de transporte da encomenda em causa
    
    public void setAce(boolean a){
    this.aceite = a;
    }

    public void setEncPronta(boolean a){
        this.encPronta = a;
    }

    public void setLinhas(ArrayList<LinhaEncomenda> le){
        this.linhas = new ArrayList<LinhaEncomenda>(le.size());
        for(LinhaEncomenda e : le){
            this.linhas.add(e.clone());
        }
    }

    public void setIdTransportador(String i){ this.idTransportador = i;}

    public void setAceComprador(boolean a){this.aceComprador = a;}

    public void setData(LocalDate data){this.data = data;}
    
    //metodo que verifica se 2 objetos são iguais

    public boolean equals (Object o){
       if (o == this) return true;
       if (o == null || o.getClass() != this.getClass()) return false;
       Encomenda e = (Encomenda) o;
       return (this.id.equals(e.getId()) &&
               this.idComprador.equals(e.getIdComprador()) &&
               this.idLoja.equals(e.getIdLoja()) &&
               this.linhas.equals(e.getLinhas()) &&
               this.peso == e.getPeso() &&
               this.produto.equals(e.getProduto()) &&
               this.encMedica == e.getEncMedica() &&
               this.aceite == e.getAce() &&
               this.idTransportador.equals(e.getIdTransportador()) &&
               this.encPronta == (e.getEncPronta()) &&
               this.aceComprador == (e.getaceC()) &&
               this.data.equals(e.getData()));
    }
    
    //metodo que devolve a classe numa string

    public String toString() {
       StringBuilder s = new StringBuilder();
       s.append("Encomenda: ") .append(this.id)
                               .append("\n")
                               .append(this.idComprador)
                               .append("\n")
                               .append(this.idLoja)
                               .append("\n")
                               .append(this.peso)
                               .append("\n")
                               .append(this.produto)
                               .append("\n")
                               .append(this.encMedica)
                               .append("\n")
                               .append(this.aceite)
                               .append("\n")
                               .append(this.idTransportador)
                               .append("\n")
                               .append(this.encPronta)
                               .append("\n")
                               .append(this.aceComprador)
                               .append("\n")
                               .append(this.data);


       return s.toString();
    }
    
    //metodo de cria um clone da classe

    public Encomenda clone(){
    return new Encomenda(this);
    }

    public void addLinhaEncomenda(LinhaEncomenda e){
        linhas.add(e);
    }




}

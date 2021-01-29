import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.io.Serializable;
public class Navegador implements INavegador, Serializable{
    private List<String> elems;
    private int pos;
    private int tam;
    private int page;

    /*
    Construtor por omissão de Navegador
    */
    public Navegador(){
        this.elems = new ArrayList<>();
        this.pos = 0;
        this.tam = 0;
        this.page = 1;
    }

    /*
    Construtor parameterizado de Navegador
    Aceita como parametros uma lista de string, correspondente ao que queremos imprimir,
    e o tamanho de cada pagina.
    */
    public Navegador(List<String> list, int tam){
        this.elems = list;
        this.pos = 0;
        this.tam = tam;
        this.page = 1;
    }

    /*
     *Avança a pagina. 
     *@return um boolean com true se poder avançar a pagina e false se nao for possível.
     */
    public  boolean nextPage(){
        if(this.page >= getNumberPages()) return false;
        //for(int i = 0; this.pos < this.elems.size() && i < this.tam; i++, this.pos++);
        this.pos += this.tam;
        if(this.pos >= this.elems.size()) this.pos = this.elems.size();
        this.page++;
        return true;
    }
    
    /*
     *Avança para a página anterior.
     *@return um boolean a indicar se pode retroceder.
    */
    public boolean previousPage(){
        if(pos == 0) return false;
        //for(int i = this.tam; this.pos > 0 && i > 0; i--, this.pos--);
        this.pos -= this.tam;
        //if(this.pos < 0) this.pos = 0
        this.page--;
        return true;
    }

    /*
     *Avança para a última página.
     *@return boolean a indicar se é possivel avançar para a ultima página.
    */
    public boolean lastPage(){
        boolean ret = jumpToPage(getNumberPages()-1);
        this.page = getNumberPages();
        return ret;
    }

    /*
     *Avança para a primeira página.
     *@return boolean a indicar se é possivel avançar para a primeira página.
    */
    public boolean firstPage(){
        this.pos = 0;
        this.page = 1;
        return true;
    }

    /*
     *Avança para a página indicada no index.
     *@param index indice da pagina que queremos aceder.
     *@return boolean a indicar se é possivel aceder essa página.
    */
    public boolean jumpToPage(int index){
        if(getNumberPages() <= index) return false;
        this.pos = index * this.tam;
        this.page = index + 1;
        return true;
    }
    /** 
     * devolve o numero de paginas
     * @return int representa o numero de paginas 
     */
    public int getNumberPages(){
        int ret = this.elems.size() / tam;
        if (this.elems.size() % tam != 0) ret++;
        return ret;
    }
    /**
     * Devolve o int page da classe
     * @return int page da classe
     */
    public int getPageNumber(){
        return this.page;
    }
    /**
     * Devolve a List<String> elems da classe
     * @return List<String> da classe
     */	
    public List<String> getPage(){
        List<String> ret = new ArrayList<>();
        for(int i = pos; i < this.elems.size() && (i-pos) < this.tam; i++){
            ret.add(this.elems.get(i));
        }
        return ret;
    }
    /**
     * Metodo que imprime o menu da pagina do Navegador
     * @return String correspondente ao menu
     */
    public String showPage(){
        Iterator<String> p = this.elems.iterator();
        StringBuilder sb = new StringBuilder();
        List<String> page = this.getPage();
        sb.append("+----------------------------------------------------+\n");
        sb.append("|                Página "+this.getPageNumber()+" / "+this.getNumberPages()+"                        |\n");
        sb.append("+----------------------------------------------------+");
        for(String s : page)
            sb.append("\n").append(s);
        sb.append("\n+----------------------------------------------------+\n");
        sb.append("| d -> próxima página        u-> Última página       |\n");
        sb.append("| a -> página anterior       p-> Primeira página     |\n");
        sb.append("| j -> saltar para a página  s -> sair               |\n");
        sb.append("+----------------------------------------------------+");
        return sb.toString();
    }
}

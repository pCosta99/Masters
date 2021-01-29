import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class Listagem<C> {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    private List<C> classe;
    private int idPagina;
    private int inicio;
    private int fim;
    private int tamanho;
    private int ultimaPagina;
    private int elementos = 5;


    /**
     * CONSTRUTOR PARAMETRIZADO
     */

    public Listagem(Collection<C> lista) {

        if(lista == null) {
            return;
        }

        this.classe = new ArrayList<>(lista);
        this.tamanho = lista.size();
        this.idPagina = this.tamanho == 0 ? 0: 1;
        this.inicio = 0;
        this.fim = this.elementos;
        this.ultimaPagina = (int) Math.ceil((double) this.tamanho / this.elementos);
    }

    /**
     * MÉTODO MOSTRAR LISTA
     */

    public void mostrarLista(View view) {

        if(this.classe == null) {
            return;
        }

        for(int i = this.inicio; i < this.fim && i < this.tamanho; i++) {
            view.imprimeLinhaCM(String.format("%d: ", i + 1));
            view.imprimeLinhaCM(this.classe.get(i).toString());
        }
    }

    /**
     * MÉTODO PASSAR PARA A PRÓXIMA PÁGINA
     */

    public Listagem proximaPagina() {
        int elementosPorPagina = this.elementos;

        if(this.classe == null) {
            return null;
        }

        if(this.fim > this.tamanho) {
            this.idPagina = 1;
            this.inicio = 0;
            this.fim = elementosPorPagina;
        } else {
            this.idPagina++;
            this.inicio = this.fim;
            this.fim += elementosPorPagina;
        }

        return this;
    }

    /**
     * MÉTODO PASSAR PARA A PÁGINA ANTERIOR
     */

    public Listagem retrocedePagina() {
        int elementosPorPagina = this.elementos;

        if(this.classe == null) {
            return null;
        }

        if(this.inicio < elementosPorPagina) {
            this.idPagina = (this.tamanho / elementosPorPagina) + 1;
            this.fim = this.idPagina * elementosPorPagina;
            this.inicio = this.fim - elementosPorPagina;
        } else {
            this.idPagina--;
            this.fim = this.inicio;
            this.inicio -= elementosPorPagina;
        }

        return this;
    }

    public Listagem atualizaClasse(Collection<C> lista) {

        if(lista == null) {
            return null;
        }

        this.classe = new ArrayList<>(lista);
        this.tamanho = lista.size();
        this.idPagina = this.tamanho == 0 ? 0: 1;
        this.inicio = 0;
        this.fim = this.elementos;
        this.ultimaPagina = (int) Math.ceil((double) this.tamanho / this.elementos);
        return this;
    }
}

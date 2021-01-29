import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Classe que auxilia a interação com o utilizador.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class Navegador {
    private static final int sizePage = 10;
    private final List<String> list;
    private final int numValues;
    private int page;
    private final int maxPage;

    /**
     * Construtor de um Navegador.
     * @param colecao, Coleção do Navegador a construir.
     */
    public Navegador(Collection<?> colecao){
        this.list = colecao.stream()
                           .map(Object::toString)
                           .collect(Collectors.toList());
        this.page = 0;
        this.numValues = this.list.size();
        this.maxPage = (numValues-1)/Navegador.sizePage;
    }

    /**
     * Construtor de um Navegador.
     * @param colecao,  Coleção do Navegador a construir.
     * @param before1, Mensagem do Navegador a construir.
     * @param before2, Mensagem do Navegador a construir.
     */
    public Navegador(Map<?,?> colecao, String before1, String before2){
        this.list = colecao.entrySet()
                           .stream()
                           .map(e -> String.format("%s%s%s%s",before1,e.getKey(),before2,e.getValue()))
                           .collect(Collectors.toList());
        this.page = 0;
        this.numValues = this.list.size();
        this.maxPage = (numValues-1)/Navegador.sizePage;
    }

    /**
     * Função que permite avançar/retroceder nas páginas.
     * @param offset, Offset que permite avançar/retroceder.
     */
    private void skipPages(int offset){
        this.page += offset;
        if(this.page < 0)            this.page = 0;
        if(this.page > this.maxPage) this.page = this.maxPage;
    }

    /**
     * Função que apresenta e permite ao utilizador escollher algumas opções.
     */
    public void show(){
        int escolha;
        do {
            System.out.printf("Página %d/%d\n\n",this.page+1,this.maxPage+1);
            for (int i = this.page * sizePage, c = 0; i < this.list.size() && c < Navegador.sizePage; i++, c++)
                System.out.println(this.list.get(i));
            System.out.println();
            this.showOptions();
            escolha = Input.lerInt("Escolha: ","Escreva um Inteiro!",0,2);
            switch(escolha){
                case 0:
                    System.out.println("Regressando ao menu anterior!");
                    break;
                case 1:
                    skipPages(1);
                    break;
                case 2:
                    skipPages(-1);
                    break;
            }
        }while (escolha != 0);
    }

    /**
     * Função que imprime as opções disponíveis.
     */
    private void showOptions(){
        System.out.println("1 - Avançar uma página.");
        System.out.println("2 - Recuar uma página.");
        System.out.println("0 - Sair do Menu.");
    }
}
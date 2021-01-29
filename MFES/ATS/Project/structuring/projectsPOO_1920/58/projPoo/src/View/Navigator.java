package View;

import java.util.ArrayList;
import java.util.List;


public class Navigator {
    //variável de instância que indica a posição onde me situo.
    private int posicao;
    //variável de instância que indica o número de strings que irei imprimir.
    private int numStrings;
    //variável de instância que imprime as strings.
    private List<String> nav;

    /**
     * Construtor por omissão
     */
    public Navigator() {
        this.posicao = 0;
        this.numStrings = 0;
        this.nav = new ArrayList<>();
    }

    /**
     * Construtor parametrizado
     * @param posicao Inteiro que representa a posição.
     * @param numStrings Inteiro que indica o número de Strings que irei imprimir.
     * @param nav Lista de Strings a imprimir.
     */
    public Navigator(int posicao, int numStrings, List<String> nav) {
        this.posicao = posicao;
        this.numStrings = numStrings;
        this.nav = new ArrayList<>();
        for (String n : nav)
            this.nav.add(n);
    }

    /**
     * Construtor por cópia.
     * @param oneNav Objeto da classe Navigator.
     */
    public Navigator(Navigator oneNav) {
        this.posicao = oneNav.getPosicao();
        this.numStrings = oneNav.getNumStrings();
        this.nav = oneNav.getNav();

    }

    /**
     * Método que dá o valor da posição.
     * @return Devolve essa posição.
     */
    public int getPosicao() {
        return this.posicao;
    }

    /**
     * Método que dá valor do número de Strings a imprimir.
     * @return Devolve esse valor.
     */
    public int getNumStrings() {
        return this.numStrings;
    }

    /**
     * Método que dá a lista de Strings a imprimir.
     * @return Devolve a lista de Strings.
     */
    public List<String> getNav() {
        List<String> ret = new ArrayList<>();
        for(String n : ret)
            ret.add(n);
        return ret;
    }

    /*
    public void setNumStrings(int numStrings) {
        this.numStrings = numStrings;
    }
     */

    /**
     * Método que define o valor da posição.
     * @param posicao
     */
    public void setPosicao(int posicao) {
        this.posicao = posicao;
    }

    /**
     * Método que define o valor da lista de Strings a imprimir.
     * @param nav Recebe uma lista de Strings.
     */
    public void setNav(List<String> nav) {
        this.nav = nav;
    }

    /**
     * Função que traduz a classe Navigator.
     * @return Devolve a String com a respetiva a tradução.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Posição;").append(this.posicao)
                .append("Número de Strings imprimir:").append(this.numStrings)
                .append("Lista de Strings a imprimir").append(this.nav);
        return sb.toString();
    }

    /**
     * Função que verifica se um objeto recebido é idêntico ao Navigator da classe.
     * @param o Recebe um objeto.
     * @return Devolve um boolean que corresponde a tal verificação.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Navigator nav = (Navigator) o;
        return this.posicao == nav.posicao &&
                this.numStrings == nav.numStrings &&
                this.nav.equals(nav);
    }

    /**
     * Função que cria um clone do Navigator.
     * @return Devolve esse clone.
     */
    @Override
    public Navigator clone() {
        return new Navigator(this);
    }

    /**
     * Função que define o número de páginas a imprimir.
     * @param numero Recebe o número.
     */
    public void tamPages(int numero) {
        this.numStrings = numero;
    }

    /**
     * Função que fornece a página inicial.
     * @return Devolve uma lista de Strings que corresponde à página inicial.
     */
    public List<String> InitialPage() {
        List<String> ret = new ArrayList<>();
        this.posicao = 0;
        int i;
        for(i = 0; i < this.numStrings; i++) {
            String s;
            s = nav.get(i);
            ret.add(s);
        }
        return ret;
    }

    /**
     * Função que fornece a mesma página.
     * @return Devolve a lista de Strings que correponde à mesma página.
     */
    public List<String> SamePage() {
        List<String> ret = new ArrayList<>();
        this.posicao = this.posicao - this.numStrings;
        int i;
        for(i = this.posicao; i < this.posicao + this.numStrings && i < this.nav.size(); i++) {
            String s;
            s = nav.get(i);
            ret.add(s);
        }
        this.posicao += this.numStrings;
        return ret;
    }

    /**
     * Função que fornece página seguinte.
     * @return Devolve a próxima página.
     */
    public List<String> NextPage() {
        List<String> ret = new ArrayList<>();
        if (this.posicao < this.nav.size()) {
            int i;
            for (i = this.posicao; i < this.posicao + this.numStrings && i < this.nav.size(); i++) {
                String s;
                s = nav.get(i);
                ret.add(s);
            }
            this.posicao += this.numStrings;
            return ret;
            //caso a posiçao onde estou seja igual ao tamanho da lista de strings,
            //não posso andar para as proximas paginas, pois estas não existem,
            //logo, nesse caso, devolvemos a mesma pagina.
        } else {
            //se a posição for maior ou igual que o tamanho da lista, dá se retorno da mesma página.
            ret = SamePage();
            return ret;
        }
    }

    /**
     * Função que permite ir para a página anterior.
     * @return Devolve uma lista de Strings que corresponde à página respetiva.
     */
    public List<String> BackPage() {
        List<String> ret = new ArrayList();
        if(this.posicao > this.numStrings) {
            this.posicao = this.posicao - 2 * this.numStrings;
            int i;
            for (i = this.posicao; i < this.posicao + this.numStrings && i < this.nav.size(); i++) {
                String s;
                s = nav.get(i);
                ret.add(s);
            }
            this.posicao += this.numStrings;
            return ret;
        } else {
            this.posicao = 0;
            ret = NextPage();
            return ret;
        }
    }

    /**
     * Função que fornece a página desejada.
     * @param numero Inteiro que representa o número da página a escolher.
     * @return
     */
    public List<String> ChoosePage(int numero) {
        List<String> ret = new ArrayList<>();
        if (this.numStrings * (numero-1)  < this.nav.size()) {
            this.posicao = numStrings * (numero-1);
            int i;
            for (i = this.posicao; i < this.posicao + this.numStrings && i < this.nav.size(); i++) {
                String s;
                s = nav.get(i);
                ret.add(s);
            }
            this.posicao += this.numStrings;
        }
        return ret;
    }

    /**
     * Função dada uma String devolve a página onde a mesma se encontra.
     * @param s Recebe a String
     * @return Devolve a página onde está inserida essa String.
     */
    //Função dada uma String devolve a página onde a mesma se encontra.
    public List<String> WhereIs(String s) {
        List<String> ret = new ArrayList<>();
        int i;
        for (i = 0; i < this.nav.size(); i++) {
            if (s.equals(nav.get(i))) {
                ret = ChoosePage((i / this.numStrings) + 1);
                return ret;
            }
        }
        ret = ChoosePage((i / this.numStrings) + 1);
        return ret;
    }

}

package View;

import Helpers.IPair;
import Helpers.Pair;

import java.util.ArrayList;
import java.util.List;

public class Navigator{
    private int posicao;
    private int numStrings;
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
     * @param posicao Recebe um inteiro que representa a posição
     * @param numStrings Recebe um inteiro que representa o número de strings a imprimir
     * @param lines Recebe a Lista de strings a imprimir
     */
    public Navigator(int posicao, int numStrings, List<String> lines) {
        this.posicao = posicao;
        this.numStrings = numStrings;
        this.nav = new ArrayList<>(lines);
    }

    public int getSize() {
        return this.nav.size();
    }

    /**
     * Construtor por cópia
     * @param oneNav Recebe um objeto da classe
     */
    public Navigator(Navigator oneNav) {
        this.posicao = oneNav.getPosicao();
        this.numStrings = oneNav.getNumStrings();
        this.nav = oneNav.getNav();
    }

    /**
     * Função que dá a posição onde me encontro
     * @return Devolve essa posição
     */
    public int getPosicao() {
        return posicao;
    }

    /**
     * Função que dá o número de strings a imprimir
     * @return Devolve esse número
     */
    public int getNumStrings() {
        return numStrings;
    }

    /**
     * Função que dá a lista de strings a imprimir
     * @return Devolve essa lista
     */
    public List<String> getNav() {
        List<String> ret = new ArrayList<>();
        for (String n : ret)
            ret.add(n);
        return ret;
    }

    /**
     * Função que altera a posição onde me encontro
     * @param posicao Devolve essa posição
     */
    public void setPosicao(int posicao) {
        this.posicao = posicao;
    }

    /**
     * Função que altera o número de Strings a imprimir
     * @param numStrings Devolve esse número
     */
    public void setNumStrings(int numStrings) {
        this.numStrings = numStrings;
    }

    /**
     * Função que altera a lista de strings a imprimir
     * @param nav Devolve essa mesma lista
     */
    public void setNav(List<String> nav) {
        this.nav = nav;
    }

    /**
     * Função que traduz a classe Navigator
     * @return Devolve a tradução
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Posicao:").append(this.posicao)
                .append("\nNumero de Strings").append(this.numStrings)
                .append("\nNavegador").append(this.nav);
        return sb.toString();
    }

    /**
     * Função que verifica se um objeto recebido é idêntico ao Navigator da classe
     * @param o Recebe um objeto
     * @return Boolean que representa a verificação
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Navigator oneNav = (Navigator) o;
        return posicao == oneNav.posicao &&
                numStrings == oneNav.numStrings &&
                this.nav.equals(oneNav.getNav());
    }

    /**
     * Função que cria um clone do Navigator
     * @return Devolve a cópia
     */
    @Override
    public Navigator clone() {
        return new Navigator(this);
    }

    /**
     * Função que define o tamanho de uma página
     * @param numero Recbe um inteiro que representa o tamanho
     */
    public void tamPages(int numero) {
        this.numStrings = numero;
    }

    /**
     * Função que permite ir para a página inicial
     * @return Lista de Strings que representa a página inicial
     */
    public List<String> InitialPage() {
        List<String> ret = new ArrayList<>();
        this.posicao = 0;
        int i;
        for (i = 0; i < this.numStrings; i++) {
            String s;
            //guardo na string o elemento da lista nav
            s = nav.get(i);
            //estou a adicionar na nova lista que crie, os elementos que estao na lista nav
            ret.add(s);
        }
        return ret;
    }

    /**
     * Função que fornece a mesma página
     * @return Lista de Strings representante dessa página
     */
    public List<String> samePage() {
        List<String> ret = new ArrayList<>();
        int i;
        this.posicao = (this.posicao) - (this.numStrings);
        for (i = this.posicao; i < this.posicao + this.numStrings && i < this.nav.size(); i++) {
            String s = nav.get(i);
            ret.add(s);
        }
        this.posicao += this.numStrings;
        return ret;
    }

    /**
     * Função que permite ir para a próxima página ou as strings seguintes
     * @return Lista de Strings que representa essa página
     */
    public List<String> nextPage() {
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
            ret = samePage();
            return ret;
        }
    }

    /**
     * Função que dá a pagina anterior ou as strings anteriores
     * @return Lista de Strings representante da página
     */
    public List<String> backPage() {
        List<String> ret = new ArrayList<>();
        if (this.posicao > this.numStrings) {
            //tenho um arrayList de strings de 1 a 6, se estou na ultima pagina
            //ou seja, se imprimi de 3 a 6, e quero voltar para a pagina anterior
            //tenho de subtrair uma vez por 3 e outra vez, so assim conseguirei ir
            //para o inicio da pagina, para depois a imprimir
            this.posicao = ((this.posicao) - 2 * (this.numStrings));
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
            ret = nextPage();
            return ret;
        }
    }

    /**
     * Função que fornece a página desejada
     * @param numero Inteiro que simboliza o número da página desejada
     * @return Lista de Strings que retrata a tal página
     */
    public List<String> choosePage(int numero) {
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
     * //Funçao que dada uma string, fornece a lista onde a mesma se encontra
     * @param s String desejada
     * @return Lista de String simbolizante da página que contém a String
     */
    public List<String> whereIs(String s) {
        List<String> ret = new ArrayList<>();
        int i;
        for (i = 0; i < this.nav.size(); i++) {
            if (s.equals(nav.get(i))) {
                ret = choosePage((i / this.numStrings) + 1);
                return ret;
            }
        }
        ret = choosePage((i / this.numStrings) + 1);
        return ret;
    }

    public IPair<Integer,Integer> pageInfo() {
        int currentPageStart = this.posicao - this.getNumStrings()+1;
        int currentPage = currentPageStart / this.getNumStrings();
        if(currentPageStart % this.getNumStrings() != 0) currentPage++;
        int numPages = this.nav.size() / this.getNumStrings();
        if (this.nav.size() % this.getNumStrings() != 0) numPages++;
        return new Pair<>(currentPage,numPages);
    }
}
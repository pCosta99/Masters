package Enums;

import Exception.TipoInvalidoException;

import java.io.Serializable;

/**
 * Associa um inteiro a um Tipo.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public enum Tipo implements Serializable {
    UTILIZADOR(1),VOLUNTARIO(2),EMPRESA(3),LOJA(4);

    private final int identificador;
    Tipo(int identificador){
        this.identificador = identificador;
    }


    /**
     * Função que retorn o identificador.
     * @return Identificador.
     */
    public int getIdentificador(){
        return identificador;
    }

    /**
     * Função que retorna o tipo associado a um identificador.
     * @param i, Identificador fornecido.
     * @return Tipo.
     * @throws TipoInvalidoException, Quando o tipo é inválido.
     */
    public static Tipo fromIdentificador(int i) throws TipoInvalidoException {
        Tipo t;
        switch(i){
            case 1: t = UTILIZADOR; break;
            case 2: t = VOLUNTARIO; break;
            case 3: t = EMPRESA;    break;
            case 4: t = LOJA;       break;
            default: throw new TipoInvalidoException(i);
        }
        return t;
    }

    /**
     * Função que verifica se um inteiro fornecido está presente nos identificadores.
     * @param i, Inteiro fornecido.
     * @return 'ture' se estiver.
     */
    public static boolean contains(int i){
        for(Tipo t : values())
            if(t.getIdentificador() == i)
                return true;
        return false;
    }

    /**
     * Função que converte os parametros do Tipo para string.
     * @return String com os parametros convertidos.
     */
    public String toString(){
        String s = null;
        switch (this){
            case UTILIZADOR: s = "Utilizador"; break;
            case VOLUNTARIO: s = "Voluntario"; break;
            case EMPRESA:    s = "Empresa";    break;
            case LOJA:       s = "Loja";       break;
        }
        return s;
    }

    /**
     * Função que guarda numa String[] os values de Tipo.
     * @return String[].
     */
    public static String[] allOptions(){
        String[] opcoes = new String[Tipo.values().length];
        int i = 0;
        for(Tipo t : Tipo.values())
            opcoes[i++] = t.toString();
        return opcoes;
    }
}

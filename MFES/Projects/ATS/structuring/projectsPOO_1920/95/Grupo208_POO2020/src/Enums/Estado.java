package Enums;

/**
 * Define um estado.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public enum Estado {
    ACEITE, ESPERARESPOSTA, AENTREGAR;

    /**
     * Função que verifica se é 'ACEITE'.
     * @return 'true' se for.
     */
    public boolean isAceite(){
        return this == ACEITE;
    }

    /**
     * Função que verifica se é 'ESPERARESPOSTA'.
     * @return 'true' se for.
     */
    public boolean isEsperarResposta(){
        return this == ESPERARESPOSTA;
    }

    /**
     * Função que verifica se é 'AENTREGAR'.
     * @return 'true' se for.
     */
    public boolean isAEntregar(){
        return this == AENTREGAR;
    }

    /**
     * Função que converte os parametros do Estado para string.
     * @return String com os parametros convertidos.
     */
    public String toString() {
        String res = null;
        switch(this){
            case ACEITE: res = "Aceite"; break;
            case AENTREGAR: res = "A entregar"; break;
            case ESPERARESPOSTA: res = "À espera de resposta"; break;
        }
        return res;
    }
}

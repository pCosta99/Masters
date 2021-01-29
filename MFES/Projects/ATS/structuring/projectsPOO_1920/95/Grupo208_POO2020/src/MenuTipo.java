import Enums.Tipo;
import Exception.TipoInvalidoException;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import static java.lang.System.out;

/**
 * Classe que auxilia a interação com o utilizador.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class MenuTipo implements Serializable {
    private final List<String> opcoes;
    private Tipo op;

    /**
     * Construtor de um MenuTipo.
     * @param opcoes, Opções do MenuTipo a construir.
     */
    public MenuTipo(String[] opcoes){
        this.opcoes = Arrays.asList(opcoes);
        this.op = null;
    }

    /**
     * Função que executa uma dada opção escolhida pelo utilizador.
     */
    public void executa(){
        int escolha = 0;
        do{
            this.showMenu();
            escolha = Input.lerInt("Tipo: ","Tipo inválido!");
        }while(!Tipo.contains(escolha));

        try{this.op = Tipo.fromIdentificador(escolha);}
        catch(TipoInvalidoException ignored){}
    }

    /**
     * Função que apresenta um conjunto de opções disponíveis.
     */
    public void showMenu(){
        for(int i = 0; i < this.opcoes.size(); i++)
            out.printf("%d - %s.\n",i+1,this.opcoes.get(i));
    }

    /**
     * Função que retorna o tipo.
     * @return Tipo.
     */
    public Tipo getOption(){
        return this.op;
    }
}

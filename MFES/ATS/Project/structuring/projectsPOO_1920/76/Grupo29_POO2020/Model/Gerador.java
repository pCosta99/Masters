package Model;

import java.util.Random;
import java.lang.Math;
import java.util.Set;

/**
 *   @class Gerador Classe responsavel pela geração de aleatoriedade
 */
public class Gerador {

    /**
     * Método responsavél por estipular o tempo que o distribuidor vai demorar a entregar a encomenda
     * @param dist distãncia a percorrer
     * @return tempo que a encomenda demora a ser entregada
     */
    public static long aleatoriedadeTempoPercurso(double dist, int tamFila){
        long tempo = (long) (dist*60)/40; //tempo (em minutos) necessario para percorrer essa distância a uma velocidade de 40 km/hora
        Random rand = new Random();
        long probAcidente=0;
        double var;

        //chove?
        if(rand.nextBoolean()) {
            tempo *= variacao(3,1);
            //se chover há maior probabilidade de haver acidentes
            probAcidente += variacao(10,0);
        }

        //transito?
        if(rand.nextBoolean()) {
            tempo *= variacao(3,1);
            //se houver transito há maior probabilidade de haver acidentes
            probAcidente += variacao(10,0);
        }

        //rua vazia?
        if(rand.nextBoolean()) {
            if((var = variacao(1,0))==0) var=0.001;
            tempo *= var;
            //a rua estiver vazia a probabilidade de haver um aumento de velocidade e cosequentemente haver acidentes é ligeiramente maior
            probAcidente += (1 + var);
        }
        //atalho?
        if(rand.nextBoolean()) tempo -= variacao(20,0);

        //Problemas a encontrar a morada?
        if(rand.nextBoolean()) tempo += variacao(20,0);

        //Acidente?
        if(probAcidente>10) {
            int max = (int) ((5 * probAcidente)/20);
            int numAcidentes = (int) variacao(max,0);
            for(int i =0;i<numAcidentes;i++ ){
                tempo += variacao(20,0);
            }
        }

        //Fila de espera na loja?
        if(rand.nextBoolean()) tempo += (tamFila - 1) * variacao(20,0);

        return tempo;
    }

    /**
     * Método que devolve uma variação positiva enquadrada entre os valores recebidos
     * @return double positivo
     */
    private static double variacao(int max, int min){
        double res= min + Math.random() * (max - min);
        return res;
    }


    /**
     * Gera um código de encomenda aleatório.
     * 
     * O código gerado será diferente dos já existentes nas encomenadas.
     * 
     * @param encomendas Conjunto de encomendas já existentes.
     * @return Código de encomenda único.
     */
    public static String gerarCodigoEncomenda(Set<String> encomendas) {
        Random rnd = new Random();
        String res = null;
        boolean r = true;

        while (r) {
            res = "e" + rnd.nextInt(100000);
            if (!encomendas.contains(res))
                r = false;
        }
        
        return res;
    }

    /**
     * Gera um código de entidade aleatório.
     * 
     * O código gerado será diferente dos já existentes nas encomenadas.
     * 
     * @param entidades Conjunto da entidades já existentes.
     * @return Código de encomenda único.
     */
    public static String gerarCodigoEntidade(Set<String> entidades, char tipo) {
        Random rnd = new Random();
        String res = null;
        boolean r = true;

        while (r) {
            res = tipo + String.valueOf(rnd.nextInt(100000));
            if (!entidades.contains(res))
                r = false;
        }
        
        return res;
    }
}
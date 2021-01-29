package DadosCreation;
import Model.Encomenda;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;


public class GerarEncomendas {
    static LeituraDados l;
    public static void main(String[] args) throws IOException {
        l = new LeituraDados();
        l.lerFicheiro("data/geraEncomendas/Lojas.txt");
        l.lerFicheiro("data/geraEncomendas/Produtos.txt");
        l.lerFicheiro("data/geraEncomendas/Utilizadores.txt");
        EscritaLogs.escreverEncomenda(geraEncomendas());

    }

    /**
     * Gera uma lista de encomendas
     * @return lista de encomendas
     */
    public static List<Encomenda> geraEncomendas (){
        List<Encomenda> enc = new ArrayList<>();
        int size = 2;
        for (int i = 0; i < 366 ; i+=size) {
            enc.add(geraEncomenda(i,size));
        }
        return enc;
    }

    /**
     * Gera uma encomenda
     * @param i posicao na lista de produtos
     * @return Encomenda gerada
     */
    private static Encomenda geraEncomenda(int i,int size){
        Random var = new Random();
        List<Model.LinhaEncomenda> linhasEnc = new ArrayList<>();

        for (int k = i; i < k + size ; i++) {
            Model.LinhaEncomenda aux = new Model.LinhaEncomenda();
            aux.setDescricao(l.getProdutos().get(i));
            double rand2 = (var.nextDouble() + 0.0001) * 100;
            aux.setQuantidade(rand2);
            aux.setReferencia("p" + var.nextInt(1500));
            double rand1 = (var.nextDouble() + 0.0001) * 100;
            aux.setPreco(rand1);
            linhasEnc.add(aux);
        }
        double peso = 5 * (var.nextDouble() + 0.0001) * 10;

        Random r = new Random();
        int low = 0;
        int high = 9;

        return new Model.Encomenda(l.getUtilizadores().get(r.nextInt(high-low) + low), "e" + (r.nextInt(999) + 999), null,null,peso,linhasEnc,null, l.getLojas().get(r.nextInt(high-low) + low),var.nextBoolean(),0);

    }
}

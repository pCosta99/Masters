package DadosCreation;
import Model.Encomenda;
import Model.LinhaEncomenda;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class EscritaLogs {
/*
 -- Encomenda

   Encomenda:<CodEncomenda>, <CodUtilizador>, <CodLoja>, <Peso>, <LinhaEncomenda>+

 -- LinhaEncomenda

    <CodProduto>, <Descrição>, <Quantidade>, <ValorUnitário>
*/

    /**
     * Guarda uma lista de encomendas num ficheiro
     * @param l lista de encomendas
     * @throws IOException ??
     */
    public static void escreverEncomenda(List<Encomenda> l) throws IOException {
        BufferedWriter buffw = new BufferedWriter(new FileWriter("data/geraEncomendas/temp.txt"));

        for(Encomenda e : l) {
            StringBuilder sb = new StringBuilder();

            sb.append("Encomenda:");
            sb.append(e.getNumEnc()).append(',');
            sb.append(e.getNome()).append(',');
            sb.append(e.getLoja()).append(',');
            sb.append(e.getPeso()).append(',');
            sb.append(gerarLinhasEnc(e.getElementos()));
            sb.append('\n');

            buffw.write(sb.toString());
        }

        buffw.flush();
        buffw.close();
    }

    /**
     * Coloca uma lista de linhas de encomenda com o formato dos logs
     * @param l lista de linhas de encomenda
     * @return String resultante
     */
    public static String gerarLinhasEnc(List<LinhaEncomenda> l) {
        StringBuilder sb = new StringBuilder();
        for(LinhaEncomenda e : l) {
            sb.append(e.getReferencia()).append(',');
            sb.append(e.getDescricao()).append(',');
            sb.append(e.getQuantidade()).append(',');
            sb.append(e.getPreco()).append(',');
        }
        sb.setLength(sb.length() - 1);
        return sb.toString();
    }
}

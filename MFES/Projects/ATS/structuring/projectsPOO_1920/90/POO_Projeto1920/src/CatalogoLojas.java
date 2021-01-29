import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que representa calalogo de produtos que as lojas vendem
 */
public class CatalogoLojas implements Catalogos, Serializable {
    private Map<String, Collection<LinhaEncomenda>> infoProdutos;


    public CatalogoLojas() {
        infoProdutos = new HashMap<>();
    }


    /**
     * Insere um produto no catálogo
     * @param codLoja Código da loja
     * @param le O que se adiciona
     */
    public void insereProduto(String codLoja, LinhaEncomenda le) {
        if (!infoProdutos.containsKey(codLoja)) {
            Collection<LinhaEncomenda> nova = new ArrayList<>();
            nova.add(le.clone());
            infoProdutos.put(codLoja, nova);
        } else {
            Collection<LinhaEncomenda> c = infoProdutos.get(codLoja);
            int numRepetidos = (int) c.stream()
                    .filter(l -> l.getDesc().equals(le.getDesc())).count();
            if (numRepetidos == 0) c.add(le.clone());
        }
    }


    /**
     * Indica uma página específica de um catálogo de uma loja
     * @param codLoja Código da loja
     * @param p página pretendida
     * @return String com a página pretendida
     */
    public String separaPorPaginas(String codLoja, int p) {
        if (infoProdutos.containsKey(codLoja)) {
            StringBuilder sb;
            Collection<LinhaEncomenda> c = infoProdutos.get(codLoja);
            String res = "";
            Iterator it = c.stream().skip((p - 1) * 10).
                    collect(Collectors.toCollection(ArrayList::new)).iterator();
            int i = 0;
            sb = new StringBuilder();
            sb.append("Produto | Preço/unidade | Peso/unidade | Código do Produto\n\n");
            while (it.hasNext() && i < 10) {
                LinhaEncomenda le = (LinhaEncomenda) it.next();
                sb.append(le.getDesc()).append(" | ")
                        .append(le.getValorUnitario()).append(" € | ")
                        .append(le.getPeso()).append(" kg | ")
                        .append(le.getCod()).append("\n");
                i++;
            }
            res = sb.toString();
            if (i == 0) return "Não existe catálogo para esta loja!\n";
            else return res;
        } else return "Não existe catálogo para esta loja!\n";
    }


    /**
     * Verifica se existe um produto numa loja
     * @param codLoja Código da loja
     * @param codProd Código do Produto
     * @return boolean que indica se existe
     */
    public boolean existeProduto(String codLoja, String codProd) {
        if (!infoProdutos.containsKey(codLoja)) return false;
        else {
            Collection<LinhaEncomenda> c = infoProdutos.get(codLoja);
            for (LinhaEncomenda le : c) if (le.getCod().equals(codProd)) return true;
        }
        return false;
    }


    /**
     * Indica o preço de um produto
     * @param codProd código do produto
     * @param codLoja código da loja
     * @return double que indica o preço do produto
     */
    public double precoDeUmProduto(String codProd, String codLoja) {
        Collection<LinhaEncomenda> c = infoProdutos.get(codLoja);
        Iterator it = c.iterator();
        boolean encontrado = false;
        double res = 0.0;
        while (it.hasNext() && !encontrado) {
            LinhaEncomenda le = (LinhaEncomenda) it.next();
            if (codProd.equals(le.getCod())) {
                encontrado = true;
                res = le.getValorUnitario();
            }
        }
        return res;
    }

    /**
     * Indica o peso de um produto
     * @param codProd código do produto
     * @param codLoja código da loja
     * @return double que indica o peso do produto
     */
    public double pesoDeUmProduto(String codProd, String codLoja) {
        Collection<LinhaEncomenda> c = infoProdutos.get(codLoja);
        Iterator it = c.iterator();
        boolean encontrado = false;
        double res = 0.0;
        while (it.hasNext() && !encontrado) {
            LinhaEncomenda le = (LinhaEncomenda) it.next();
            if (codProd.equals(le.getCod())) {
                encontrado = true;
                res = le.getPeso();
            }
        }
        return res;
    }


    /**
     * Indica o nome de um produto
     * @param codProd código do produto
     * @param codLoja código da loja
     * @return String que indica o nome do produto
     */
    public String nomeDeUmProduto(String codProd, String codLoja) {
        Collection<LinhaEncomenda> c = infoProdutos.get(codLoja);
        Iterator it = c.iterator();
        boolean encontrado = false;
        String res = "";
        while (it.hasNext() && !encontrado) {
            LinhaEncomenda le = (LinhaEncomenda) it.next();
            if (codProd.equals(le.getCod())) {
                encontrado = true;
                res = le.getDesc();
            }
        }
        return res;
    }


    /**
     * Devolve os catálogos de produtos existentes
     */
    public Map<String, Collection<LinhaEncomenda>> getInfoProdutos(){
        return infoProdutos;
    }

    /**
     * adiciona a uma loja um catálogo
     * @param codLoja código da loja
     * @param catalogo Catálogo que vai ser adicionado
     */
    public void adicionaInfoProdutos(String codLoja, Collection<LinhaEncomenda> catalogo){
        infoProdutos.put(codLoja, catalogo);
    }

    /**
     * Coloca em String
     */
    public String toString() {
        final StringBuilder sb = new StringBuilder("CatalogoLojas{");
        sb.append("infoProdutos=").append(infoProdutos.keySet());
        sb.append('}');
        return sb.toString();
    }

}

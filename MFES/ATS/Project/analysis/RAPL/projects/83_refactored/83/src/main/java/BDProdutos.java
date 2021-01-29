import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

    public class BDProdutos implements Serializable {
        private Map<String, LinhaEncomenda> produtos;
        private Set<String> codigos;

        public BDProdutos() {
            this.produtos = new TreeMap<>();
            this.codigos = new TreeSet<>();
        }

        public BDProdutos(BDProdutos r) {
            setProdutos(r.getProdutos());
            setCodigos(r.getCodigos());
        }

        public Map<String, LinhaEncomenda> getProdutos() {
            return this.produtos.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, r -> r.getValue().clone()));
        }


        public void setProdutos(Map<String, LinhaEncomenda> produtos) {
            this.produtos = new TreeMap<>();
            produtos.forEach((key, value) -> this.produtos.put(key, value.clone()));
        }

        /**
         * Método que devolve um map com os produtos que são considerados como médicos
         * @param produtos é o map com todos os produtos
         */
        public Map<String, LinhaEncomenda> getProdMedicos(Map<String, LinhaEncomenda> produtos){
            Map<String, LinhaEncomenda> aux = new TreeMap<>();
            for(Map.Entry<String, LinhaEncomenda> e : produtos.entrySet()){
                if(e.getKey().equals("Desinfetante") || e.getKey().equals("Alcool") || e.getKey().equals("Saco de lixo 50l") || e.getKey().equals("Saco de lixo 30l")){
                    aux.put(e.getKey(), e.getValue().clone());
                }
            }
            return aux;
        }

        public Set<String> getCodigos() {
            return new HashSet<>(this.codigos);
        }

        public void setCodigos(Set<String> codigos) {
            this.codigos = new TreeSet<>();
            this.codigos.addAll(codigos);
        }


        public BDProdutos clone() {
            return new BDProdutos(this);
        }

        public String toString() {
            return "Total de Produtos: " + "\n" +
                    this.produtos;
        }

        public boolean equals(Object obj) {
            if (obj == this) return true;
            if (obj == null || obj.getClass() != this.getClass()) return false;
            BDProdutos r = (BDProdutos) obj;
            return this.produtos.equals(r.getProdutos());
        }

        /**
         * Método que verifica se um produto existe
         * @param v é o produto a ser testado
         */

        public boolean existe(String v) {
            return this.produtos.containsKey(v);
        }

        /**
         * Método que adiciona um novo produto
         * @param v linha encomenda
         */
        public void add(LinhaEncomenda v) {
            this.produtos.put(v.getDescricao(), v.clone());
        }

        /**
         * Método que determina os produtos que não são médicos, que serão depois impressos
         */
        public String listProdutosNormais(){
            StringBuilder sb = new StringBuilder();
            Map<String, LinhaEncomenda> normais = new HashMap<>();
            for(Map.Entry<String, LinhaEncomenda> e : this.produtos.entrySet()){
                if(!e.getKey().equals("Desinfetante") && !e.getKey().equals("Alcool") && !e.getKey().equals("Saco de lixo 50l") && !e.getKey().equals("Saco de lixo 30l")){
                    normais.put(e.getKey(), e.getValue().clone());
                }
            }
            sb.append("LISTA DE PRODUTOS\n");
            for(String s: normais.keySet()){
                sb.append("--> ").append(s).append("\n");
            }
            return sb.toString();
        }

        /**
         * Método que devolve os produtos médicos para serem impressos
         */
        public String listProdutosMedicos(){
            StringBuilder sb = new StringBuilder();
            Map<String, LinhaEncomenda> aux = getProdMedicos(this.produtos);
            sb.append("LISTA DE PRODUTOS MÉDICOS\n");
            for(String s: aux.keySet()){
                sb.append("--> ").append(s).append("\n");
            }
            return sb.toString();
        }


        /**
         * Método que verifica se um produto existe
         * @param cod codigo
         * @throws ProductNotFoundException se o produto nao existe
         */
        public void existeProd(String cod) throws ProductNotFoundException{
            if(!this.produtos.containsKey(cod)) throw new ProductNotFoundException();
        }
    }

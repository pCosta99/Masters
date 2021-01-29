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

        public BDProdutos(Map<String, LinhaEncomenda> produtos, Set<String> codigos) {
            setProdutos(produtos);
            setCodigos(codigos);
        }

        public BDProdutos(BDProdutos r) {
            setProdutos(r.getProdutos());
            setCodigos(r.getCodigos());
        }

        public Map<String, LinhaEncomenda> getProdutos() {
            return this.produtos.entrySet().stream().collect(Collectors.toMap(r -> r.getKey(), r -> r.getValue().clone()));
        }


        public void setProdutos(Map<String, LinhaEncomenda> produtos) {
            this.produtos = new TreeMap<>();
            produtos.entrySet().forEach(e -> this.produtos.put(e.getKey(), e.getValue().clone()));
        }

        /**
         * Método que devolve um map com os produtos que são considerados como médicos
         * @param produtos é o map com todos os produtos
         * @return
         */
        public Map<String, LinhaEncomenda> getProdMedicos(Map<String, LinhaEncomenda> produtos){
            Map<String, LinhaEncomenda> aux = new TreeMap<>();
            for(String s: produtos.keySet()){
                if(s.equals("Desinfetante") || s.equals("Alcool") || s.equals("Saco de lixo 50l") || s.equals("Saco de lixo 30l")){
                    aux.put(s, produtos.get(s).clone());
                }
            }
            return aux;
        }

        public Set<String> getCodigos() {
            return this.codigos.stream().collect(Collectors.toSet());
        }

        public void setCodigos(Set<String> codigos) {
            this.codigos = new TreeSet<>();
            for (String s : codigos) this.codigos.add(s);
        }


        public BDProdutos clone() {
            return new BDProdutos(this);
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("Total de Produtos: ").append("\n");
            sb.append(this.produtos);

            return sb.toString();
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
         * @return
         */

        public boolean existe(String v) {
            return this.produtos.keySet().contains(v);
        }

        /**
         * Método que adiciona um novo produto
         * @param v
         */
        public void add(LinhaEncomenda v) {
            this.produtos.put(v.getDescricao(), v.clone());
        }

        /**
         * Método que determina os produtos que não são médicos, que serão depois impressos
         * @return
         */
        public String listProdutosNormais(){
            StringBuilder sb = new StringBuilder();
            Map<String, LinhaEncomenda> normais = new HashMap<>();
            for(String s: this.produtos.keySet()){
                if(!s.equals("Desinfetante") && !s.equals("Alcool") && !s.equals("Saco de lixo 50l") && !s.equals("Saco de lixo 30l")){
                    normais.put(s, this.produtos.get(s));
                }
            }
            sb.append("LISTA DE PRODUTOS\n");
            for(String s: normais.keySet()){
                sb.append("--> " + s + "\n");
            }
            return sb.toString();
        }

        /**
         * Método que devolve os produtos médicos para serem impressos
         * @return
         */
        public String listProdutosMedicos(){
            StringBuilder sb = new StringBuilder();
            Map<String, LinhaEncomenda> aux = getProdMedicos(this.produtos);
            sb.append("LISTA DE PRODUTOS MÉDICOS\n");
            for(String s: aux.keySet()){
                sb.append("--> " + s + "\n");
            }
            return sb.toString();
        }


        /**
         * Método que verifica se um produto existe
         * @param cod
         * @return
         * @throws ProductNotFoundException
         */
        public boolean existeProd(String cod) throws ProductNotFoundException{
            if(!this.produtos.containsKey(cod)) throw new ProductNotFoundException();
            else return true;
        }

    }

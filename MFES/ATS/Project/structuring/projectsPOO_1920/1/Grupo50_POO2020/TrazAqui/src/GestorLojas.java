import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class GestorLojas implements Serializable{
        private Map<String, Loja> lojas;

        public GestorLojas(){
            this.lojas = new HashMap<String, Loja>();
        }

        public GestorLojas(Map<String, Loja> lojas){
            this.lojas = lojas;
        }

        public GestorLojas(GestorLojas umaBaseDados){
            this.lojas = umaBaseDados.getLojas();
        }


        /**
         * Getters & Setters
         */

        /**
         * Obter uma copia do mapeamento das Lojas
         * @return Map<String: código da Loja, Loja>
         */
        public Map<String, Loja> getLojas(){
            return this.lojas;
        }

        /**
         * @param novasLojas
         */
        public void setLojas(Map<String, Loja> novasLojas){
            this.lojas = novasLojas;
        }

        /**
         * Verificar a existência de uma encomenda dado o seu codigo
         * @param codigo da encomenda
         * @return True se a encomenda existir
         */
        public boolean existeLoja(String codigo){
            return this.lojas.containsKey(codigo);
        }

        /**
         *
         * @return Número de lojas
         */
        public int numLojas(){
            return this.lojas.size();
        }

        /**
         * Adiciona a informação de uma nova encomenda
         * @param e nova encomenda a inserir
         */
        public void addLoja(Loja e) throws EntidadeRepetidaException{
            if(this.lojas.putIfAbsent(e.getCodigo(), e.clone()) != null)
                throw new EntidadeRepetidaException("Já existe uma loja com o essa referência " + e.getCodigo() + " na App");
        }

        /**
         *
         * @param cod código da encomenda a procurar
         * @return cópia da encomenda encontrada
         * @throws EntidadeInexistenteException caso o código da encomenda não exista
         */
        public Loja getLoja(String cod) throws EntidadeInexistenteException{
            Loja l;

            try{
                l = this.lojas.get(cod);
            }
            catch(NullPointerException e){
                throw new EntidadeInexistenteException("Não existe objeto com nome " + cod);
            }
            return l;
        }

    public int tamanho(){
        return this.lojas.size();
    }

        /**
         *
         * @return Lista das lojas
         */
        public List<Loja> getLojaAsList(){
            return lojas.values()
                    .stream()
                    .map(Loja::clone)
                    .collect(Collectors.toList());
        }


    /**
     *
     * @param localizacao Localização de lojas
     * @param raio Raio máximo de lojas
     * @return Lista de lojas que se apresentam numa localização dentro de um certo raio
     */
        public List<Loja> lojasPeriferia(GPS localizacao, double raio){
            return this.lojas.values().stream()
                .filter(loja -> loja.getLocalizacao().dentroRaio(localizacao, raio))
                .collect(Collectors.toList());
    }

}


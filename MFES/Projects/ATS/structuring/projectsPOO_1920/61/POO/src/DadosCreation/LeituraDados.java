package DadosCreation;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


public class LeituraDados {

    private List<String> utilizadores;
    private List<String> lojas;
    private List<String> produtos;

    /**
     * Construtor da classe
     */
    public LeituraDados(){
        this.utilizadores = new ArrayList<>();
        this.produtos = new ArrayList<>();
        this.lojas = new ArrayList<>();
    }

    /**
     * Indica os códigos dos utilizadores
     * @return Os códigos dos utilizadores
     */
    public List<String> getUtilizadores(){
        return new ArrayList<>(utilizadores);
    }

    /**
     * Define os códigos dos utilizadores
     * @param newU Os códigos dos utilizadores a definir
     */
    public void setUtilizadores(List<String> newU){
        utilizadores = new ArrayList<>(newU);
    }

    /**
     * Indica os códigos das lojas
     * @return Os códigos das lojas
     */
    public List<String> getLojas() {
        return new ArrayList<>(lojas);
    }

    /**
     * Define os códigos das lojas
     * @param lojas Os códigos das lojas a definir
     */
    public void setLojas(List<String> lojas) {
        this.lojas = new ArrayList<>(lojas);
    }

    /**
     * Indica os códigos dos produtos
     * @return Os códigos dos produtos
     */
    public List<String> getProdutos() {
        return new ArrayList<>(produtos);
    }

    /**
     * Define os códigos dos produtos
     * @param produtos O códigos dos produtos a definir
     */
    public void setProdutos(List<String> produtos) {
        this.produtos = new ArrayList<>(produtos);
    }

    /**
     * Lê um ficheiro
     * @param file O nome do ficheiro
     * @throws IOException Caso o ficheiro não exista
     */
    public void lerFicheiro(String file) throws IOException {
        BufferedReader buffr = new BufferedReader(new FileReader(file));

        String linha = buffr.readLine();
        while (linha != null) {
            processaDados(linha, file);
            linha = buffr.readLine();
        }
        buffr.close();
    }

    /**
     * Armazena os dados
     * @param dados O código a ser armazenado
     * @param FileName O nome do ficheiro de origem
     */
    private void processaDados(String dados, String FileName){

        switch (FileName) {
            case "data/geraEncomendas/Utilizadores.txt": {
                utilizadores.add(dados);
                break;
            }
            case "data/geraEncomendas/Produtos.txt": {
                produtos.add(dados);
                break;
            }
            case "data/geraEncomendas/Lojas.txt": {
                lojas.add(dados);
                break;
            }

            default:
                break;
        }

    }

}



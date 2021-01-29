package Model;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Set;

import Exceptions.NoEntityException;

/** Interface da classe Encomenda. */
public interface IEncomenda {

    /** 
     * Retorna o código da encomenda.
     * @return Código da encomenda. 
     */
    String getCodigo();

    /** 
     * Retorna o código da loja.
     * @return Código da loja onde foi feita a encomenda. 
     */
    String getCodLoja();

    /**
     * Retorna o código do utilizador. 
     * @return Código do utilizador que encomendou. 
     */
    String getCodUtil();

    /** 
     * Retorna a data em que a encomenda foi aceite.
     * @return Data em que a encomenda foi aceite. 
     */
    LocalDateTime getDataAceitacao();

    /** 
     * Retorna o peso da encomenda.
     * @return Peso da encomenda. 
     */
    double getPeso();
    
    /** 
     * Retorna a data do pedido de encomenda.
     * @return Data do pedido de encomenda. 
     */
    LocalDateTime getData();
    
    /**
     * Define data em que a encomenda foi aceite.
     * @param ldt Data de aceitação.
     */
    void setDataAceitacao(LocalDateTime ldt);
    
    /**
     * Define custo dos portes da encomenda.
     * @param portes Custo dos portes.
     */
    void setPortes(BigDecimal portes);

    /** 
     * Retorna o custo dos portes da encomenda.
     * @return Custo dos portes da encomenda. 
     */
    BigDecimal getPortes();

    /** 
     * Indica se a encomenda contém medicamentos.
     * @return Valor booleano a indicar se a encomenda contém medicamentos. 
     */
    boolean getMedica();

    /**
     * Define se uma encomenda contém medicamentos ou não.
     * @param medica Valor booleano a indicar se a encomenda contém medicamentos.
     */
    void setMedica(boolean medica);

    /**
     * Define o peso da encomenda.
     * @param peso Peso da encomenda.
     */
    void setPeso(double peso);

    /**
     * Indica o número de linhas de encomenda na encomenda.
     * @return Número de linhas de encomenda presentes.
     */
    int nLinhasEnc();
    
    /**
     * Retorna o conjunto dos códigos dos produtos nas linhas de encomenda.
     * @return Conjunto dos códigos dos produtos.
     */
    Set<String> getProdutosDasLinhas();

    /**
     * Define o preço unitário de um produto na linha de encomendas.
     * @param codigo Código do produto.
     * @param preco  Preço unitário.
     * @throws NoEntityException Caso não exista o produto com o dado código.
     */
    void setValorUnitn(String codigo, BigDecimal preco) throws NoEntityException;
    
}
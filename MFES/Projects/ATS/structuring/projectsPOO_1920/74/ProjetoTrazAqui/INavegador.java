import java.util.List;

public interface INavegador{
	/*
     *Avança a pagina. 
     *@return um boolean com true se poder avançar a pagina e false se nao for possível.
     */
	boolean nextPage();
	/*
     *Avança para a página anterior.
     *@return um boolean a indicar se pode retroceder.
    */
	boolean previousPage();
	/*
     *Avança para a última página.
     *@return boolean a indicar se é possivel avançar para a ultima página.
    */
	boolean lastPage();
	 /*
     *Avança para a primeira página.
     *@return boolean a indicar se é possivel avançar para a primeira página.
    */
	boolean firstPage();
	/*
     *Avança para a página indicada no index.
     *@param index indice da pagina que queremos aceder.
     *@return boolean a indicar se é possivel aceder essa página.
    */
	boolean jumpToPage(int i);
	/** 
     * devolve o numero de paginas
     * @return int representa o numero de paginas 
     */
	int getNumberPages();
	/**
     * Devolve a List<String> elems da classe
     * @return List<String> da classe
     */	
	List<String> getPage();
	 /**
     * Devolve o int page da classe
     * @return int page da classe
     */
	int getPageNumber();
	/**
     * Metodo que imprime o menu da pagina do Navegador
     * @return String correspondente ao menu
     */
	String showPage();
}
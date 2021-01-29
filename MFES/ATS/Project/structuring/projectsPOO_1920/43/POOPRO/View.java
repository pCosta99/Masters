public class  View {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */


    /**
     * CONSTRUTOR VAZIO
     */

    public View() {

    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    public View(View nView) {

    }

    /**
     * MÉTODO CLONE
     */

    /*
    public View clone() {
        return new View(this);
    }*/

    /**
     * MÉTODOS
     */

    public void paginaInicial() {
        System.out.println("____________________________");
        System.out.println("*PAGINA INICIAL*");
        System.out.println("[1] LOGIN");
        System.out.println("[2] REGISTO");
        System.out.println("[3] QUERIES");
        System.out.println("[4] GRAVAR ESTADO");
        System.out.println("[5] LER ESTADO");
        System.out.println("[6] CARREGAR LOG");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void paginaLogin() {
        System.out.println("____________________________");
        System.out.println("*PAGINA LOGIN*");
        System.out.println("[1] LOGIN CLIENTE");
        System.out.println("[2] LOGIN LOJA");
        System.out.println("[3] LOGIN TRANSPORTADORA");
        System.out.println("[4] LOGIN VOLUNTARIO");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void paginaRegisto() {
        System.out.println("____________________________");
        System.out.println("*PAGINA REGISTO*");
        System.out.println("[1] REGISTO CLIENTE");
        System.out.println("[2] REGISTO LOJA");
        System.out.println("[3] REGISTO TRANSPORTADORA");
        System.out.println("[4] REGISTO VOLUNTARIO");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void paginaQueries() {
        System.out.println("____________________________");
        System.out.println("*PAGINA QUERIES*");
        System.out.println("[1] TOTAL FATURADO TRANSPORTADORA NUM PERÍODO");
        System.out.println("[2] 10 EMPRESAS QUE MAIS UTILIZAM O SISTEMA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuCliente() {
        System.out.println("____________________________");
        System.out.println("*MENU CLIENTE*");
        System.out.println("[1] COMEÇAR ENCOMENDA");
        System.out.println("[2] VER PEDIDOS");
        System.out.println("[3] VER REGISTO");
        System.out.println("[4] CLASSIFICAR TRANSPORTE");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuLoja() {
        System.out.println("____________________________");
        System.out.println("*MENU LOJA*");
        System.out.println("[1] VER PEDIDOS");
        System.out.println("[2] VER PEDIDOS ACEITES");
        System.out.println("[3] VER PEDIDOS PRONTOS");
        System.out.println("[4] VER PRODUTOS");
        System.out.println("[5] VER REGISTO");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuTransportadora() {
        System.out.println("____________________________");
        System.out.println("*MENU TRANSPORTADORA*");
        System.out.println("[1] VER PEDIDOS");
        System.out.println("[2] VER PEDIDOS ACEITES");
        System.out.println("[3] VER REGISTO");
        System.out.println("[4] VER CLASSIFICACAO ATUAL");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuVoluntario() {
        System.out.println("____________________________");
        System.out.println("*MENU VOLUNTARIO*");
        System.out.println("[1] VER PEDIDOS");
        System.out.println("[2] VER PEDIDOS ACEITES");
        System.out.println("[3] VER REGISTO");
        System.out.println("[4] VER CLASSIFICACAO ATUAL");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuClassificar() {
        System.out.println("____________________________");
        System.out.println("*MENU CLASSIFICAR*");
        System.out.println("[1] CLASSIFICAR TRANSPORTE");
        System.out.println("[2] IGNORAR TRANSPORTE");
        System.out.println("[3] PROXIMA PAGINA");
        System.out.println("[4] PAGINA ANTERIOR");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuListaLojas() {
        System.out.println("____________________________");
        System.out.println("*LISTA DE LOJAS*");
        System.out.println("[1] SELECIONAR LOJA");
        System.out.println("[2] PAGINA ANTERIOR");
        System.out.println("[3] PROXIMA PAGINA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuListaEncomendas() {
        System.out.println("____________________________");
        System.out.println("*LISTA DE ENCOMENDAS - REGISTO*");
        System.out.println("[1] PAGINA ANTERIOR");
        System.out.println("[2] PROXIMA PAGINA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuListaProdutos() {
        System.out.println("____________________________");
        System.out.println("*LISTA DE PRODUTOS - CLIENTE*");
        System.out.println("[1] SELECIONAR PRODUTO");
        System.out.println("[2] FINALIZADO");
        System.out.println("[3] PAGINA ANTERIOR");
        System.out.println("[4] PROXIMA PAGINA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuPedidos() {
        System.out.println("____________________________");
        System.out.println("*LISTA DE PEDIDOS*");
        System.out.println("[1] ACEITAR PEDIDO");
        System.out.println("[2] RECUSAR PEDIDO");
        System.out.println("[3] PAGINA ANTERIOR");
        System.out.println("[4] PROXIMA PAGINA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuPedidosAceites() {
        System.out.println("____________________________");
        System.out.println("*LISTA DE PEDIDOS*");
        System.out.println("[1] PEDIDO PRONTO");
        System.out.println("[2] PAGINA ANTERIOR");
        System.out.println("[3] PROXIMA PAGINA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuPedidosProntos() {
        System.out.println("____________________________");
        System.out.println("*LISTA DE PEDIDOS PRONTOS*");
        System.out.println("[1] PAGINA ANTERIOR");
        System.out.println("[2] PROXIMA PAGINA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuProdutosLoja() {
        System.out.println("____________________________");
        System.out.println("*LISTA DE PRODUTOS - LOJA*");
        System.out.println("[1] ADICIONAR PRODUTO");
        System.out.println("[2] PAGINA ANTERIOR");
        System.out.println("[3] PROXIMA PAGINA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void menuPedidosTransportadora() {
        System.out.println("____________________________");
        System.out.println("*LISTA DE PEDIDOS*");
        System.out.println("[1] ACEITAR PEDIDO");
        System.out.println("[2] PAGINA ANTERIOR");
        System.out.println("[3] PROXIMA PAGINA");
        System.out.println("[0] QUIT");
        System.out.println("____________________________");
    }

    public void limparEcra() {
        System.out.flush();
    }

    public void imprimeLinhaSM(String linha) {
        System.out.print(linha);
    }

    public void imprimeLinhaCM(String linha) {
        System.out.println(linha);
    }

}

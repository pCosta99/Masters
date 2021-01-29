package View;

/**
 * Classe que imprime menus
 */
public interface ViewMenu {

    /**
     * Menu geral
     */
    static void mainmenu() {
        System.out.println("\n************ TrazAqui! ************\n");
        System.out.println("-> Carregar estado (1)");
        System.out.println("-> Guardar estado (2)\n");
        System.out.println("-> Registar novo utilizador/voluntário/empresa/loja (3)"); // -> register
        System.out.println("-> Estatísticas (4)\n"); // -> stats
        System.out.println("-> Login Utilizador (5)"); // -> user
        System.out.println("-> Login Voluntário ou Empresa (6)\n"); // -> agent
        System.out.println("-> Sair (0)");
    }

    /**
     * Menu para registar novas entidades
     */
    static void register() {
        System.out.println("\n* Novo Utilizador (1)");
        System.out.println("* Novo Voluntário (2)");
        System.out.println("* Nova Empresa (3)");
        System.out.println("* Nova Loja (4)");
        System.out.println("\n-> Voltar (0)");
    }

    /**
     * Menu das queries estatisticas
     */
    static void stats() {
        System.out.println("\n* Total faturado até agora, por empresa (1)");
        System.out.println("* Lista de utilizadores que mais usam a aplicação (2)");
        System.out.println("* Lista de empresas que mais usam a aplicação (3)");
        System.out.println("\n-> Voltar (0)");
    }

    /**
     * Menu do utilizador
     */
    static void user() {
        System.out.println("\n* Solicitar nova encomenda (1)");
        System.out.println("* Ver lista de encomendas prontas (2)");
        System.out.println("* Ver histórico de encomendas entregues (3)");
        System.out.println("* Classificar serviço (4)");
        System.out.println("* Confirmar chegada da encomenda (5)");
        System.out.println("\n-> Logout (0)");
    }

    /**
     * Menu dos agentes
     */
    static void agent() {
        System.out.println("\n* Transportar nova encomenda (1)");
        System.out.println("* Alterar estado (2)");
        System.out.println("* Ver lista de encomendas pendentes (3)");
        System.out.println("* Ver histórico de encomendas entregues (4)");
        System.out.println("\n-> Logout (0)");
    }

    /**
     * Menu do carrinho de compras
     */
    static void carrinho(){
        System.out.println("\n* Adicionar produto (1)");
        System.out.println("* Remover produto (2)");
        System.out.println("* Ver lista de produtos novamente (3)");
        System.out.println("* Concluir encomenda (4)");
        System.out.println("\n-> Cancelar e sair (0)");
    }

}

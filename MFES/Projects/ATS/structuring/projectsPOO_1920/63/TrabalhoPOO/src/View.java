public class View {
    /** variaveis de instancia */

    /** constructores de classe */
    /** vazio */

    /** parametrico */

    /** copia */

    /** gets/sets das variaveis de instancia */

    /** metodos override */

    /** metodos especificos */
    /**
     * Janela de entrar/registo (1º passo)
     */
    public void inicio(){
        System.out.println("1 - Entrar");
        System.out.println("2 - Registar");
        System.out.println("3 - Sobre");
        System.out.println("4 - Estatisticas");
        System.out.println("0 - Sair");
    }

    /**
     * Janela de entrar/registo nivel 1
     * @param i = 0 se entrar ou i = 1 se registar
     */
    public void entrarOUregistar1(int i){
        if(i==0){
            System.out.println("Indique que tipo de usuário pretende entrar:");
        }else{
            System.out.println("Indique que tipo de usuário pretende registar:");
        }
        System.out.println("1 - Utilizador");
        System.out.println("2 - Voluntário");
        System.out.println("3 - Transportadora");
        System.out.println("4 - Loja");
        System.out.println("0 - Sair");
    }

    /**
     * Janela de entrar/registo nivel 2
     */
    public void entrarOUregistar2(){
        System.out.println("1 - Email");
        System.out.println("2 - Password");
        System.out.println("3 - Entrar");
    }

    /**
     * Janela do sobre
     */
    public void sobre(){
        System.out.println("Traz Aqui é uma aplicação para a gestão de entregas de encomendas.");
        System.out.println("Desenvolvida em Java. Versão 1.0.");
        System.out.println("Autores:");
        System.out.println("\tA89555 Diogo Barros");
        System.out.println("\tA89492 Ivo Boas");
        System.out.println("\tA42865 Mário Coelho");
        System.out.println("\n\n0 - Sair");
    }

    /**
     * Janela das Utilizadores
     */
    public void janelaUtilizador(String codUtilizador){
        System.out.println("Bemvindo " + codUtilizador);
        System.out.println("Indique a operação que pretende efetuar");
        System.out.println("1 - Registar Nova Entrega");
        System.out.println("2 - Estado Entrega");
        System.out.println("3 - Classificar Entrega");
        System.out.println("4 - Histórico Entregas");
        System.out.println("0 - Sair");
    }

    /**
     * Janela das Transportadoras
     */
    public void janelaTransportadora(){
        System.out.println("Menu de Transportadora");
        System.out.println("Indique a operação que pretende efetuar");
        System.out.println("1 - Entregas Disponiveis");
        System.out.println("2 - Propor Entraga");
        System.out.println("3 - Relatório Entrega");
        System.out.println("0 - Sair");
    }

    /**
     * Janela dos Voluntario
     */
    public void janelaVoluntario(){
        System.out.println("Menu do Voluntario");
        System.out.println("Indique a operação que pretende efetuar");
        System.out.println("1 - Sinalizar que pode recolher uma encomenda");
        System.out.println("2 - Ir busar uma encomenda a uma loja");
        System.out.println("3 - Fazer o transporte da encomenda");
        System.out.println("0 - Sair");
    }

    /**
     * Janela das lojas
     */
    public void janelaLoja(){
        System.out.println("Menu da Loja");
        System.out.println("Indique a operação que pretende efetuar");
        System.out.println("1 - Sinalizar que uma encomenda pode ser levantada");
        System.out.println("0 - Sair");
    }

    public static void print(String string){
        System.out.print(string);
    }

    public void janelaEstatisticas(){
        System.out.println("Menu das Estatisticas");
        System.out.println("1 - Ver a faturacao de uma Transportadora");
        System.out.println("2 - 10 Transportadoras que precorreram mais kms");
        System.out.println("2 - 10 Utilizadores que mais utilizaram o sistema");
        System.out.println("0 - Sair");
    }
}

package View;

import Controller.Controller;
import Models.Produto;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * Classe para outputs gerais
 */
public interface View {

    /**
     * Vários outputs gerais
     * @param o opcao para saber que output mostar
     */
    static void show(MenuOption o) {
        switch (o) {
            case Nome -> System.out.println("Nome: ");
            case Code -> System.out.println("Código: ");
            case X -> System.out.println("Coordenada x: ");
            case Y -> System.out.println("Coordenada y: ");
            case Email -> System.out.println("Email: ");
            case Pass -> System.out.println("Password: ");
            case Raio -> System.out.println("Raio de ação: ");
            case Tax -> System.out.println("Preço por km: ");
            case Cap -> System.out.println("Capacidade máxima por transporte: ");
            case NIF -> System.out.println("NIF: ");
            case Filas -> System.out.println("Esta loja permite saber o tamanho das filas? (s/n)");
            case AceitaEncomendaMedica -> System.out.println("Pode transportar encomendas médicas? (s/n)");
            case Disponibilidade -> System.out.println("Está preparado para aceitar novas encomendas? (s/n)");
            case Registado -> System.out.println("Registado com sucesso.");

            case Max -> System.out.println("Número máximo de nomes a listar: ");
            case Cod_Agente -> System.out.println("Código do agente: ");
            case Cod_Emp -> System.out.println("Código da empresa transportadora: ");
            case Cod_Loja -> System.out.println("\nCódigo da loja desejada: ");
            case Classif -> System.out.println("Classificação (0-10): ");
            case Enc_Prontas -> System.out.println("\nEncomendas prontas a entregar: ");
            case Enc_Pendentes -> System.out.println("\nEncomendas pendentes: ");
            case Enc_Entregues -> System.out.println("\nEncomendas já entregues: ");
            case Nada -> System.out.println("Nenhuma!");
            case Enter -> System.out.println("\nCarregue no Enter para continuar.");
            case CarrinhoCompras ->System.out.println("\nCarrinho de compras");
            case TiposLoja -> System.out.println("Tipos de Loja: \n\nmercearia \ninformatica\nrestaurante\nvestuario\n\nQual o tipo de loja?\n\n");

            case Rem_Prod -> System.out.println("Código do produto a remover do carrinho de compras: ");
            case Add_Prod -> System.out.println("Código do produto a adicionar ao carrinho de compras: ");
            case Inv_Prod -> System.out.println("Produto não existe.");
            case No_Enc -> System.out.println("O utilizador não está à espera de nenhuma encomenda.");
            case EncComing -> System.out.println("Já tem uma encomenda a caminho!");
        }
    }

    /**
     * Imprime uma lista de Strings.
     *
     * @param l Lista de Strings a imprimir
     */
    static void showList(List<String> l) {
        for (String o : l) {
            System.out.println(o);
        }
    }

    /**
     * Limpa o ecrã.
     */
    static void clrscr() {
        try {
            if (System.getProperty("os.name").contains("Windows"))
                Runtime.getRuntime().exec("cmd / c cls");
            else
                Runtime.getRuntime().exec("clear");
        } catch (final Exception ignored) {
        }
    }

    /**
     * output do Orçamento criado por uma empresa
     * @param orcamento Preço do transporte
     * @param tempo Tempo previsto para entrega
     * @return retorna a resposta do user (sim caso aceite orçamento ou nao caso contrario)
     */
    static Boolean pedido_orcamento(double orcamento, double tempo) {
        System.out.println("Custo do transporte: " + orcamento);
        System.out.println("Tempo de espera: " + tempo);
        System.out.println("-> O utilizador aceita este orçamento? (s/n)");

        return Controller.yes_or_no();
    }

    /**
     * Imprime o estado do agente (pode ou nao recolher encomendas)
     * @param e boolean que determina se pode ou nao recolher encomendas
     */
    static void aceitaEncs(boolean e) {
        if (e) System.out.println("\n[Estado: Pronto a recolher encomendas.]");
        else System.out.println("\n[Estado: Não recolhe mais encomendas de momento.]");
    }

    /**
     * Retorna uma mensagem de boas vindas com um determinado nome
     *
     * @param name String do nome completo
     */
    static void welcome(String name) {
        System.out.println("\nBem vindo " + name + "!");
    }

    /**
     * Imprime o nome de uma loja e a lista de encomendas a ela associada
     *
     * @param loja String com o nome da loja
     * @param encs Lista de Strings com os códigos das encomendas
     */
    static void showLoja(String loja, List<String> encs) {
        System.out.println("\n" + loja);
        View.showList(encs);
    }

    /**
     * Imprime o codigo de uma encomenda
     *
     * @param s String do codigo da encomenda
     */
    static void showEnc(String s) {
        System.out.println("[Encomenda " + s + "]");
    }

    /**
     * Output da faturação
     * @param fat faturação
     */
    static void showFat(double fat) {
        System.out.println("Faturação total: " + fat);
    }

    /**
     * Output da classificacao de um agente
     * @param classif classificacao de um agente
     */
    static void showClassif(double classif) {
        System.out.println("Classificação: " + classif);
    }

    /**
     * Imprime um dado Mapa no buffer de saida do progama.
     *
     * @param input     Mapa a ser imprimido.
     * @param keyshow   Função que processa cada chave do mapa.
     * @param valueshow Função que processa cada valor do mapa.
     * @param <K>       Tipo de cada chave do mapa.
     * @param <V>       Tipo de cada valor do mapa.
     */
    static <K, V> void showMap(Map<K, V> input, Function<K, String> keyshow, Function<V, String> valueshow) {
        input.forEach((key, value) -> System.out.println(keyshow.apply(key) + valueshow.apply(value)));
    }

    /**
     * Imprime um string
     *
     * @param s String a imprimir
     */
    static void simpleshow(String s) {
        System.out.println(s);
    }

    /**
     * Varias opcções para outputs
     */
    enum MenuOption {
        Email, Pass, Code, Nome, X, Y, Raio, Tax, Cap, NIF,
        Filas, Registado, Max, Enc_Prontas, Enc_Pendentes, Enc_Entregues, Nada, Disponibilidade,
        Cod_Emp, Cod_Agente, Cod_Loja, Classif, Enter, AceitaEncomendaMedica, TiposLoja, CarrinhoCompras,
        Rem_Prod, Add_Prod, Inv_Prod, No_Enc, EncComing
    }

}

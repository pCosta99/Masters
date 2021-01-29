package View;

import Model.RegistoEncomenda;
import Model.RegistosEncomenda;

import java.util.Scanner;
import java.util.List;
import java.util.ArrayList;
import java.text.DecimalFormat;
import java.util.AbstractMap.SimpleEntry;

public class View {

    // Welcome Messages.
    
    public static void welcomeMessage() {
        System.out.println("\nBem Vindo ao TrazAqui!\n");
        System.out.println("Premir 1 -> Login.\n");
        System.out.println("Premir 2 -> Registo.\n");
        System.out.println("Premir 3 -> Load.\n");
        System.out.println("Premir 4 -> Save.\n");
    }

    public static void welcomeWithUserNameAndName(String userName, String nome) {
        System.out.println("Bem Vindo, " + nome + ". " + "UserName: " + userName);
    }

    public static void welcomeUser() {
        System.out.println("Bem Vindo Caro Utilizador!");
    }

    
    public static void welcomeVoluntario() {
        System.out.println("Bem Vindo Caro Voluntário!");
    }

    
    public static void welcomeTransportadora() {
        System.out.println("Bem Vindo Transportadora!");
    }

    
    public static void welcomeLoja() {
        System.out.println("Bem Vindo Loja!");
    }

    // Insert Messages.

    
    public static void insertUserName() {
        System.out.println("Insira o Seu Nome de Utilizador.\n");
    }

    
    public static void insertName() {
        System.out.println("Insira o Seu Nome.\n");
    }

    
    public static void insertEmail() {
        System.out.println("Insira o Seu Email.\n");
    }

    
    public static void insertPassWord() {
        System.out.println("Insira a Sua PalavraPasse.\n");
    }

    
    public static void insertGPSX() {
        System.out.println("Insira a Sua Localização X.\n");
    }

    
    public static void insertGPSY() {
        System.out.println("Insira a Sua Localização Y.\n");
    }

    
    public static void insertRaio() {
        System.out.println("Insira o Raio de Ação.\n");
    }

    
    public static void insertNIF() {
        System.out.println("Insira o NIF da Empresa.\n");
    }

    public static void insertPrecoKM() {
        System.out.println("Insira o Preço Por KM.\n");
    }

    public static void insertNewPassWord() {
        System.out.println("Insira a PalavraPasse Nova.\n");
    }

    public static void insertNewEmail() {
        System.out.println("Insira o Novo Email.\n");
    }

    public static void insertNewX() {
        System.out.println("Insira a Nova Coordenada X.\n");
    }

    public static void insertNewY() {
        System.out.println("Insira a Nova Coordenada Y.\n");
    }

    public static void insertNewName() {
        System.out.println("Insira o Novo Nome.\n");
    }

    public static void insertAceitaFila() {
        System.out.println("Pretende Que a Sua Loja Disponibilize Informação Sobre as Filas de Espera?\n");
        System.out.println("Prima 1: Sim.\n");
        System.out.println("Prima 2: Não.\n");
    }

    public static void insertCodigoEncomenda() {
        System.out.println("Insira o Código de Encomenda.\n");
    }

    public static void insertCodigoLoja() {
        System.out.println("Insira o Código de Loja.\n");
    }

    public static void insertPesoEncomenda() {
        System.out.println("Insira o Peso da Encomenda.\n");
    }

    public static void insertTransportadoraVoluntario() {
        System.out.println("Insira o Código do Voluntário/Transportadora.\n");
    }

    public static void mensagemEncomenda() {
        System.out.println("A Sua Encomenda Foi Criada Com Sucesso!\n");
        System.out.println("Relativamente aos Produtos Encomendados:\n");
    }

    public static void insertCodigoProduto() {
        System.out.println("Insira o Código do Produto.\n");
    }

    public static void insertDescricaoProduto() {
        System.out.println("Insira a Descrição do Produto.\n");
    }

    public static void insertQuantidadeProduto() {
        System.out.println("Insira a Quantidade Comprada.\n");
    }

    public static void insertPrecoProduto() {
        System.out.println("Insira o Preço Unitário.\n");
    }

    public static void produtoAdicionado() {
        System.out.println("O Produto Foi Adicionado à Encomenda.\n");
        System.out.println("Deseja Adicionar Mais Produtos?\n");
        System.out.println("Prima 1: Inserir Mais Produtos.\n");
        System.out.println("Prima 2: Não Inserir Mais Produtos.\n");
    }

    // Error Messages.
    
    public static void invalidUserName() {
        System.out.println("UserName Inválido.\n");
        System.out.println("O UserName Deve Respeitar a Seguinte Sintaxe:\n");
        System.out.println("Utilizador     -> uNumeroNumeroNumero...\n");
        System.out.println("Voluntário     -> vNumeroNumeroNumero...\n");
        System.out.println("Transportadora -> tNumeroNumeroNumero...\n");
        System.out.println("Loja           -> lNumeroNumeroNumero...\n");
        System.out.println("Insira o UserName Novamente.\n");
    }

    
    public static void userNameNotFound() {
        System.out.println("O UserName Não Existe.\n");
        System.out.println("Insira Novamente.\n");
    }

    
    public static void wrongPassWord() {
        System.out.println("A Palavra Passe Inserida Não Está Correta. Insira de Novo.\n");
    }

    
    public static void wrongOption() {
        System.out.println("A Opção Inserida Não é Valida. Insira 1 ou 2.\n");
    }

    
    public static void userNameAlreadyExists() {
        System.out.println("O UserName Já Existe.\n");
        System.out.println("Insira Novamente.\n");
    }

    
    public static void deveSerDe1a4() {
        System.out.println("A Opção Inserida Não é Válida. Insira um Número de 1 a 4.\n");
    }

    public static void deveSerDe1a20() {
        System.out.println("A Opção Inserida Não é Válida. Insira uma Classificação de 0 a 20.\n");
    }

    public static void deveSerDe1a2() {
        System.out.println("A Opção Inserida Não é Válida. Insira um Número de 1 a 2.\n");
    }

    public static void deveSerDe1a3() {
        System.out.println("A Opção Inserida Não é Válida. Insira um Número de 1 a 3.\n");
    }

    public static void deveSerDe1a1() {
        System.out.println("A Opção Inserida Não é Válida. Insira um Número de 1 a 1.\n");
    }

    // Successful Messages.

    public static void trocaComSucessoPw(String pw1, String pw2) {
        System.out.println("A Operação Foi Bem Sucedida!\n");
        System.out.println("PalavraPasse Antiga: " + pw1 + "\n");
        System.out.println("PalavraPasse Nova: " + pw2 + "\n");
    }

    public static void trocaComSucessoNome(String nomeAntigo, String nomeNovo) {
        System.out.println("A Operação Foi Bem Sucedida!\n");
        System.out.println("Nome Antigo: " + nomeAntigo + "\n");
        System.out.println("Nome Novo: " + nomeNovo + "\n");
    }

    public static void trocaComSucessoGPS(double xAntigo, double xNovo, double yAntigo, double yNovo) {
        System.out.println("A Operação Foi Bem Sucedida!\n");
        System.out.println("Coordenada X Antiga: " + xAntigo + "\n");
        System.out.println("Coordenada X Nova: " + xNovo + "\n");
        System.out.println("Coordenada Y Antiga: " + yAntigo + "\n");
        System.out.println("Coordenada y Nova: " + yNovo + "\n");
    }

    public static void trocaComSucessoEmail(String emailAntigo, String emailNovo) {
        System.out.println("A Operação Foi Bem Sucedida!\n");
        System.out.println("Email Antigo: " + emailAntigo + "\n");
        System.out.println("Email Novo: " + emailNovo + "\n");
    }

    public static void lojaNaoExiste() {
        System.out.println("O Código de Loja Inserido Não Existe.\n");
        System.out.println("Insira o Código de Loja Novamente.\n");
    }

    public static void voluntarioTransportadoraNaoExiste() {
        System.out.println("O Código Inserido Não Existe.\n");
        System.out.println("Insira o Código Novamente.\n");
    }

    // Other Messages.

    public static void registoComSucesso() {
        System.out.println("Foi Registado Com Sucesso!");
    }
    
    public static void printSpace() {
        System.out.println();
    }

    public static void quemFala() {
        System.out.println("Utilizador, Voluntário, Transportadora ou Loja?\n");
        System.out.println("Utilizador     -> Prima 1.\n");
        System.out.println("Voluntário     -> Prima 2.\n");
        System.out.println("Transportadora -> Prima 3.\n");
        System.out.println("Loja           -> Prima 4.\n");
    }

    public static void desejaContinuar() {
        System.out.println("Deseja Continuar?\n");
        System.out.println("Prima 1 -> Continuar.\n");
        System.out.println("Prima 2 -> Sair.\n");
    }

    public static void criarEncomenda() {
        System.out.println("Crie a Sua Encomenda!\n");
    }

    // Queries Messages.

    public static void queriesUser() {
        System.out.println("O Que Pretende Fazer?\n");
        System.out.println("Prima 1 -> Pedir Encomenda.\n");
        System.out.println("Prima 2 -> Alterar os Dados de Utilizador.\n");
    }

    public static void changeUserPersonalData() {
        System.out.println("O Que Pretende Alterar?\n");
        System.out.println("Prima 1 -> Alterar PassWord.\n");
        System.out.println("Prima 2 -> Alterar Nome.\n");
        System.out.println("Prima 3 -> Alterar GPS.\n");
        System.out.println("Prima 4 -> Alterar Email.\n");
    }

    public static void relativamenteAoTransporte() {
        System.out.println("Relativamente à Entrega da Encomenda.\n");
        System.out.println("Prima 1 -> Receber o Código de um Voluntário/Transportadora Para Efetuar o Transporte.\n");
        System.out.println("Prima 2 -> Ver Todos os Registos de Um Voluntário/Transportadora em Específico.\n");
    }

    public static void printRegistos(RegistosEncomenda r) {
        System.out.println(r.toString());
    }

    public static void quemVaiTransportar() {
        System.out.println("Prima 1 -> Transporte Via Voluntário. (Tempo de Entrega Maior, Custo Zero)\n");
        System.out.println("Prima 2 -> Transporte Via Transportadora. (Tempo de Entrega Menor, Custo Associado)\n");
    }

    public static void printQuemTransporta(String quemTransportou, int opcao) {
        if (opcao == 1) {
            System.out.println("O Sistema TrazAqui Atribuiu o Voluntário: " + quemTransportou);
        }
        else {
            System.out.println("O Sistema TrazAqui Sugere a Transportadora: " + quemTransportou);
        }
    }

    public static void printCustoViagem(double distancia, double custoTotal, String transportadora) {
        System.out.printf("A Distância Entre o Utilizador e Transportadora é: %.0fKM e o Custo Total da Entrega da Encomenda é: %.0f€\n",distancia,custoTotal);
        System.out.println();
        System.out.println("Aceita Que a Sua Encomenda Seja Transportada Pela Transportadora " + transportadora + "?\n");
        System.out.println("Prima 1 -> Sim.\n");
        System.out.println("Prima 2 -> Não. (Será Atribuída Outra Transportadora)\n");
    }

    public static void aleatoriedadeViagem(int i) {
        if (i == 0) {
            System.out.println("O Tempo Estava Ótimo Hoje. Além Disso, Circulava-se Pelas Estradas Sem Trânsito.\n");
        }
        if (i == 1) {
            System.out.println("O Tempo Estava Ótimo Hoje, No Entanto a Cor dos Sinais de Trânsito Não Estavam a Seu Favor.\n");
        }
        if (i == 2) {
            System.out.println("Choveu de Forma Intensa Hoje. Com Chuva, Modere a Velocidade. Pelo Menos, Não Havia Trânsito.\n");
        }
        if (i == 3) {
            System.out.println("Além De Estar a Chover Intensamente, o Trânsito Não Facilitou a Entrega da Encomenda.\n");
        }
        if (i == 4) {
            System.out.println("A Neve Inesperada na Cidade Provocou um Atraso Abismal na Entrega da Encomenda.\n");
        }
    }

    public static void printTempoViagem(double tempoViagem) {
        System.out.printf("A Entrega da Sua Encomenda Demorou Cerca de %.0f Minutos.\n",tempoViagem);
    }

    public static void printTopUsers(List<SimpleEntry<String,Integer>> aux) {
        int i = 1;
        System.out.println("Quais Os Utilizadores Que Utilizam Mais o Sistema?\n");
        for (SimpleEntry<String,Integer> s : aux) {
            System.out.println(i + "º -> Utilizador: " + s.getKey() + " Pediu " + s.getValue() + " Encomenda(s).");
            i++;
        }
        System.out.println("\n");
    }

    public static void printTopCarriers(List<SimpleEntry<String,Double>> aux) {
        int i = 1;
        DecimalFormat df = new DecimalFormat("#.##");
        System.out.println("Quais as Empresas Transportadoras a Utilizar Mais o Sistema? (Em KMs Percorridos)\n");
        for (SimpleEntry<String,Double> s : aux) {
            System.out.println(i + "º -> Empresa Transportadora: " + s.getKey() + " Percorreu " + df.format(s.getValue()) + " KMs");
            i++;
        }
        System.out.println("\n");
    }

    public static void printClassificar() {
        System.out.println("Finalmente, Atríbua uma Classificação ao Responsável Pela Entrega da Sua Encomenda.\n");
        System.out.println("Insira uma Classificação de 0 a 20.\n");
    }

    public static void printClassificacaoFinalVoluntario(String voluntario, double classificacaoMedia) {
        System.out.printf("A Sua Classificação Foi Atribuída. Neste Momento, a Classificação Média do Voluntário %s é %.0f.\n",voluntario,classificacaoMedia);
    }

    public static void printEncomendaPronta(String qualLoja, List<Integer> encomendaPronta) {
        System.out.println("A Loja " + qualLoja + " Indica Que a Sua Encomenda Está Pronta.\n");
        System.out.println("Havia Cerca de " + (encomendaPronta.get(0)) + " Encomenda(s) Na Fila de Espera.\n");
        System.out.println("O Tempo Médio de Processamento de Cada Encomenda à Frente da Sua Foi de " + encomendaPronta.get(1) + " Minuto(s).\n");
        System.out.println("Portanto, o Tempo Total De Espera na Loja Foi de " + encomendaPronta.get(2) + " Minuto(s).\n");
    }

    public static void printClassificacaoFinalTransportadora(String transportadora, double classificacaoMedia) {
        System.out.printf("A Sua Classificação Foi Atribuída. Neste Momento, a Classificação Média da Transportadora %s é %.0f.\n",transportadora,classificacaoMedia);
    }

    public static void printRegistosDeAlguem(List<RegistoEncomenda> registos) {
        if (registos.size() == 0) {
            System.out.println("Neste Momento Ainda Não Existe Qualquer Registo Associado a Esse Voluntário/Transportadora.\n");
        }
        else {
            System.out.println(registos.toString());
        }
    }

    public static void voluntariosTransportadorasQueries() {
        System.out.println("Prima 1 -> Mudar Disponibilidade Relativamente à Entrega de Encomendas.\n");
    }

    public static void lojasQueries() {
        System.out.println("Prima 1 -> Mudar Disponibilidade Relativamente à Entrega de Encomendas.\n");
    }

    public static void changeAceita() {
        System.out.println("Prima 1 -> Aceita Encomenda.\n");
        System.out.println("Prima 2 -> Não Aceita Encomenda.\n");
    }

    public static void changeAceitaComSucesso() {
        System.out.println("As Suas Informações Foram Devidamente Alteradas.\n");
    }

    public static void changeAceitaFila() {
        System.out.println("Prima 1 -> Aceita Fila.\n");
        System.out.println("Prima 2 -> Não Aceita Fila.\n");
    }

}

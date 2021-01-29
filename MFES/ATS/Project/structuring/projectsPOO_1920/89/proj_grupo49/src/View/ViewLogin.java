package View;

import Model.Transportadora;
import Model.Voluntario;

public class ViewLogin {

        public void menuLogin(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> SIGN IN                                                                               |");
        System.out.println("|2  -> SIGN UP                                                                               |");
        System.out.println("|0  -> Exit Program                                                                          |");
        System.out.println("|____________________________________________________________________________________________|");
    }

    public void printBarraN(){
        System.out.println();
    }
    public void flush(){
        System.out.println("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
    }

    public void pressioneEnter(){
        System.out.println("\nPressione enter para continuar");
    }

    public void printError(){
        System.out.println("\nNão escreveu nenhuma das duas opções válidas!");
        System.out.print("Por favor tente outra vez: \n");
    }

    public void printInv(){
        System.out.println("\nValor Inválido!");
        System.out.print("Por favor tente outra vez: \n");
    }


    public void LoginDeny(){
        System.out.println("Password ou ou ID incorreto!");
    }

    public void loginAccep(){
        System.out.println("Login aceite!");
    }

    public void siginD(){
        System.out.println("O ID que escolheu já existe ou não é válido! Por favor escolha outra opção");
    }

    public void siginA(){
        System.out.println("Conta criada com sucesso!");
    }


    public void Op(){
        System.out.println("Escolha uma opção:");
    }

    public void ID(){
            System.out.println("Insira o seu ID:");
    }

    public void IDT(){System.out.println("Insira o seu ID começado por t:");
    }

    public void IDV(){System.out.println("Insira o seu ID começado por v:");
    }

    public void pass(){
            System.out.println("Insira a palavra-passe:");
    }

    public void raio(){
            System.out.println("Insira o seu raio de ação em Km:");
    }

    public void addMedico(){
        System.out.println("Tem aptidões para transporte de encomendas médicas?");
        System.out.println("1 -> Sim");
        System.out.println("2 -> Não");
        System.out.println("Escolha a sua opção:");
    }

    public void printDadosVol(Voluntario v){
        System.out.println("\nOs seus dados atuais: ");
        System.out.println("Nome:" + v.getNome());
        System.out.println("Codigo:"  + v.getCod());
        System.out.println("Raio de ação:" + v.getRaio());
        System.out.println("Médico: "+ v.aceitoTransporteMedicamentos());
        System.out.println("Classificacao: " + v.getClGeral());
        System.out.println("Localização:\n    " + v.getGPS().toString() + "\n");
    }

    public void printDadosTrans(Transportadora t){
        System.out.println("\nOs seus dados atuais: ");
        System.out.println("Nome:" + t.getNome());
        System.out.println("Codigo:"  + t.getCod());
        System.out.println("Medico: " + t.aceitoTransporteMedicamentos());
        System.out.println("Raio de ação:" + t.getRaio());
        System.out.println("Taxa:" + t.getTaxa());
        System.out.println("Taxa de Peso:" + t.getTaxaPeso());
        System.out.println("Kms percorridos: " + t.getKms());
        System.out.println("A sua Classificação: " + t.getClGeral());
        System.out.println("Localização:\n    " + t.getGPS().toString() + "\n");
    }

    public void GPS(){
            System.out.println("Insira a sua Latidude e Longitude:");
    }

    public void nome(){
            System.out.println("Insira o seu nome:");
    }

    public void nif(){
            System.out.println("Insira o seu nif:");
    }
    public void pesol(){
        System.out.println("Insira o peso limite dos carregamentos:");
    }
    public void taxa(){
        System.out.println("Insira a taxa sobre encomenda:");
    }

    public void taxapeso(){
        System.out.println("Insira a taxa sobre o peso:");
    }

    public void continuar(){
        System.out.println("Deseja prosseguir (1), Caso contrário (0):");
    }

    public void filaEspera(){
        System.out.println("É uma loja com fila de espera ? Sim(1) Não(2)");
    }
}

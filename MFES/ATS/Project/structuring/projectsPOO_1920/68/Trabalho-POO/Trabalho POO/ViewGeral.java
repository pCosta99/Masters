import java.io.Serializable;
import java.util.List;

class ViewGeral implements Serializable {

    public static void menuInicial(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                      Bem Vindo à aplicação TrazAqui                       |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|         1. Carregar Logs                                                  |");
        System.out.println("|         2. Carregar Estado                                                |");
        System.out.println("|         0. Sair                                                           |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("     Digite a sua opção");
        System.out.print("\n\n");
    }




    public void bemVindo(){

        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                      Bem Vindo à aplicação TrazAqui                       |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|         1. Login                                                          |");
        System.out.println("|         2. Registar                                                       |");
        System.out.println("|         3. Guardar estado                                                 |");
        System.out.println("|         4. Top 10 utilizadores (em número de encomendas solicitadas)      |");
        System.out.println("|         5. Top 10 empresas (em km percorridos)                            |");
        System.out.println("|         0. Sair                                                           |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("     Digite a sua opção");
        System.out.print("\n\n");
    }

    public void login(){

        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                   Login                                   |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        -> Digite o seu username                                           |");
        System.out.println("|        -> Digite a sua password                                           |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("    q: voltar ao menu principal");
        System.out.print("\n\n");

    }

    public void RegistoEscolher(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                  Registo                                  |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        1. Registar Utilizador                                             |");
        System.out.println("|        2. Registar Voluntário                                             |");
        System.out.println("|        3. Registar Transportadora                                         |");
        System.out.println("|        0. Voltar para menu principal                                      |");
        System.out.println("|___________________________________________________________________________|");
        System.out.print("\n\n");
    }

    public void RegistoUtilizador(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                  Registo                                  |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        -> Digite o seu nome                                               |");
        System.out.println("|        -> Digite um username                                              |");
        System.out.println("|        -> Digite uma password                                             |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("    q: voltar ao menu principal");
        System.out.print("\n\n");
    }

    public void RegistoVoluntario(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                  Registo                                  |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        -> Digite um username                                              |");
        System.out.println("|        -> Digite uma password                                             |");
        System.out.println("|        -> Digite o seu nome                                               |");
        System.out.println("|        -> Digite o raio no qual aceita fazer entregas (em Km)             |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("    q: voltar ao menu principal");
        System.out.print("\n\n");
    }

    public void RegistoTransportadora(){
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|                                  Registo                                  |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("|                                                                           |");
        System.out.println("|        -> Digite um username                                              |");
        System.out.println("|        -> Digite uma password                                             |");
        System.out.println("|        -> Digite o nome da empresa                                        |");
        System.out.println("|        -> Digite o NIF da empresa                                         |");
        System.out.println("|        -> Digite o raio no qual aceita fazer entregas (em Km)             |");
        System.out.println("|        -> O preço cobrado por Km                                          |");
        System.out.println("|___________________________________________________________________________|");
        System.out.println("    q: voltar ao menu principal");
        System.out.print("\n\n");
    }

    public static void saida(){
        System.out.print("\n");
        System.out.print("\n   ________________________________________________________________________________________________");
        System.out.print("\n  |                                                                                                |");
        System.out.print("\n  |                                  Volte em breve ( ＾◡＾)っ``                                    |");
        System.out.print("\n  |                                                                                                |");
        System.out.print("\n  |                                Obrigado por utilizar TrazAqui                                  |");
        System.out.print("\n  |________________________________________________________________________________________________|");
        System.out.print("\n\n\n");
    }



    public static void PrintMensagem(String msg){
        System.out.println("\n" + msg);
    }

    public void TopUtilizadores(List<Utilizador> top_utilizadores) {
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|               Top Utilizadores que mais encomendas pediram                |");
        System.out.println("|___________________________________________________________________________|");

        for (int i = 0; i<10; i++){
            Utilizador u = top_utilizadores.get(i);
            String cod= u.getCodigo();
            System.out.print("      Código: "+ cod);
            int nrEspacos=6-cod.length(); while(nrEspacos>0){System.out.print(" "); nrEspacos--;}
            System.out.println("      Nome: "+ u.getNome());
            System.out.println("      Encomendas: "+ u.numeroEncomendas());
            System.out.println();
        }        System.out.println("    q: voltar ao menu principal");
        System.out.print("\n\n");
    }

    public void TopEmpresas(List<Transportadora> top_empresas) {
        System.out.println("____________________________________________________________________________");
        System.out.println("|                                                                           |");
        System.out.println("|               Top Empresas que mais km percorreram                        |");
        System.out.println("|___________________________________________________________________________|");
        for (Transportadora t: top_empresas){
            String cod= t.getCodigo();
            System.out.print("      Código: "+ cod);
            int nrEspacos=10-cod.length(); while(nrEspacos>0){System.out.print(" "); nrEspacos--;}
            System.out.println("      Nome: "+ t.getNome());
            System.out.println();
        }        System.out.println("    q: voltar ao menu principal");
        System.out.print("\n\n");
    }

}


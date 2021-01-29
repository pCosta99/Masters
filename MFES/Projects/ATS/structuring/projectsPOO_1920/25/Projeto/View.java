
public class View 
{
     public static void limpaEcra() {
        System.out.print("\u001B[2J");
        System.out.flush();
    }
    
    public static void menu(){
        System.out.println(" ##################################");
        System.out.println(" #      #### TrazAqui! ####       #");
        System.out.println(" ##################################");
        System.out.println("-> 1        Registar                                     ");
        System.out.println("-> 2        Login                                        ");
        System.out.println("-> 3        Bónus                                        ");
        System.out.println("-> 0        Sair                                         ");
    }
  
    public static void menuBonus(){
        System.out.println(" ##################################");
        System.out.println(" #      #### TrazAqui! ####       #");
        System.out.println(" ##################################");
        System.out.println("-> 1        Top 10 Utilizadores que mais usaram o sistema   ");
        System.out.println("-> 2        Top 10 Transportadoras que mais usaram o sistema");
        System.out.println("-> 0        Voltar                                          ");
    } 
    
    public static void menuRegistos(){
        System.out.println(" ##################################");
        System.out.println(" #      #### TrazAqui! ####       #");
        System.out.println(" ##################################");
        System.out.println("-> 1        Registar Utilizador               ");
        System.out.println("-> 2        Registar Voluntario               ");
        System.out.println("-> 3        Registar Empresa Transportadora   ");
        System.out.println("-> 4        Registar Loja                     ");
        System.out.println("-> 0        Voltar                            ");
    }
    
    public static void menuLogin(){
        System.out.println(" ##################################");
        System.out.println(" #      #### TrazAqui! ####       #");
        System.out.println(" ##################################");
        System.out.println("-> 1        Login Utilizador               ");
        System.out.println("-> 2        Login Voluntario               ");
        System.out.println("-> 3        Login Empresa Transportadora   ");
        System.out.println("-> 4        Login Loja                     ");
        System.out.println("-> 0        Voltar                         ");
    }
    
    public static void menuLoginUtilizador(){
        System.out.println(" ##################################");
        System.out.println(" #      #### TrazAqui! ####       #");
        System.out.println(" ##################################");
        System.out.println("-> 1        Solicitar Entrega                                                       ");
        System.out.println("-> 2        Verificar entregas recebidas num periodo de tempo                       ");
        System.out.println("-> 3        Verificar todos os registos de encomendas recebidas através da trazAqui ");
        System.out.println("-> 4        Pedir nova encomenda a uma loja                                         ");
        System.out.println("-> 0        Voltar                                                                  ");
    }
    
    public static void menuLoginVoluntario(){
        System.out.println(" ##################################");
        System.out.println(" #      #### TrazAqui! ####       #");
        System.out.println(" ##################################");
        System.out.println("-> 1        Verificar entregas feitas num periodo de tempo                              ");
        System.out.println("-> 2        Alterar disponibilidade                                                     ");
        System.out.println("-> 3        Verificar todos os registos de encomendas transportadas através da trazAqui ");
        System.out.println("-> 4        Verificar a sua classificação média                                         ");
        System.out.println("-> 0        Voltar                                                                      ");
    }
    
    public static void menuLoginEmpresas(){
        System.out.println(" ##################################");
        System.out.println(" #      #### TrazAqui! ####       #");
        System.out.println(" ##################################");
        System.out.println("-> 1        Verificar entregas feitas num periodo de tempo                              ");
        System.out.println("-> 2        Verificar todos os registos de encomendas transportadas através da trazAqui ");
        System.out.println("-> 3        Verificar a sua classificação média                                         ");
        System.out.println("-> 4        Total faturado num periodo de tempo                                         ");
        System.out.println("-> 0        Voltar                                                                      ");
    }
    
    public static void menuLoginLoja(){
        System.out.println(" ##################################");
        System.out.println(" #      #### TrazAqui! ####       #");
        System.out.println(" ##################################");
        System.out.println("-> 1        Número de Pessoas na fila de Espera                                        ");
        System.out.println("-> 2        Verificar se existe uma encomenda de um utilizador pronta para ser entregue");
        System.out.println("-> 0        Voltar                                                                     ");
    }
    
    public static void codLojaNaoEncontrado(){
        System.out.println("Loja não está registada ou o código introduzido está errado, porfavor verifique o código da loja e volte a tentar mais tarde.");
    }

    public static void LojaNaoEncontrada(){
        System.out.println("Loja não está registada ou as credênciais introduzidas estão erradas, porfavor verifique as credenciais da loja e volte a tentar mais tarde.");
    }
    
    public static void TransportadoraNaoEncontrada(){
        System.out.println("Transportadora não está registada ou as credênciais introduzidas estão erradas, porfavor verifique as credenciais da loja e volte a tentar mais tarde.");
    }   

    public static void utilizadorNaoEncontrado(){
        System.out.println("Utilizador não está registado ou credenciais estão erradas, porfavor verifique as suas credenciais e volte a tentar mais tarde.");
    }

    public static void voluntarioNaoEncontrado(){
        System.out.println("Voluntario não está registado ou credenciais estão erradas, porfavor verifique as suas credenciais e volte a tentar mais tarde.");
    }

    public static void encomendaNaoEncontrada(){
        System.out.println("Encomenda não está registada ou o código introduzido está errado, porfavor verifique o código da encomenda e volte a tentar mais tarde.");
    }

    public static void pedirTempoAntes(){
        System.out.println("Insira a primeira data (da forma ANO-MES-DIA HORA:MINUTOS)");
    }

    public static void pedirTempoDepois(){
        System.out.println("Insira a segunda data (da forma ANO-MES-DIA HORA:MINUTOS)");
    }

    public static void pedirLoja(){
        System.out.println("Loja onde vai comprar a sua encomenda : ");
    }

    public static void pedirEncomenda(){
        System.out.println("Qual destas encomendas deseja que seja transportada?");
    }
    
    public static void pedirNome(){
        System.out.println("Nome : ");
    }
    
    public static void pedirCodigo(){
        System.out.println("Codigo : ");
    }
    
    public static void pedeClassificaçao(){
        System.out.println("Classifique a qualidade do transporte (de 0 a 10 porfavor)");
    }

    public static void pedirLocalizaçaoX(){
        System.out.println("Coordenada X: ");
    }
    
    public static void pedirLocalizaçaoY(){
        System.out.println("Coordenada Y: ");
    }
    
    public static void pedirEmail(){
        System.out.println("Email : ");
    }
    
    public static void pedirPass(){
        System.out.println("Password : ");
    }
    
    public static void pedirRaio(){
        System.out.println("Raio de ação : ");
    }
    
    public static void pedirAptoMed(){
        System.out.println("Apto Entregas médicas (coloque true ou false) :  ");
    }
    
    public static void pedirNif(){
        System.out.println("NIF: ");
    }
    
    public static void pedirPrecoKm(){
        System.out.println("Preco por Km: ");
    }
    
    public static void pedirMaxEnc(){
        System.out.println("Maximo de Encomendas:  ");
    }
    
    public static void fecharPrograma(){
        System.out.println("A fechar programa.");
    }

    public static void opcaoInvalida(){
        System.out.println("Opcao Invalida. ");
    }
  
    public static void estadoGravado(){
        System.out.println("Estado Gravado.");
    }
}

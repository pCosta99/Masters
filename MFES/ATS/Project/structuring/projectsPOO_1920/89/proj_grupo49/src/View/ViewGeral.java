package View;

import Model.*;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class ViewGeral {

    public void printHeader(){
        System.out.println("|============================================================================================|");
        System.out.println("||                                     Welcome to                                           ||");
        System.out.println("||                                      TRAZ AQUI !                                         ||");
        System.out.println("|============================================================================================|");
    }

    public void menuInicial(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Sign in / Sign up as:                                                                       |");
        System.out.println("|1  -> Loja                                                                                  |");
        System.out.println("|2  -> Utilizador                                                                            |");
        System.out.println("|3  -> Voluntário                                                                            |");
        System.out.println("|4  -> Transportadora                                                                        |");
        System.out.println("|5  -> Utilizadores mais frequentes                                                          |");
        System.out.println("|6  -> Transportadoras mais frequentes(km)                                                   |");
        System.out.println("|7  -> Gravar estado                                                                         |");
        System.out.println("|8  -> Ver todos os utilizadores                                                             |");
        System.out.println("|9  -> Ver todos as lojas                                                                    |");
        System.out.println("|10 -> Ver todos as transportadoras                                                          |");
        System.out.println("|11 -> Ver todos os voluntarios                                                              |");
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

    public void printExit(){
        System.out.println("\nObrigado por usar Traz Aqui!");
    }

    public void printError(){
        System.out.println("\nNão escreveu nenhuma das duas opções válidas!");
        System.out.print("Por favor tente outra vez: ");
    }

    public void maisUsados(Set<Utilizador> aux){
        Iterator<Utilizador> it = aux.iterator();
        if (aux.size()==0){
            System.out.println("Lista Vazia");
            return;
        }
        System.out.println("Utilizadores ordenados em ordem ao numero de compras!");
        if(aux.size()<10){
            int size = aux.size();
            while(it.hasNext() && size>0){
                Utilizador u = it.next();
                System.out.println(u.getCod());
                size--;
            }
        }
        else{
            int size=10;
            while(it.hasNext() && size>0) {
                Utilizador u = it.next();
                System.out.println(u.getCod());
                size--;
            }
        }
    }

    public void maisUsadosT(Set<Transportadora> aux){
        Iterator<Transportadora> it = aux.iterator();
        if (aux.size()==0){
            System.out.println("Lista Vazia");
            return;
        }
        System.out.println("Transportadoras ordenadas por o numero de kms feitos!");
        if(aux.size()<10){
            int size = aux.size();
            while(it.hasNext() && size>0){
                Transportadora t = it.next();
                System.out.println(t.getCod());
                size--;
            }
        }
        else{
            int size=10;
            while(it.hasNext() && size>0) {
                Transportadora t = it.next();
                System.out.println(t.getCod());
                size--;
            }
        }
    }


    public void load(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|LOAD:                                                                                       |");
        System.out.println("|1  -> Ler a partir de ficheiros                                                             |");
        System.out.println("|2  -> Ficheiros .dat guardados                                                              |");
        System.out.println("|0  -> Voltar ao Menu                                                                        |");
        System.out.println("|____________________________________________________________________________________________|");
    }

    public void readError(){
        System.out.println("Os ficheiros a load não encontrados! Por favor repita o processo!");
    }

    public void printErrorMessage(String e){
        System.out.println("Erro na gravação :"+  e);
    }

    public void leitura(){
        System.out.println("Leitura bem sucedida!");
    }

    public void printUsers(Utilizadores u){
        if(u.getUtilizadores().size()<=0){
            System.out.println("Nao há utilizadores inscritos na aplicação!");
        }
        else{
            for(Utilizador users : u.getUtilizadores().values()){
                System.out.println("Utilizador: " + users.getCod() + " Nome: " + users.getNome());
            }
        }
    }

    public void printVols(Voluntarios u){
        if(u.getVoluntarios().size()<=0){
            System.out.println("Nao há voluntarios inscritos na aplicação!");
        }
        else{
            for(Voluntario users : u.getVoluntarios().values()){
                System.out.println("Voluntario: " + users.getCod() + " Nome: " + users.getNome());
            }
        }
    }

    public void printTransp(Transportadoras u){
        if(u.getTransportadoras().size()<=0){
            System.out.println("Nao há transportadoras inscritos na aplicação!");
        }
        else{
            for(Transportadora users : u.getTransportadoras().values()){
                System.out.println("Transportadora: " + users.getCod() + " Nome: " + users.getNome());
            }
        }
    }

    public void printLojas(Lojas u){
        if(u.getLojas().size()<=0){
            System.out.println("Nao há lojas inscritos na aplicação!");
        }
        else{
            for(Loja users : u.getLojas().values()){
                System.out.println("Loja: " + users.getCod() + " Nome: " + users.getNome());
            }
        }
    }



}

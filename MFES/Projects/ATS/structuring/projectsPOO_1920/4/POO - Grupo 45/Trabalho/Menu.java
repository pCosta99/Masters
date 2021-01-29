
import java.io.*;
import java.util.*;

public class Menu{
    
    public void clear(){
        System.out.print("\f");
    }
    
    public void menuInicial(){
        this.clear();
        System.out.println("******* Traz Aqui *******\n");
        System.out.println("       1. Log In         ");
        System.out.println("       2. Registar        \n");
        System.out.println("       0. Sair          ");
    }

    public void logIn(){
        this.clear();
        System.out.println("******* Log In *******\n");
        System.out.println("     1. Utilizador    ");
        System.out.println("     2. Loja       ");
        System.out.println("     3. Transporte\n");
        System.out.println("     0. Sair");
    }

    public void registar(){
        this.clear();
        System.out.println("******* Registar *******\n");
        System.out.println("      1. Utilizador");
        System.out.println("      2. Loja");
        System.out.println("      3. Empresa");
        System.out.println("      4. Voluntário\n");
        System.out.println("      0. Sair");
    }

    public void utilizador(String user){
        this.clear();
        System.out.println("******* Menu " + user + " *******\n");
        System.out.println("     1. Efectuar Encomenda");
        System.out.println("     2. Encomendas Pendentes");
        System.out.println("     3. Aceitar Transporte");
        System.out.println("     4. Histórico Encomendas");
        System.out.println("     5. Classificar Entrega\n");
        System.out.println("     0. Sair");
    }
    
    public void loja(String user){
        this.clear();
        System.out.println("******* Menu " + user + " *******\n");
        System.out.println("     1. Encomendas Pendentes");
        System.out.println("     2. Encomendas à Espera de Entrega");
        System.out.println("     3. Histórico Encomendas");
        System.out.println("     0. Sair");
    }
    
    public void voluntario(String user){
        this.clear();
        System.out.println("******* Menu " + user + " *******\n");
        System.out.println("     1. Encomendas prontas para entrega");
        System.out.println("     2. Histórico Encomendas");
        System.out.println("     0. Sair");
    }
    
    public void empresa(String user){
        this.clear();
        System.out.println("******* Menu " + user + " *******\n");
        System.out.println("     1. Encomendas prontas para entrega");
        System.out.println("     2. Histórico encomendas");
        System.out.println("     3. Alterar custo");
        System.out.println("     0. Sair");
    }

}

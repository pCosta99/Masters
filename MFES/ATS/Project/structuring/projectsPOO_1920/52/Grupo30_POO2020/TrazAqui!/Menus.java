import java.util.*;
import java.io.*;

public class Menus implements Serializable {

    
    public static void logo(){
        System.out.println(" %%%%%%%                     %%%%%                      % ");
        System.out.println("    %                       %     %                     % ");
        System.out.println("    %    %%%  %%%    %%%%%  %     %                 %   % ");
        System.out.println("    %   %        %      %   %     %    %%%   %   %      % ");
        System.out.println("    %   %     %%%%     %    %%%%%%%   %   %  %   %  %   % ");
        System.out.println("    %   %    %   %    %     %     %   %   %  %   %  %     ");
        System.out.println("    %   %     %%% %  %%%%%  %     %    %%%%   %%%   %   % ");
        System.out.println("                                          %               ");
        System.out.println("                                          %               ");
    }
    
    
    
    
    
    
    
    public static void menu(){
        
        System.out.println("--------------------MENU PRINCIPAL--------------------");
        System.out.println("1-Login");
        System.out.println("2-Registar Utilizador");
        System.out.println("3-Gravar/Carregar");
        System.out.println("4-Administrador App");
        System.out.println("5-Rankings");
        System.out.println("0-Sair");
    }

    public static void submenu_administrador(){
        System.out.println("--------------------MENU ADMINISTRADOR--------------------");
        System.out.println("1-Consultar Encomendas Realizadas por Empresas");
        System.out.println("2-Consultar Encomendas Realizadas por Voluntarios");
        System.out.println("3-Consultar Encomendas");
        System.out.println("4-Consultar Lojas");
        System.out.println("5-Consultar Clientes");
        System.out.println("6-Consultar Empresas");
        System.out.println("7-Consultar Pedidos");
        System.out.println("8-Consultar Voluntarios");
        System.out.println("0-Sair");
    }

    public static void submenu_login(){
        System.out.println("--------------------MENU LOGIN--------------------");
        System.out.println("1-Login Cliente");
        System.out.println("2-Login Loja");
        System.out.println("3-Login Empresa");
        System.out.println("4-Login Voluntario");
        System.out.println("0-Sair");
    }

     public static void submenu_cliente(){
        System.out.println("--------------------MENU CLIENTE--------------------");
        System.out.println("1-Criar encomenda a uma loja");
        System.out.println("2-Responder a serviços de entrega propostos");
        System.out.println("3-Histórico de encomendas");
        System.out.println("4-Classificar serviços");
        System.out.println("0-Sair");
    }

    public static void submenu_Classificar(){
        System.out.println("--------------------MENU CLASSIFICAR--------------------");
        System.out.println("1-Classificar encomendas entregues por Voluntarios");
        System.out.println("2-Classificar encomendas entregues por Empresas");
        System.out.println("0-Sair");
    }
    
    public static void submenu_Empresa(){
        System.out.println("--------------------MENU EMPRESA--------------------");
        System.out.println("1-Sinalizar disposição para entregar encomendas");
        System.out.println("2-Histórico de encomendas entregues");
        System.out.println("3-Consultar faturado por um intervalo de tempo");
        System.out.println("0-Sair");
    }

    public static void submenu_Loja(){
        System.out.println("--------------------MENU LOJA--------------------");
        System.out.println("1-Sinalizar que existe uma encomenda de um cliente pronta para ser entregue");
        System.out.println("0-Sair");
    }

     public static void submenu_Voluntario(){
        System.out.println("--------------------MENU VOLUNTARIO--------------------");
        System.out.println("1-Entregar encomenda");
        System.out.println("2-Consultar historico de encomendas entregues por período");
        System.out.println("0-Sair");
    }
    
    public static void submenu_RegistarUtilizador(){
        System.out.println("--------------------MENU REGISTAR UTILIZADOR--------------------");
        System.out.println("1-Registar Cliente");
        System.out.println("2-Registar Loja");
        System.out.println("3-Registar Empresa");
        System.out.println("4-Registar Voluntario");
        System.out.println("0-Sair");
    }

    public static void submenu_HistoricoEncomendas(){
        System.out.println("--------------------MENU HISTORICO DE ENCOMENDAS--------------------");
        System.out.println("1-Consultar encomendas entregues por empresa por um período");
        System.out.println("2-Consultar encomenda entregues por voluntario por um período");
        System.out.println("0-Sair");
    }

    public static void submenu_gravar(){
        System.out.println("--------------------MENU GRAVAR/CARREGAR--------------------");
        System.out.println("1-Gravar estado");
        System.out.println("2-Carregar estado");
        System.out.println("3-Carregar Logs");
        System.out.println("0-Sair");
    }
    
    public static void submenu_rankings(){
        System.out.println("--------------------MENU RANKINGS--------------------");
        System.out.println("1-Top10 clientes por número de encomendas");
        System.out.println("2-Top10 empresas por km");
        System.out.println("0-Sair");
    }
}
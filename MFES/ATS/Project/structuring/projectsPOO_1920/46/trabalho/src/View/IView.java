package View;

import Controler.IControler;
import javafx.scene.Scene;
import Model.*;
import javafx.stage.Stage;

import java.util.List;

/**
 * Interface da View
 * */
public interface IView {
    /**
     * Cria um alert
     * @param titulo Titulo da Janela "Alert"
     * @param mensagem Mensagem a imprimir no "Alert"
     * */
    void alert(String titulo, String mensagem);

    /**
     * Função que gera a Scene JavaFX representativa do Menu Principal
     * @return Scene repesentativa do Menu Principal
     * */
    Scene menu();

    /**
     * Função que gera a Scene JavaFX representativa do Registo de Utilizador
     * @return Scene repesentativa do Registo de Utilizador
     * */
    Scene registar_user();

    /**
     * Função que gera a Scene JavaFX representativa do Registo de Transportadora
     * @return Scene repesentativa do Registo de Transportadora
     * */
    Scene registar_transportadora();

    /**
     * Função que gera a Scene JavaFX representativa do Registo de Voluntário
     * @return Scene repesentativa do Registo de Voluntário
     * */
    Scene registar_voluntario();

    /**
     * Função que gera a Scene JavaFX representativa do Registo de Loja
     * @return Scene repesentativa do Registo de Loja
     * */
    Scene registar_loja();

    /**
     * Função que gera a Scene JavaFX representativa do Login de User
     * @return Scene repesentativa do Login de User
     * */
    Scene login_user();

    /**
     * Função que gera a Scene JavaFX representativa do Login de Voluntário
     * @return Scene repesentativa do Login de Voluntário
     * */
    Scene login_voluntario();

    /**
     * Função que gera a Scene JavaFX representativa do Login de Transportadora
     * @return Scene repesentativa do Login de Transportadora
     * */
    Scene login_transportadora();

    /**
     * Função que gera a Scene JavaFX representativa do Login de Loja
     * @return Scene repesentativa do Login de Loja
     * */
    Scene login_loja();

    /**
     * Função que gera a Scene JavaFX representativa do Menu de Utilizador
     * @param u Utilizador
     * @param lojas Lista de Lojas
     * @param historico Histórico do Utilizador
     * @param encomenda Encomendas que precisam de ser processadas
     * @return Scene repesentativa do Menu de Utilizador
     * */
    Scene menu_user(IUtilizador u, List<String> lojas, List<String> historico, IEncomenda encomenda);

    /**
     * Função que gera a Scene JavaFX representativa do Menu de Transportadora
     * @param t Transportadora
     * @param lojas Lista de Lojas
     * @param faturacao Historico de faturação
     * @param fat Faturação Total
     * @return Scene repesentativa do Menu de Transportadora
     * */
    Scene menu_transportadora(ITransportadora t, List<String> lojas, List<String> faturacao , Double fat);

    /**
     * Função que gera a Scene JavaFX representativa do Menu de Voluntário
     * @param v Voluntário
     * @param lojas Lista de Lojas
     * @param historico Histórico de Voluntário
     * @return Scene repesentativa do Menu de Voluntário
     * */
    Scene menu_voluntario(IVoluntario v, List<String> lojas, List<String> historico);

    /**
     * Função que gera a Scene JavaFX representativa do Menu de Loja
     * @param l Loja
     * @param encomendas  encomendas em loja
     * @return Scene repesentativa do Menu de Loja
     * */
    Scene menu_loja(ILoja l, List<String> encomendas);

    /**
     * Função que gera a Scene JavaFX representativa da Seleção de Produtos por parte do Utilizador
     * @param u Uilizador
     * @param l Loja
     * @param produtos Produtos
     * @return Scene repesentativa da Seleção de Produtos por parte do Utilizador
     * */
    Scene select_produtos(IUtilizador u, ILoja l, List<String> produtos);

    /**
     * Função que cria uma nova Janela
     * @param title Titulo da nova Janela
     * @param s Scene do JavaFX
     * */
    void make_window(String title, Scene s);

    /**
     * Função que gera a Scene JavaFX representativa da Lista de Encomendas Ativas a ler pela Transportadora
     * @param t Transportadora
     * @param recolha Lista de encomendas para recolha
     * @return Scene repesentativa do Menu Principal
     * */
    Scene encomendas_ativas(ITransportadora t, List<String> recolha);

    /**
     * Função que gera a Scene JavaFX representativa da Lista de Encomendas Ativas a ler pelo Voluntário
     * @param v Voluntário
     * @param recolha Lista de encomendas para recolha
     * @return Scene repesentativa do Menu Principal
     * */
    Scene encomendas_ativas(IVoluntario v, List<String> recolha);

    /**
     * Função que gera a Scene JavaFX representativa da Lista de Encomendas Ativas
     * @param encomendas lista
     * @return Scene repesentativa do Menu Principal
     * */
    Scene print_list(List<String> encomendas);

    int rating(String s, String s1);
}
